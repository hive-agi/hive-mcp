(ns hive-mcp.server.core
  "MCP server entry point — Integrant lifecycle orchestrator.

   Replaces the monolithic 7-phase start! with declarative Integrant system.
   All initialization order is determined by #ig/ref dependency edges in
   resources/hive/system.edn + profile overlays.

   Public API:
     start!  — Init Integrant system from config (non-blocking, returns system map)
     stop!   — Halt running system (ig/halt! in reverse init order)
     reset!  — stop! + clj-reload namespace refresh + start! (dev workflow)
     -main   — Entry point: start! + block on keepalive"
  (:refer-clojure :exclude [reset!])
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async]
            [integrant.core :as ig]
            [meta-merge.core :refer [meta-merge]]
            [taoensso.timbre :as log]
            ;; ── System layer namespaces (load init-key/halt-key! multimethods) ──
            [hive-mcp.system.layer1]
            [hive-mcp.system.layer2]
            [hive-mcp.system.layer3]
            [hive-mcp.system.layer4]
            [hive-mcp.system.layer5]
            [hive-mcp.system.addon-hot]
            [hive-mcp.system.keepalive :as keepalive]
            ;; Engine resilience — defense-in-depth L0.3 boot-time hprof control
            [hive-mcp.engine.hprof.boot :as hprof]
            ;; Engine resilience — defense-in-depth L0.5 bounded manifold pool
            [hive-mcp.engine.manifold-pool :as manifold-pool]
            [hive-mcp.config.io :as config-io]
            [hive-mcp.server.log-sink :as log-sink]
            [malli.core :as m])
  (:gen-class))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Configure Timbre to write to stderr instead of stdout
;; This is CRITICAL for MCP servers — stdout is the JSON-RPC channel
;; =============================================================================

(defonce console-sink
  (log-sink/make-sink (log-sink/err-stream-writer)))

(defonce file-sink
  (log-sink/make-sink (log-sink/file-line-writer)))

(defn register-coordinator-file!
  "Install the boot-only file appender over an injected asynchronous sink."
  [sink]
  (log/merge-config!
   {:appenders {:coordinator-file {:enabled? true
                                   :async? false
                                   :fn (log-sink/boot-appender-fn sink)}}}))

(m/=> register-coordinator-file! [:=> [:cat log-sink/Sink] :map])

(log/merge-config!
 {:appenders
  {:println {:enabled? true
             :async? false
             :fn (log-sink/appender-fn console-sink)}}})

(register-coordinator-file! file-sink)
;; =============================================================================
;; System State — single defonce atom replaces 6 private atoms
;; =============================================================================

(defonce system (atom nil))

;; Backward-compat bridge for bb-mcp forwarding handlers.
;; bb-mcp resolves this atom to get middleware-wrapped tool handlers.
;; Populated by populate-server-context! (init.clj) after Integrant (go).
(defonce server-context-atom (atom nil))

;; =============================================================================
;; Profile-Aware Config Loading
;; =============================================================================

(defn profile-from
  "The profile keyword for a CLI --profile value, a HIVE_PROFILE value and the
   config.edn [:system :profile] value, any of which may be nil. Precedence:
   CLI > env > config.edn > :desktop. Pure, so the precedence can be tested
   under any ambient environment."
  ([cli-profile env-profile] (profile-from cli-profile env-profile nil))
  ([cli-profile env-profile config-profile]
   (keyword (or cli-profile env-profile config-profile "desktop"))))

(def ProfileName
  "A profile as the CLI, the env or config.edn spell it: a string or a
   keyword, nil when that source names none."
  [:maybe [:or [:string {:min 1}] :keyword]])

(m/=> profile-from [:function
                    [:=> [:cat ProfileName ProfileName] :keyword]
                    [:=> [:cat ProfileName ProfileName ProfileName] :keyword]])

(defn config-system
  "The :system section of config.edn: {:profile kw, :overrides {ig-key map-or-nil}}.
   {} when the file or the section is absent or unreadable."
  []
  (or (some-> (config-io/read-config-file config-io/config-path) :ok :system) {}))

(m/=> config-system [:=> [:cat] [:map [:profile {:optional true} :any]
                                 [:overrides {:optional true} [:maybe :map]]]])

(defn resolve-profile
  "Resolve the profile keyword from the CLI --profile arg, HIVE_PROFILE env,
   config.edn [:system :profile], or :desktop, in that order."
  ([] (resolve-profile nil))
  ([cli-profile]
   (profile-from cli-profile (System/getenv "HIVE_PROFILE") (:profile (config-system)))))

(m/=> resolve-profile [:function [:=> [:cat] :keyword] [:=> [:cat ProfileName] :keyword]])

(defn read-base-config
  "Read and parse the base system.edn config with Integrant readers."
  []
  (if-let [r (io/resource "hive/system.edn")]
    (ig/read-string (slurp r))
    (throw (ex-info "Base system.edn not found on classpath"
                    {:resource "hive/system.edn"
                     :hint "Ensure resources/ is on :paths"}))))

(def SystemConfig
  "An Integrant config: component key -> its config."
  [:map-of :keyword :any])

(m/=> read-base-config [:=> [:cat] SystemConfig])

(defn read-profile-config
  "Read profile overlay EDN. Returns {} if profile file not found."
  [profile]
  (let [path (str "hive/profiles/" (name profile) ".edn")]
    (if-let [r (io/resource path)]
      (ig/read-string (slurp r))
      (do (log/warn "Profile" path "not found, using base config only")
          {}))))

(m/=> read-profile-config [:=> [:cat [:or :keyword :string]] SystemConfig])

(defn apply-overlay
  "BASE with OVERLAY meta-merged over it. A key OVERLAY sets to nil is removed
   (Integrant exclusion); meta-merge alone would keep the base value."
  [base overlay]
  (let [nil-keys (into #{} (comp (filter (fn [[_ v]] (nil? v))) (map key)) overlay)
        overlay' (into {} (remove (fn [[_ v]] (nil? v))) overlay)]
    (apply dissoc (meta-merge base overlay') nil-keys)))

(m/=> apply-overlay [:=> [:cat [:map-of :any :any] [:map-of :any :any]] [:map-of :any :any]])

(defn load-system-config
  "Base system.edn, then the profile overlay, then config.edn's
   [:system :overrides], each applied with `apply-overlay`."
  ([] (load-system-config (resolve-profile)))
  ([profile] (load-system-config profile (:overrides (config-system))))
  ([profile overrides]
   (-> (read-base-config)
       (apply-overlay (read-profile-config profile))
       (apply-overlay (or overrides {})))))

(m/=> load-system-config [:function
                          [:=> [:cat] SystemConfig]
                          [:=> [:cat [:or :keyword :string]] SystemConfig]
                          [:=> [:cat [:or :keyword :string] [:maybe [:map-of :any :any]]] SystemConfig]])

;; =============================================================================
;; Boot timing — per-init-key duration instrumentation
;; =============================================================================

(defn- boot-timing?
  "Per-key boot timing is on by default; disable with HIVE_BOOT_TIMING=0."
  []
  (not= "0" (System/getenv "HIVE_BOOT_TIMING")))

(defn- timed-init
  "Drop-in for `(ig/init config)` that logs per-key init duration plus a sorted
   summary of the slowest keys. Replicates integrant 1.0.1 `init` internals
   exactly — `build` with the private `wrapped-assert-key` assertf and the
   `resolve-key` resolvef — and only wraps `init-key` with a nanoTime probe, so
   behaviour is identical to `ig/init`. Toggle off with HIVE_BOOT_TIMING=0.

   Greppable: every line is tagged `[boot-timing]`."
  [config]
  (if-not (boot-timing?)
    (ig/init config)
    (let [timings (atom [])
          ;; wrapped-assert-key is private — reuse it to keep spec-assertion
          ;; behaviour identical, falling back to a noop if it ever moves.
          assertf (or (some-> (resolve 'integrant.core/wrapped-assert-key) deref)
                      (fn [_ _ _]))
          t0      (System/nanoTime)
          sys     (ig/build
                   config (keys config)
                   (fn [k v]
                     (let [s  (System/nanoTime)
                           r  (ig/init-key k v)
                           ms (/ (- (System/nanoTime) s) 1e6)]
                       (swap! timings conj [k ms])
                       (log/info (log-sink/boot-event k :init ms :ok))
                       r))
                   assertf
                   ig/resolve-key)
          total   (/ (- (System/nanoTime) t0) 1e6)
          slowest (take 8 (sort-by (comp - second) @timings))]
      (log/info (log-sink/boot-event :system :complete total :ok))
      (doseq [[k ms] slowest]
        (log/info (log-sink/boot-event k :slowest ms :ok)))
      sys)))

;; =============================================================================
;; Lifecycle — start! / stop! / reset!
;; =============================================================================

(defn start!
  "Initialize the Integrant system from system.edn + profile overlay.

   Non-blocking — returns the initialized system map. Caller is responsible
   for blocking (see -main which uses keepalive/await-shutdown-or-stdio!).

   Idempotent guard: throws if system already running.

   Accepts optional profile keyword (:desktop, :k8s-headless, :k8s-minimal).
   Profile precedence: explicit :profile > HIVE_PROFILE env > :desktop.
   `:hprof-boot!` is a boot port for isolated lifecycle tests; production
   defaults to hprof/boot! and retains the disk-pressure abort."
  [& {:keys [profile hprof-boot!] :or {profile nil hprof-boot! hprof/boot!}}]
  (let [profile (resolve-profile profile)]
    (when @system
      (throw (ex-info "System already running. Call (stop!) first or (reset!) to restart."
                      {:profile profile})))
    (log/info "Starting hive-mcp server with Integrant, profile:" profile)
    (when-let [sock (System/getenv "EMACS_SOCKET_NAME")]
      (log/info "Targeting Emacs daemon:" sock))
    ;; ENGINE-L0.3 — rotate stale hprofs and abort early on disk pressure
    ;; before allocating any heap-heavy subsystems.
    (hprof-boot!)
    ;; ENGINE-L0.5 — cap manifold's wait-pool BEFORE any deferred runs
    ;; (the wait-pool fn is delay-cached, so this must precede ig/init).
    (manifold-pool/boot!)
    (let [config (load-system-config profile)
          sys    (timed-init config)]
      (clojure.core/reset! system sys)
      ;; Populate server-context-atom for bb-mcp forwarding handlers.
      ;; Must run AFTER extensions are loaded (tools registered).
      (try
        (require 'hive-mcp.server.init)
        (when-let [populate! (resolve 'hive-mcp.server.init/populate-server-context!)]
          (populate!))
        (catch Exception e
          (log/warn "populate-server-context! failed (non-fatal):" (ex-message e))))
      (log/info "Integrant system initialized:" (count sys) "keys")
      sys)))

(def StartOpts
  "start! / reset! keyword args: :profile, and the :hprof-boot! port."
  [:* [:alt [:cat [:= :profile] ProfileName]
       [:cat [:= :hprof-boot!] [:=> [:cat] :any]]]])

(m/=> start! [:=> [:cat StartOpts] SystemConfig])

(defn stop!
  "Halt the running Integrant system. Safe to call when no system running.
   Keys are halted in reverse init order (Integrant default)."
  []
  (when-let [sys @system]
    (log/info "Halting Integrant system...")
    (ig/halt! sys)
    (clojure.core/reset! system nil)
    (log/info "Integrant system halted.")
    :halted))

(m/=> stop! [:=> [:cat] [:maybe [:= :halted]]])

(defn reset!
  "Stop system, reload changed namespaces via clj-reload, re-init system.
   The full REPL-driven development cycle.

   clj-reload is dev-only (not in main deps), so we use requiring-resolve.
   Accepts the same optional hprof boot port as start! for isolated tests."
  [& {:keys [profile hprof-boot!] :or {profile nil hprof-boot! hprof/boot!}}]
  (let [profile (or profile (resolve-profile))]
    (stop!)
    (log/info "Reloading changed namespaces...")
    (if-let [reload-fn (requiring-resolve 'clj-reload.core/reload)]
      (do (reload-fn)
          (log/info "Namespaces reloaded. Restarting..."))
      (log/warn "clj-reload not available (dev-only dep). Skipping reload."))
    (start! :profile profile :hprof-boot! hprof-boot!)))

(m/=> reset! [:=> [:cat StartOpts] SystemConfig])

;; =============================================================================
;; Blocking — keepalive integration for -main
;; =============================================================================

(defn- block-until-shutdown!
  "Block the calling thread on the keepalive component until shutdown signal.

   In :stdio mode (desktop): races MCP stdio join promise vs shutdown-ch.
   In :promise mode (K8s headless): blocks on shutdown-ch (SIGTERM).

   After unblocking, halts the system and exits."
  [sys]
  (let [keepalive-state (get sys :hive/keepalive)]
    (if-let [mcp-stdio (get sys :hive/mcp-stdio)]
      ;; Desktop: race stdio EOF vs shutdown signal
      (let [[source _val] (keepalive/await-shutdown-or-stdio!
                            keepalive-state (:join mcp-stdio))]
        (log/info "hive-mcp unblocked via" source "— initiating clean shutdown")
        (stop!)
        (System/exit 0))
      ;; Headless: block on shutdown-ch (SIGTERM or halt!)
      (do
        (keepalive/await-shutdown! keepalive-state)
        (log/info "hive-mcp shutdown signal received — initiating clean shutdown")
        (stop!)
        (System/exit 0)))))

;; =============================================================================
;; CLI Entry Point
;; =============================================================================

(defn- parse-cli-args
  "Parse CLI args for --profile flag. Returns map with :profile (or nil)."
  [args]
  (loop [args args
         opts {}]
    (if (empty? args)
      opts
      (let [[flag val & rest] args]
        (if (= flag "--profile")
          (recur rest (assoc opts :profile val))
          (recur (next args) opts))))))

(defn -main
  "Entry point for the MCP server.

   Usage:
     java -jar hive-mcp.jar                          ; desktop (default)
     java -jar hive-mcp.jar --profile k8s-headless   ; headless K8s
     HIVE_PROFILE=k8s-minimal java -jar hive-mcp.jar ; env-based

   Profile precedence: --profile flag > HIVE_PROFILE env > desktop"
  [& args]
  (let [{:keys [profile]} (parse-cli-args args)
        sys (start! :profile profile)]
    (block-until-shutdown! sys)))

(comment
  ;; For REPL development — use dev/user.clj (go)/(halt)/(reset) instead
  (start!)
  (start! :profile :k8s-headless)
  (stop!)
  (reset!)
  @system)
