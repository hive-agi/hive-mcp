(ns hive-mcp.launch.schema
  "The malli schema for the `:launch` section of ~/.config/hive-mcp/config.edn.

   LAUNCH-CONFIG LC0: DATA ONLY. Nothing reads this section yet; LC1's planner
   turns it into a LaunchPlan, LC2 turns `:deps` into the -Sdeps overlay, and
   LC3 makes bin/hive-mcp go through both. The `.cljc` extension is for that
   downstream work: `hive-mcp.launch.knobs` (this addon's env-knob table) is
   loaded under babashka by the `bb test:launch` task.

   EVERY KEY IS OPTIONAL. A person's config.edn carries only what they differ
   on, so an absent key means the default in `hive-mcp.launch.defaults`.
   Validating `{}` passes; validating `defaults` passes.

   `LaunchConfig` is deliberately OPEN (malli's default for :map): the same
   config.edn holds `:addons`, `:memory`, `:services` and the rest, and a
   future `:launch` key must not fail an older hive-mcp's validation."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Leaves
;; =============================================================================

(def SizeString
  "A JVM heap size as the launcher writes it: a number and a unit suffix.

   `10g`, `512m`, `2048k`. bin/hive-mcp pastes the raw string into
   `-J-Xmx<value>` and lets the JVM parse it, so this is a STRING schema, not
   a number: coercing to bytes here would lose the caller's spelling and
   change the -J flags the script produces (LC3 compares argv byte for byte).
   The value is validated for shape only: a bare byte count (1073741824) is
   as valid as 10g, because -Xmx accepts both."
  [:and
   :string
   [:fn {:error/message "a size like 10g, 512m, 2048k"}
    (fn size-string? [s]
      (let [s (str/trim s)
            n (count s)
            digit? (fn [c] (str/includes? "0123456789" (str c)))]
        (cond
          (zero? n) false
          (every? digit? s) true
          :else (and (< 1 n)
                     (contains? #{"k" "m" "g" "t"}
                                (str/lower-case (subs s (dec n))))
                     (every? digit? (subs s 0 (dec n)))))))]])

(def PathDescriptor
  "A path DEFAULT stated as data, never as a literal home directory.

   bin/hive-mcp resolves these at run time from the environment:
   ${XDG_STATE_HOME:-$HOME/.local/state}/hive-mcp/heapdumps, and
   ${XDG_DATA_HOME:-$HOME/.local/share}/hive-mcp/restart-PORT.lock. Writing
   either as a string would bake one host's home into a shareable launcher, so
   `hive-mcp.launch.defaults` carries a descriptor and LC1 resolves it.

   :xdg names the base-directory variable (:state -> XDG_STATE_HOME,
   :data -> XDG_DATA_HOME); :fallback is the path under HOME when that
   variable is unset; :suffix is appended; :port marks the segment LC1 fills
   from the effective nREPL port, followed by :suffix-after-port."
  [:map
   [:xdg [:enum :state :data :config :cache]]
   [:fallback {:optional true} [:vector :string]]
   [:suffix {:optional true} :string]
   [:port {:optional true} :boolean]
   [:suffix-after-port {:optional true} :string]])

(def PathValue
  "A resolved path (a literal string, the HIVE_MCP_HEAP_DUMP_DIR override) or
   the descriptor a default carries before LC1 resolves it."
  [:or :string PathDescriptor])

(def OomdPreference
  "`ManagedOOMPreference=` value for the systemd user scope.

   :avoid and :omit are systemd's own; :none is accepted by bin/oom-policy.sh
   as a valid preference; :off is the launcher's run-in-the-caller's-cgroup
   switch (the script tests `!= off` before asking for a scope)."
  [:enum :avoid :omit :none :off])

;; =============================================================================
;; Sections
;; =============================================================================

(def JvmConfig
  "Heap and native-allocation flags. Each value becomes one -J flag, in the
   order bin/hive-mcp writes them today."
  [:map
   [:xmx {:optional true} SizeString]
   [:xms {:optional true} SizeString]
   [:soft-max-heap {:optional true} SizeString]
   [:malloc-arena-max {:optional true} :int]
   [:extra-opts {:optional true} [:vector :string]]])

(def ForensicsConfig
  "Crash forensics: core dumps and where an OOM heap dump lands.

   :heap-dump-dir may be a literal path (the HIVE_MCP_HEAP_DUMP_DIR override)
   or, when unset, defaults to the XDG-resolved descriptor in
   `hive-mcp.launch.defaults`."
  [:map
   [:coredump? {:optional true} :boolean]
   [:heap-dump-dir {:optional true} PathValue]])

(def OomConfig
  "OOM standing: the kernel's oom_score_adj for this process tree, and the
   systemd-oomd preference for the scope it runs in."
  [:map
   [:score-adj {:optional true} :int]
   [:oomd-preference {:optional true} OomdPreference]])

(def RestartLockConfig
  "The advisory flock that lets exactly one launcher own a port.

   :by is who claims the lock (HIVE_MCP_RESTART_BY), recorded in the holder
   file and restarts.log so a refused second launch can say who holds it."
  [:map
   [:enabled? {:optional true} :boolean]
   [:file {:optional true} PathValue]
   [:by {:optional true} [:maybe :string]]])

(def SuperviseConfig
  "Restart-on-OOM: how many OOM deaths inside :window-s before giving up."
  [:map
   [:enabled? {:optional true} :boolean]
   [:max-restarts {:optional true} :int]
   [:window-s {:optional true} :int]])

;; =============================================================================
;; The section
;; =============================================================================

(def LaunchConfig
  "The `:launch` section. Every key optional; absent means the default.

   :deps is deps.edn shaped — a map of lib symbol to coordinate map — so a
   value copied out of local.deps.edn is already valid (LC2's importer relies
   on that). :aliases is likewise a deps.edn alias map, left unvalidated
   beyond `is a map`: it holds :main-opts, :jvm-opts, :extra-deps and whatever
   else tools.deps accepts."
  [:map
   [:jvm {:optional true} JvmConfig]
   [:profile {:optional true} [:maybe :keyword]]
   [:deps {:optional true} [:map-of :symbol :map]]
   [:workspace {:optional true} [:maybe :string]]
   [:presets {:optional true} [:vector :keyword]]
   [:extra-paths {:optional true} [:vector :string]]
   [:aliases {:optional true} :map]
   [:fast-boot? {:optional true} :boolean]
   [:forensics {:optional true} ForensicsConfig]
   [:oom {:optional true} OomConfig]
   [:restart-lock {:optional true} RestartLockConfig]
   [:supervise {:optional true} SuperviseConfig]
   ;; Not in the plan's key list, but bin/hive-mcp reads HIVE_MCP_NREPL_PORT
   ;; (it names the restart-lock file) and the knob table must land somewhere.
   [:nrepl-port {:optional true} [:maybe :int]]])

;; =============================================================================
;; Queries used by the tests and by LC1's planner
;; =============================================================================

(defn- map-children
  "The `[key props child-schema]` entries of the `:map` schema `s`, or nil
   when `s` is not a :map.

   malli 0.20.2 exposes no public path lookup (`m/get` is absent from the
   public API), so callers walk children instead of asking for a path. Each
   entry is [key props child]; props carries `{:optional true}`."
  [s]
  (let [s (m/schema s)]
    (when (= :map (m/type s))
      (m/children s))))

(defn key-present?
  "True when `k` is a declared key of the `:map` schema `s` at its top level."
  [s k]
  (boolean (some (fn [entry] (= k (first entry))) (map-children s))))

(defn child-schema
  "The child schema declared for `k` under `s`, or nil when absent."
  [s k]
  (some (fn [entry] (when (= k (first entry)) (last entry))) (map-children s)))

(defn optional?
  "True when `k`'s entry under `s` carries {:optional true}."
  [s k]
  (boolean
   (some (fn [entry]
           (when (= k (first entry))
             (true? (:optional (second entry)))))
         (map-children s))))

(defn path-present?
  "True when the keyword PATH exists in `schema`, walking one :map level per
   step. [:jvm :xmx] is true; [:jvm :nope] and [:nope :xmx] are not."
  [schema path]
  (loop [s schema, [k & more] (seq path)]
    (cond
      (nil? k) true
      (nil? s) false
      (not (key-present? s k)) false
      (empty? more) true
      :else (recur (child-schema s k) more))))

(defn explain
  "A malli explanation of why `launch` is not a LaunchConfig, or nil when it
   is one. Errors are humanized by the caller if it wants prose."
  [launch]
  (when-not (m/validate LaunchConfig launch)
    (m/explain LaunchConfig launch)))

;; =============================================================================
;; Contracts (malli 0.20.2 is a babashka dependency too, so these are shared)
;; =============================================================================

(m/=> key-present? [:=> [:cat :any :any] :boolean])
(m/=> child-schema [:=> [:cat :any :any] :any])
(m/=> optional? [:=> [:cat :any :any] :boolean])
(m/=> path-present? [:=> [:cat :any [:sequential :keyword]] :boolean])
(m/=> explain [:=> [:cat :any] [:maybe [:map]]])

(comment
  ;; An absent section is a valid one: everything is optional.
  (m/validate LaunchConfig {}) ;=> true

  ;; A person who only wants a bigger heap:
  (m/validate LaunchConfig {:jvm {:xmx "24g"}}) ;=> true

  ;; deps.edn shape passes through untouched (LC2 depends on this)
  (m/validate LaunchConfig
              {:deps {'io.github.hive-agi/hive-carto {:local/root "../hive-carto"}}})
  ;=> true

  ;; A typo does not: :xmx is a string, and "10" has no unit
  (m/validate LaunchConfig {:jvm {:xmx 10}}) ;=> false
  (m/validate LaunchConfig {:jvm {:xmx "10"}}) ;=> false
  (path-present? LaunchConfig [:jvm :xmx]) ;=> true
  (path-present? LaunchConfig [:jvm :nope]) ;=> false
  )