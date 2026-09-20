;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.disc
  "Staleness port: what the KERNEL is allowed to know about disc knowledge.

   HIVE-KERNEL steps K1/E6c. Two kernel namespaces still reach into
   `hive-mcp.knowledge-graph.disc`, a hive-memory extraction target:

   - `swarm.coordinator` propagates staleness when a claimed file changed
     during its claim (`propagate-staleness!` with the `:hash-mismatch`
     base value);
   - `tools.catchup.spawn` asks for the stale-file list it puts in a
     spawn context (`top-stale-files`).

   Both are KG domain work, so they go through a port rather than a move:
   the disc graph is exactly the kind of knowledge the hive-memory addon
   will own. (File CONTENT HASHING is the other half of that coupling and is
   NOT here: hashing a file is kernel work misfiled under the KG prefix, and
   the lever for that is a move, not a port. See memory 20260919135625-54c352a0.)

   Host-local, like the K1 ports and `hive-mcp.spi.metrics`: hive-spi's
   released `swarm.ports.memory-scope` carries IDiscStaleness with three
   OTHER methods (staleness-warnings, format-staleness-warnings,
   kg-first-context) and cannot grow these without a release, which this repo's
   CI cannot resolve (memory 20260919203856-2bb9451d).

   With nothing installed the port late-binds BY SYMBOL to the in-core
   namespace through `hive-mcp.swarm.adapters.soft`, so behaviour today is
   unchanged and the day the namespace leaves the same call answers the Noop:
   no staleness value, no propagation, no stale files."
  (:require [hive-mcp.swarm.adapters.soft :as soft]))

(defprotocol IDiscKnowledge
  "The disc-graph questions the kernel asks."
  (-staleness-value [this reason]
    "The base staleness weight for REASON (:hash-mismatch, :git-commit,
     :time-decay), or nil when no disc knowledge is present.")
  (-propagate-staleness! [this disc-path base-staleness reason]
    "Mark entries grounded in DISC-PATH as stale. Returns the host's
     {:propagated :skipped :errors :grounded} tally, or nil.")
  (-top-stale-files [this opts]
    "The N stalest files for OPTS {:n :project-id :threshold}, freshest
     information first. Returns a vector, empty when nothing is known."))

(def noop
  "The answer when no disc knowledge is present. Honest about absence: no
   weight, no propagation, and an EMPTY stale-file list rather than a
   pretend one."
  (reify IDiscKnowledge
    (-staleness-value [_ _] nil)
    (-propagate-staleness! [_ _ _ _] nil)
    (-top-stale-files [_ _] [])))

;; ---------------------------------------------------------------------------
;; Late binding to the in-core disc namespace, by symbol
;; ---------------------------------------------------------------------------

(def ^:private host-syms
  {::staleness-values  'hive-mcp.knowledge-graph.disc/base-staleness-values
   ::propagate         'hive-mcp.knowledge-graph.disc/propagate-staleness!
   ::top-stale-files   'hive-mcp.knowledge-graph.disc/top-stale-files})

(defonce ^:private cache
  ^{:doc "{port-key -> resolved | ::absent}. Cleared by install!/uninstall!/reset-cache!."}
  (atom {}))

(defn- host-var
  "What PORT-KEY names in the host, or nil when its namespace is absent.
   Both outcomes are cached: `top-stale-files` sits on the catchup path."
  [port-key]
  (let [hit (get @cache port-key)]
    (cond
      (= ::absent hit) nil
      (some? hit)      hit
      :else            (let [v (soft/resolve-soft (host-syms port-key))]
                         (swap! cache assoc port-key (or v ::absent))
                         v))))

(def ^:private host-adapter
  "Delegates to `hive-mcp.knowledge-graph.disc` while that namespace is still
   in core, and to `noop` once it is not."
  (reify IDiscKnowledge
    (-staleness-value [_ reason]
      (when-let [v (host-var ::staleness-values)]
        ;; base-staleness-values is DATA, so the resolved var derefs to the map
        (get (if (var? v) @v v) reason)))
    (-propagate-staleness! [_ disc-path base-staleness reason]
      (if-let [f (host-var ::propagate)]
        (f disc-path base-staleness reason)
        nil))
    (-top-stale-files [_ {:keys [n project-id threshold]}]
      (if-let [f (host-var ::top-stale-files)]
        ;; the host fn takes kwargs; only pass the keys the caller set, so the
        ;; host keeps owning every default
        (apply f (cond-> []
                   n         (conj :n n)
                   project-id (conj :project-id project-id)
                   threshold (conj :threshold threshold)))
        []))))

;; ---------------------------------------------------------------------------
;; Registry
;; ---------------------------------------------------------------------------

(defonce ^:private installed (atom nil))

(defn reset-cache!
  "Forget which host vars resolved. Returns nil."
  []
  (reset! cache {})
  nil)

(defn install!
  "Install IMPL as the disc-knowledge implementation. Returns IMPL."
  [impl]
  (reset! installed impl)
  (reset-cache!)
  impl)

(defn uninstall!
  "Drop the installed implementation; calls fall back to the late-bound host
   namespace, then to `noop`. Returns nil."
  []
  (reset! installed nil)
  (reset-cache!)
  nil)

(defn current
  "The implementation calls are routed to right now."
  []
  (or @installed host-adapter))

;; ---------------------------------------------------------------------------
;; What the kernel calls
;; ---------------------------------------------------------------------------

(defn staleness-value
  "Base staleness weight for REASON, or nil."
  [reason]
  (-staleness-value (current) reason))

(defn propagate-staleness!
  "Propagate staleness from DISC-PATH at weight BASE-STALENESS for REASON."
  [disc-path base-staleness reason]
  (-propagate-staleness! (current) disc-path base-staleness reason))

(defn top-stale-files
  "The stalest files for OPTS {:n :project-id :threshold}; [] when unknown."
  [opts]
  (-top-stale-files (current) opts))
