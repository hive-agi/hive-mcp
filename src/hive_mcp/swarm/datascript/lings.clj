(ns hive-mcp.swarm.datascript.lings
  "Entity lifecycle operations for lings (slaves, tasks, claims).

   Compat shim: moved to hive-agent.swarm.datascript.lings in hive-agent.
   Every public var delegates there; see hive-mcp.swarm.delegate for behavior
   without the addon. `with-critical-op` expands to this namespace's
   enter/exit fns, which delegate."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.lings" sym))

(defn- impl!
  "impl, after the host adapters fill the swarm port slots (resolved late:
   a static require would close a load cycle)."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

(defn add-slave! {:arglists '([slave-id {:keys [name status depth parent presets cwd project-id kanban-task-id], :or {status :idle, depth 1}}])} [& args] (apply (impl! 'add-slave!) args))
(defn add-task! {:arglists '([task-id slave-id {:keys [status prompt files], :or {status :dispatched}}])} [& args] (apply (impl! 'add-task!) args))
(defn add-to-wait-queue! {:arglists '([ling-id file-path])} [& args] (apply (impl! 'add-to-wait-queue!) args))
(defn append-stdout! {:arglists '([slave-id lines])} [& args] (apply (impl! 'append-stdout!) args))
(defn archive-claim-to-history! {:arglists '([file-path {:keys [slave-id prior-hash released-hash lines-added lines-removed]}])} [& args] (apply (impl! 'archive-claim-to-history!) args))
(defn can-kill? {:arglists '([slave-id])} [& args] (apply (impl! 'can-kill?) args))
(defn claim-age-ms {:arglists '([file-path])} [& args] (apply (impl! 'claim-age-ms) args))
(def ^:private dead-statuses
  "Slave statuses whose claims no longer protect anything."
  #{:terminated :dead :killed :zombie})

(defn- live-foreign-holder
  "The id of the LIVE slave other than `slave-id` holding `file-path`, or nil.

   Live means registered, not in a dead status, and holding a claim that is not
   stale. A dead or stale holder is exactly the case the upsert exists for (a
   ling that died holding a file must not fence it off forever), so only a live
   one refuses."
  [file-path slave-id]
  (let [{holder :slave-id} ((impl! 'get-claim-info) file-path)]
    (when (and holder (not= holder slave-id))
      (let [slave ((delegate/resolve-var "hive-datascript.swarm.queries" 'get-slave)
                   holder)]
        (when (and slave
                   (not (contains? dead-statuses (:slave/status slave)))
                   (not ((impl! 'claim-stale?) file-path)))
          holder)))))

(defn- claim-lock
  "The one claim lock (the logic-db atom `coordinator/atomic-claim-files!` and
   `claim.registry/acquire!` hold), resolved late: a static require would
   close a load cycle. Reentrant, so taking it under either of those is free."
  []
  (if-let [f (try (requiring-resolve 'hive-mcp.swarm.logic/get-logic-db-atom)
                  (catch Throwable _ nil))]
    (f)
    ::no-logic-lock))

(defn claim-file!
  "Claim `file-path` (a claim key) for `slave-id`.

   The store's claim-file! UPSERTS on the unique key, so an unchecked call used
   to hand another live ling's claim to the caller without a word
   (SWARM-CLAIM-STEAL). This shim refuses that case under the claim lock and
   returns {:claimed? false :refused :held-by-live-slave :file :held-by}
   instead of writing. Re-claiming one's own key, or taking over a key held by
   a dead or stale slave, still upserts as before."
  {:arglists '([file-path slave-id & [{:keys [task-id prior-hash qn mode]}]])}
  [file-path slave-id & more]
  (locking (claim-lock)
    (if-let [holder (live-foreign-holder file-path slave-id)]
      (do (log/warn "claim refused: held by a live slave"
                    {:file file-path :held-by holder :requesting slave-id})
          {:claimed? false
           :refused  :held-by-live-slave
           :file     file-path
           :held-by  holder})
      (apply (impl! 'claim-file!) file-path slave-id more))))
(defn claim-stale? {:arglists '([file-path] [file-path threshold-ms])} [& args] (apply (impl! 'claim-stale?) args))
(defn cleanup-stale-claims! {:arglists '([] [threshold-ms])} [& args] (apply (impl! 'cleanup-stale-claims!) args))
(defn cleanup-stdout-buffer! {:arglists '([slave-id])} [& args] (apply (impl! 'cleanup-stdout-buffer!) args))
(defn complete-task! {:arglists '([task-id])} [& args] (apply (impl! 'complete-task!) args))
(def default-stale-threshold-ms @(impl 'default-stale-threshold-ms))
(defn enter-critical-op! {:arglists '([slave-id op-type])} [& args] (apply (impl! 'enter-critical-op!) args))
(defn exit-critical-op! {:arglists '([slave-id op-type])} [& args] (apply (impl! 'exit-critical-op!) args))
(defn fail-task! {:arglists '([task-id status])} [& args] (apply (impl! 'fail-task!) args))
(defn get-all-claims {:arglists '([& args])} [& args] (apply (impl! 'get-all-claims) args))
(defn get-claim-info {:arglists '([& args])} [& args] (apply (impl! 'get-claim-info) args))
(defn get-critical-ops {:arglists '([slave-id])} [& args] (apply (impl! 'get-critical-ops) args))
(defn get-stale-claims {:arglists '([] [threshold-ms])} [& args] (apply (impl! 'get-stale-claims) args))
(defn get-stdout {:arglists '([slave-id] [slave-id n])} [& args] (apply (impl! 'get-stdout) args))
(defn get-stdout-buffer-info {:arglists '([slave-id])} [& args] (apply (impl! 'get-stdout-buffer-info) args))
(defn get-stdout-since {:arglists '([slave-id since-idx])} [& args] (apply (impl! 'get-stdout-since) args))
(defn init-stdout-buffer! {:arglists '([slave-id])} [& args] (apply (impl! 'init-stdout-buffer!) args))
(defn refresh-claim! {:arglists '([file-path])} [& args] (apply (impl! 'refresh-claim!) args))
(defn release-claim! {:arglists '([file-path])} [& args] (apply (impl! 'release-claim!) args))
(defn release-claims-for-slave! {:arglists '([slave-id])} [& args] (apply (impl! 'release-claims-for-slave!) args))
(defn release-claims-for-task! {:arglists '([task-id])} [& args] (apply (impl! 'release-claims-for-task!) args))
(defn remove-slave! {:arglists '([slave-id])} [& args] (apply (impl! 'remove-slave!) args))
(defn reset-stdout-buffers! {:arglists '([])} [& args] (apply (impl! 'reset-stdout-buffers!) args))
(def stdout-buffer-max-lines @(impl 'stdout-buffer-max-lines))
(def stdout-buffers @(impl 'stdout-buffers))
(defn update-slave! {:arglists '([slave-id updates])} [& args] (apply (impl! 'update-slave!) args))
(defn update-task! {:arglists '([task-id updates])} [& args] (apply (impl! 'update-task!) args))

(defmacro with-critical-op
  "Execute body while holding a critical operation guard.
   Ensures the critical op is properly released even on exception."
  [slave-id op-type & body]
  `(do
     (enter-critical-op! ~slave-id ~op-type)
     (try
       ~@body
       (finally
         (exit-critical-op! ~slave-id ~op-type)))))
