(ns hive-mcp.swarm.datascript.lings
  "Entity lifecycle operations for lings (slaves, tasks, claims).

   Compat shim: moved to hive-agent.swarm.datascript.lings in hive-agent.
   Every public var delegates there; see hive-mcp.swarm.delegate for behavior
   without the addon. `with-critical-op` expands to this namespace's
   enter/exit fns, which delegate."
  (:require [hive-mcp.swarm.delegate :as delegate]))
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
(defn claim-file! {:arglists '([file-path slave-id & [{:keys [task-id prior-hash qn mode]}]])} [& args] (apply (impl! 'claim-file!) args))
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
