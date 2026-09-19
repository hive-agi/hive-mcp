(ns hive-mcp.swarm.datascript.queries
  "Read-only query operations for swarm state.

   Compat shim: moved to hive-agent.swarm.datascript.queries in hive-agent.
   Every public var delegates there; see hive-mcp.swarm.delegate for behavior
   without the addon."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.datascript.queries" sym))

(defn check-file-conflicts {:arglists '([requesting-slave files])} [& args] (apply (impl 'check-file-conflicts) args))
(defn db-stats {:arglists '([])} [& args] (apply (impl 'db-stats) args))
(defn dump-db {:arglists '([])} [& args] (apply (impl 'dump-db) args))
(defn get-all-claims {:arglists '([])} [& args] (apply (impl 'get-all-claims) args))
(defn get-all-slaves {:arglists '([& {:keys [include-stale?], :or {include-stale? false}}])} [& args] (apply (impl 'get-all-slaves) args))
(defn get-child-project-ids {:arglists '([parent-project-id])} [& args] (apply (impl 'get-child-project-ids) args))
(defn get-claims-for-file {:arglists '([file-path])} [& args] (apply (impl 'get-claims-for-file) args))
(defn get-completed-tasks {:arglists '([& {:keys [slave-id since limit], :or {limit 100}}])} [& args] (apply (impl 'get-completed-tasks) args))
(defn get-recent-claim-history {:arglists '([& {:keys [file slave-id since limit], :or {limit 50}}])} [& args] (apply (impl 'get-recent-claim-history) args))
(defn get-slave {:arglists '([slave-id])} [& args] (apply (impl 'get-slave) args))
(defn get-slave-by-kanban-task {:arglists '([kanban-task-id])} [& args] (apply (impl 'get-slave-by-kanban-task) args))
(defn get-slave-by-name {:arglists '([name])} [& args] (apply (impl 'get-slave-by-name) args))
(defn get-slave-by-name-or-id {:arglists '([identifier])} [& args] (apply (impl 'get-slave-by-name-or-id) args))
(defn get-slave-ids-by-project {:arglists '([project-id])} [& args] (apply (impl 'get-slave-ids-by-project) args))
(defn get-slaves-by-project {:arglists '([project-id & {:keys [include-stale?], :or {include-stale? false}}])} [& args] (apply (impl 'get-slaves-by-project) args))
(defn get-slaves-by-status {:arglists '([status])} [& args] (apply (impl 'get-slaves-by-status) args))
(defn get-task {:arglists '([task-id])} [& args] (apply (impl 'get-task) args))
(defn get-tasks-for-slave {:arglists '([slave-id & [status]])} [& args] (apply (impl 'get-tasks-for-slave) args))
(defn has-conflict? {:arglists '([file-path requesting-slave])} [& args] (apply (impl 'has-conflict?) args))
