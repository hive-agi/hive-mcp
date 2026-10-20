(ns hive-mcp.swarm.datascript.queries
  "Read-only query operations for swarm state.

   Compat shim: moved to hive-agent.swarm.datascript.queries in hive-agent.
   Every public var delegates there; see hive-mcp.swarm.delegate for behavior
   without the addon."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript.schema :as s]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.queries" sym))

(defn check-file-conflicts {:arglists '([requesting-slave files])} [& args] (apply (impl 'check-file-conflicts) args))

;; Contracts transcribe hive-datascript.swarm.queries, where the behaviour
;; lives. Unknown ids answer nil or an empty result, never a refusal.
(m/=> check-file-conflicts
      [:=> [:cat s/EntityId [:maybe s/StringColl]]
       [:maybe [:sequential [:map [:file :string] [:held-by [:maybe :string]]]]]])
(defn db-stats {:arglists '([])} [& args] (apply (impl 'db-stats) args))

(m/=> db-stats [:=> [:cat]
                [:map [:slaves s/Count] [:tasks s/Count] [:claims s/Count]
                 [:claim-history s/Count] [:active-tasks s/Count] [:wrap-queue s/Count]
                 [:unprocessed-wraps s/Count] [:completed-tasks s/Count]]])
(defn dump-db {:arglists '([])} [& args] (apply (impl 'dump-db) args))

(m/=> dump-db [:=> [:cat]
               [:map [:slaves [:sequential s/Slave]] [:tasks [:sequential :map]]
                [:claims [:sequential s/ClaimRow]] [:stats :map]]])
(defn get-all-claims {:arglists '([])} [& args] (apply (impl 'get-all-claims) args))

(m/=> get-all-claims [:=> [:cat] [:sequential s/ClaimRow]])
(defn get-all-slaves {:arglists '([& {:keys [include-stale?], :or {include-stale? false}}])} [& args] (apply (impl 'get-all-slaves) args))

(m/=> get-all-slaves [:=> [:cat s/IncludeStaleArgs] [:sequential s/Slave]])
(defn get-child-project-ids {:arglists '([parent-project-id])} [& args] (apply (impl 'get-child-project-ids) args))

(m/=> get-child-project-ids [:=> [:cat [:maybe :string]] [:set :string]])
(defn get-claims-for-file {:arglists '([file-path])} [& args] (apply (impl 'get-claims-for-file) args))

(m/=> get-claims-for-file [:=> [:cat s/EntityId] [:maybe s/ClaimInfo]])
(defn get-completed-tasks {:arglists '([& {:keys [slave-id since limit], :or {limit 100}}])} [& args] (apply (impl 'get-completed-tasks) args))

(m/=> get-completed-tasks [:=> [:cat s/CompletedTaskArgs] [:sequential s/Task]])
(defn get-recent-claim-history {:arglists '([& {:keys [file slave-id since limit], :or {limit 50}}])} [& args] (apply (impl 'get-recent-claim-history) args))

(m/=> get-recent-claim-history [:=> [:cat s/ClaimHistoryArgs] [:sequential s/ClaimHistory]])
(defn get-slave {:arglists '([slave-id])} [& args] (apply (impl 'get-slave) args))

(m/=> get-slave [:=> [:cat s/EntityId] [:maybe s/Slave]])
(defn get-slave-by-kanban-task {:arglists '([kanban-task-id])} [& args] (apply (impl 'get-slave-by-kanban-task) args))

(m/=> get-slave-by-kanban-task [:=> [:cat s/EntityId] [:maybe s/Slave]])
(defn get-slave-by-name {:arglists '([name])} [& args] (apply (impl 'get-slave-by-name) args))

(m/=> get-slave-by-name [:=> [:cat s/EntityId] [:maybe s/Slave]])
(defn get-slave-by-name-or-id {:arglists '([identifier])} [& args] (apply (impl 'get-slave-by-name-or-id) args))

;; The body reads the identifier with str/starts-with?, so it must be a string.
(m/=> get-slave-by-name-or-id [:=> [:cat :string] [:maybe s/Slave]])
(defn get-slave-ids-by-project {:arglists '([project-id])} [& args] (apply (impl 'get-slave-ids-by-project) args))

(m/=> get-slave-ids-by-project [:=> [:cat [:maybe :string]] [:sequential :string]])
(defn get-slaves-by-project {:arglists '([project-id & {:keys [include-stale?], :or {include-stale? false}}])} [& args] (apply (impl 'get-slaves-by-project) args))

(m/=> get-slaves-by-project [:=> [:cat [:maybe :string] s/IncludeStaleArgs] [:sequential s/Slave]])
(defn get-slaves-by-status {:arglists '([status])} [& args] (apply (impl 'get-slaves-by-status) args))

(m/=> get-slaves-by-status [:=> [:cat [:maybe :keyword]] [:sequential s/Slave]])
(defn get-task {:arglists '([task-id])} [& args] (apply (impl 'get-task) args))

(m/=> get-task [:=> [:cat s/EntityId] [:maybe s/Task]])
(defn get-tasks-for-slave {:arglists '([slave-id & [status]])} [& args] (apply (impl 'get-tasks-for-slave) args))

;; nil when the slave is unknown, else its tasks (optionally by status).
(m/=> get-tasks-for-slave [:=> [:cat s/EntityId [:? [:maybe :keyword]]] [:maybe [:sequential s/Task]]])
(defn has-conflict? {:arglists '([file-path requesting-slave])} [& args] (apply (impl 'has-conflict?) args))

;; The body only looks the key up, so any value is read (an existing caller
;; passes a vector of paths and gets {:conflict? false}).
(m/=> has-conflict? [:=> [:cat :any :any] [:or [:map [:conflict? [:= false]]] [:map [:conflict? [:= true]] [:held-by [:maybe :string]]]]])
