(ns hive-mcp.swarm.datascript.coordination.session-registry
  "Session-scoped registry of what a run produced, for wrap to harvest.

   Compat shim: moved to hive-datascript.swarm.coordination.session-registry
   in hive-datascript. Every public var delegates there; see
   hive-mcp.swarm.delegate for behavior without the library. Row ownership
   (hive-mcp.session.identity) is installed as the store's :scope-rows hook by
   hive-mcp.swarm.adapters.store-hooks."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript.schema :as s]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.coordination.session-registry" sym))

(defn- impl!
  "impl, after the host adapters (and so the :scope-rows hook) are installed."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

(defn clear-completed-tasks! {:arglists '([task-ids] [])} [& args] (apply (impl! 'clear-completed-tasks!) args))

;; Contracts transcribe hive-datascript.swarm.coordination.session-registry,
;; where the behaviour lives.
(m/=> clear-completed-tasks! [:function
                              [:=> [:cat [:maybe [:sequential [:maybe :string]]]] s/Count]
                              [:=> [:cat] s/Count]])
(defn clear-kanban-movements! {:arglists '([movement-ids] [])} [& args] (apply (impl! 'clear-kanban-movements!) args))

(m/=> clear-kanban-movements! [:function
                               [:=> [:cat [:maybe [:sequential [:maybe :string]]]] s/Count]
                               [:=> [:cat] s/Count]])
(defn get-completed-task {:arglists '([task-id])} [& args] (apply (impl! 'get-completed-task) args))

(m/=> get-completed-task [:=> [:cat s/EntityId] [:maybe s/CompletedTask]])
(defn get-completed-tasks-this-session {:arglists '([& {:keys [agent-id project-id session-ref parent-of]}])} [& args] (apply (impl! 'get-completed-tasks-this-session) args))

(m/=> get-completed-tasks-this-session [:=> [:cat s/SessionArgs] [:sequential s/CompletedTask]])
(defn get-kanban-movements-this-session {:arglists '([& {:keys [agent-id project-id session-ref parent-of]}])} [& args] (apply (impl! 'get-kanban-movements-this-session) args))

(m/=> get-kanban-movements-this-session [:=> [:cat s/SessionArgs] [:sequential s/KanbanMovement]])
(defn register-completed-task! {:arglists '([task-id {:keys [title agent-id project-id session-id]}])} [& args] (apply (impl! 'register-completed-task!) args))

(m/=> register-completed-task!
      [:=> [:cat :string [:maybe [:map [:title {:optional true} [:maybe :string]]
                                  [:agent-id {:optional true} [:maybe :string]]
                                  [:project-id {:optional true} [:maybe :string]]
                                  [:session-id {:optional true} [:maybe :string]]]]]
       s/TxReport])
(defn register-kanban-movement! {:arglists '([{:keys [task-id title from to agent-id project-id session-id]}])} [& args] (apply (impl! 'register-kanban-movement!) args))

(m/=> register-kanban-movement!
      [:=> [:cat [:map [:task-id :string] [:to :string]
                  [:title {:optional true} [:maybe :string]]
                  [:from {:optional true} [:maybe :string]]
                  [:agent-id {:optional true} [:maybe :string]]
                  [:project-id {:optional true} [:maybe :string]]
                  [:session-id {:optional true} [:maybe :string]]]]
       s/TxReport])
