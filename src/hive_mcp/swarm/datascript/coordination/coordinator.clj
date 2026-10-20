(ns hive-mcp.swarm.datascript.coordination.coordinator
  "Coordinator instance registry (register, heartbeat, stale marking).

   Compat shim: moved to hive-datascript.swarm.coordination.coordinator in
   hive-datascript. Every public var delegates there; see
   hive-mcp.swarm.delegate for behavior without the library."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript.schema :as s]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.coordination.coordinator" sym))

(defn get-all-coordinators {:arglists '([])} [& args] (apply (impl 'get-all-coordinators) args))

;; Contracts transcribe hive-datascript.swarm.coordination.coordinator, where
;; the behaviour lives.
(m/=> get-all-coordinators [:=> [:cat] [:sequential s/Coordinator]])
(defn get-coordinator {:arglists '([coordinator-id])} [& args] (apply (impl 'get-coordinator) args))

(m/=> get-coordinator [:=> [:cat s/EntityId] [:maybe s/Coordinator]])
(defn get-coordinators-by-status {:arglists '([status])} [& args] (apply (impl 'get-coordinators-by-status) args))

;; The store asserts a known status (:pre).
(m/=> get-coordinators-by-status [:=> [:cat s/CoordinatorStatus] [:sequential s/Coordinator]])
(defn get-coordinators-for-project {:arglists '([project])} [& args] (apply (impl 'get-coordinators-for-project) args))

(m/=> get-coordinators-for-project [:=> [:cat [:maybe :string]] [:sequential s/Coordinator]])
(defn mark-coordinator-stale! {:arglists '([coordinator-id])} [& args] (apply (impl 'mark-coordinator-stale!) args))

(m/=> mark-coordinator-stale! [:=> [:cat s/EntityId] s/MaybeTxReport])
(defn mark-coordinator-terminated! {:arglists '([coordinator-id])} [& args] (apply (impl 'mark-coordinator-terminated!) args))

(m/=> mark-coordinator-terminated! [:=> [:cat s/EntityId] s/MaybeTxReport])
(defn register-coordinator! {:arglists '([coordinator-id {:keys [project pid session-id]}])} [& args] (apply (impl 'register-coordinator!) args))

;; DataScript refuses to store nil, so :project must be given (a string).
;; BUG: the docstring calls :project optional, but omitting it throws
;; "Cannot store nil as a value" (hive-datascript coordinator.clj:30).
(m/=> register-coordinator!
      [:=> [:cat :string [:map [:project :string]
                          [:pid {:optional true} [:maybe :int]]
                          [:session-id {:optional true} [:maybe :string]]]]
       s/TxReport])
(defn remove-coordinator! {:arglists '([coordinator-id])} [& args] (apply (impl 'remove-coordinator!) args))

(m/=> remove-coordinator! [:=> [:cat s/EntityId] s/MaybeTxReport])
(defn update-heartbeat! {:arglists '([coordinator-id])} [& args] (apply (impl 'update-heartbeat!) args))

(m/=> update-heartbeat! [:=> [:cat s/EntityId] s/MaybeTxReport])
