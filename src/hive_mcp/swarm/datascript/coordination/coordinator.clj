(ns hive-mcp.swarm.datascript.coordination.coordinator
  "Coordinator instance registry (register, heartbeat, stale marking).

   Compat shim: moved to hive-datascript.swarm.coordination.coordinator in
   hive-datascript. Every public var delegates there; see
   hive-mcp.swarm.delegate for behavior without the library."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.coordination.coordinator" sym))

(defn get-all-coordinators {:arglists '([])} [& args] (apply (impl 'get-all-coordinators) args))
(defn get-coordinator {:arglists '([coordinator-id])} [& args] (apply (impl 'get-coordinator) args))
(defn get-coordinators-by-status {:arglists '([status])} [& args] (apply (impl 'get-coordinators-by-status) args))
(defn get-coordinators-for-project {:arglists '([project])} [& args] (apply (impl 'get-coordinators-for-project) args))
(defn mark-coordinator-stale! {:arglists '([coordinator-id])} [& args] (apply (impl 'mark-coordinator-stale!) args))
(defn mark-coordinator-terminated! {:arglists '([coordinator-id])} [& args] (apply (impl 'mark-coordinator-terminated!) args))
(defn register-coordinator! {:arglists '([coordinator-id {:keys [project pid session-id]}])} [& args] (apply (impl 'register-coordinator!) args))
(defn remove-coordinator! {:arglists '([coordinator-id])} [& args] (apply (impl 'remove-coordinator!) args))
(defn update-heartbeat! {:arglists '([coordinator-id])} [& args] (apply (impl 'update-heartbeat!) args))
