(ns hive-mcp.swarm.datascript.coordination.wrap-queue
  "Ling wrap notification queue for coordinator permeation.

   Compat shim: moved to hive-agent.swarm.datascript.coordination.wrap-queue in
   hive-agent. Every public var delegates there; see hive-mcp.swarm.delegate for
   behavior without the addon."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.datascript.coordination.wrap-queue" sym))

(defn add-wrap-notification! {:arglists '([wrap-id {:keys [agent-id session-id project-id created-ids stats parent-session-id depth]}])} [& args] (apply (impl 'add-wrap-notification!) args))
(defn get-unprocessed-wraps {:arglists '([])} [& args] (apply (impl 'get-unprocessed-wraps) args))
(defn get-unprocessed-wraps-for-hierarchy {:arglists '([project-id-prefix])} [& args] (apply (impl 'get-unprocessed-wraps-for-hierarchy) args))
(defn get-unprocessed-wraps-for-project {:arglists '([project-id])} [& args] (apply (impl 'get-unprocessed-wraps-for-project) args))
(defn get-unprocessed-wraps-for-session {:arglists '([parent-session-id])} [& args] (apply (impl 'get-unprocessed-wraps-for-session) args))
(defn mark-wrap-processed! {:arglists '([wrap-id])} [& args] (apply (impl 'mark-wrap-processed!) args))
