;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.bootstrap.datahike
  "DatahikeBootstrap - persistent slave projection backed by a dedicated
   Datahike store at `data/swarm/datahike` (separate from the KG store).

   Compat shim: moved to hive-agent.swarm.bootstrap.datahike in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.bootstrap.datahike" sym))

(defn ->DatahikeBootstrap
  {:arglists '([conn-atom cfg])}
  [& args]
  (apply (impl '->DatahikeBootstrap) args))

(defn map->DatahikeBootstrap
  {:arglists '([m])}
  [& args]
  (apply (impl 'map->DatahikeBootstrap) args))

(defn make-datahike-bootstrap
  {:arglists '([] [{:keys [db-path] :or {db-path default-db-path}}])}
  [& args]
  (apply (impl 'make-datahike-bootstrap) args))
