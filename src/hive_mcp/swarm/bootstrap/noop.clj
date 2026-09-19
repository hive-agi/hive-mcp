;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.bootstrap.noop
  "NoopBootstrap - explicit Null Object for swarm bootstrap.

   Compat shim: moved to hive-agent.swarm.bootstrap.noop in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.bootstrap.noop" sym))

(defn ->NoopBootstrap
  {:arglists '([])}
  [& args]
  (apply (impl '->NoopBootstrap) args))

(defn map->NoopBootstrap
  {:arglists '([m])}
  [& args]
  (apply (impl 'map->NoopBootstrap) args))

(defn make-noop-bootstrap
  {:arglists '([])}
  [& args]
  (apply (impl 'make-noop-bootstrap) args))
