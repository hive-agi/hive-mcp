(ns hive-mcp.swarm.datascript.connection
  "DataScript connection management for swarm state.

   Compat shim: moved to hive-agent.swarm.datascript.connection in hive-agent.
   Every public var delegates there; see hive-mcp.swarm.delegate for behavior
   without the addon. The *test-conn* override is not re-exported: set it
   through `with-test-conn`."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.datascript.connection" sym))

(def DSConn @(impl 'DSConn))
(defn create-conn {:arglists '([])} [& args] (apply (impl 'create-conn) args))
(defn current-test-conn {:arglists '([])} [& args] (apply (impl 'current-test-conn) args))
(defn ensure-conn {:arglists '([])} [& args] (apply (impl 'ensure-conn) args))
(defn gen-id {:arglists '([] [prefix])} [& args] (apply (impl 'gen-id) args))
(defn get-conn {:arglists '([])} [& args] (apply (impl 'get-conn) args))
(defn now {:arglists '([])} [& args] (apply (impl 'now) args))
(defn reset-conn! {:arglists '([])} [& args] (apply (impl 'reset-conn!) args))
(defn with-test-conn {:arglists '([test-conn f])} [& args] (apply (impl 'with-test-conn) args))
