(ns hive-mcp.swarm.datascript.connection
  "DataScript connection management for swarm state.

   Compat shim: moved to hive-agent.swarm.datascript.connection in hive-agent.
   Every public var delegates there; see hive-mcp.swarm.delegate for behavior
   without the addon. The *test-conn* override is not re-exported: set it
   through `with-test-conn`."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript.schema :as s]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.connection" sym))

(def DSConn @(impl 'DSConn))
(defn create-conn {:arglists '([])} [& args] (apply (impl 'create-conn) args))

;; Contracts transcribe hive-datascript.swarm.connection, where the behaviour
;; lives.
(m/=> create-conn [:=> [:cat] s/Conn])
(defn current-test-conn {:arglists '([])} [& args] (apply (impl 'current-test-conn) args))

(m/=> current-test-conn [:=> [:cat] [:maybe s/Conn]])
(defn ensure-conn {:arglists '([])} [& args] (apply (impl 'ensure-conn) args))

(m/=> ensure-conn [:=> [:cat] s/Conn])
(defn gen-id {:arglists '([] [prefix])} [& args] (apply (impl 'gen-id) args))

(m/=> gen-id [:function
              [:=> [:cat] [:string {:min 36 :max 36}]]
              [:=> [:cat :any] [:string {:min 37}]]])
(defn get-conn {:arglists '([])} [& args] (apply (impl 'get-conn) args))

(m/=> get-conn [:=> [:cat] s/Conn])
(defn now {:arglists '([])} [& args] (apply (impl 'now) args))

(m/=> now [:=> [:cat] s/Inst])
(defn reset-conn! {:arglists '([])} [& args] (apply (impl 'reset-conn!) args))

;; Ends in a log call, or in the coordinator guard's skip log: nil either way.
(m/=> reset-conn! [:=> [:cat] :nil])
(defn with-test-conn {:arglists '([test-conn f])} [& args] (apply (impl 'with-test-conn) args))

;; Answers whatever F answers, with TEST-CONN bound as this thread's conn.
(m/=> with-test-conn [:=> [:cat s/Conn ifn?] :any])
