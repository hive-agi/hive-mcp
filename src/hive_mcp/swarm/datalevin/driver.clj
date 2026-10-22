(ns hive-mcp.swarm.datalevin.driver
  "Late-bound facade over datalevin.core for the swarm coordination store.

   Compat shim: moved to hive-agent.swarm.datalevin.driver in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript.schema :as ds-schema]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.datalevin.driver" sym))

(defn- impl!
  "impl, after making sure the host adapters fill the swarm port slots.
   The adapters are resolved late: a static require would close a load
   cycle through the events adapter. A failure here (for instance a call
   made while that cycle is still loading) leaves the ports on their
   noops until a later call installs them."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

(defn close
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'close) args))

;; Contracts transcribe the datalevin.core signatures each passthrough
;; forwards to (late-bound by hive-agent.swarm.datalevin.driver). `q` is
;; exempted in the swarm/datalevin coverage gate: its result shape is chosen
;; by the query's :find spec.
(m/=> close [:=> [:cat ds-schema/Conn] :nil])

(defn db
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'db) args))

;; A Datalevin DB is a record, so a map.
(m/=> db [:=> [:cat ds-schema/Conn] :map])

(defn get-conn
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'get-conn) args))

;; (get-conn dir), (get-conn dir schema), (get-conn dir schema opts).
(m/=> get-conn [:=> [:cat :string [:? [:maybe :map]] [:? [:maybe :map]]] ds-schema/Conn])

(defn pull
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'pull) args))

;; (pull db selector eid) or with opts; an absent entity pulls nil.
(m/=> pull [:=> [:cat :map [:sequential :any] :any [:? [:maybe :map]]] [:maybe :map]])

(defn q
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'q) args))

;; (q query & inputs): a query form (vector, map or string) then its inputs.
;; The result is whatever the query's :find spec projects (a set of tuples, a
;; scalar, a collection, a pulled map), so the output is stated as open.
(m/=> q [:=> [:cat [:or [:vector :any] :map :string] [:* :any]] :any])

(defn transact!
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'transact!) args))

;; (transact! conn tx-data) or with tx-meta; answers the transaction report.
(m/=> transact! [:=> [:cat ds-schema/Conn [:sequential :any] [:? :any]]
                 [:map [:db-before :any] [:db-after :any] [:tx-data [:sequential :any]]]])
