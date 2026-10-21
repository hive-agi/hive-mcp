(ns hive-mcp.swarm.datalevin.connection
  "Datalevin LMDB connection management for swarm state.

   Compat shim: moved to hive-agent.swarm.datalevin.connection in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript.schema :as ds-schema]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.datalevin.connection" sym))

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

;; Contracts transcribe the docstrings and bodies of
;; hive-agent.swarm.datalevin.connection, where the behaviour lives.

(def ^:private SwarmId
  "gen-id: a random UUID string, optionally prefixed with `<prefix>-`."
  [:string {:min 36}])

(defn clear-db!
  {:arglists '([confirm])}
  [& args]
  (apply (impl! 'clear-db!) args))

;; Any confirm value is read: anything but :i-mean-it is refused with an
;; ex-info, which is the guard the docstring promises. The destruction path
;; ends in a log call (or the coordinator guard's skip), so it answers nil.
(m/=> clear-db! [:=> [:cat :any] :nil])

(defn close!
  {:arglists '([])}
  [& args]
  (apply (impl! 'close!) args))

(m/=> close! [:=> [:cat] :nil])

(defn create-conn
  {:arglists '([])}
  [& args]
  (apply (impl! 'create-conn) args))

(m/=> create-conn [:=> [:cat] ds-schema/Conn])

(defn current-db
  {:arglists '([])}
  [& args]
  (apply (impl! 'current-db) args))

;; A Datalevin DB is a record, so a map.
(m/=> current-db [:=> [:cat] :map])

(defn db
  {:arglists '([c])}
  [& args]
  (apply (impl! 'db) args))

(m/=> db [:=> [:cat ds-schema/Conn] :map])

(defn ensure-conn
  {:arglists '([])}
  [& args]
  (apply (impl! 'ensure-conn) args))

(m/=> ensure-conn [:=> [:cat] ds-schema/Conn])

(defn gen-id
  {:arglists '([] [prefix])}
  [& args]
  (apply (impl! 'gen-id) args))

(m/=> gen-id [:function
              [:=> [:cat] [:string {:min 36 :max 36}]]
              [:=> [:cat :any] SwarmId]])

(defn get-conn
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-conn) args))

(m/=> get-conn [:=> [:cat] ds-schema/Conn])

(defn get-db-path
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-db-path) args))

;; The override, else HIVE_SWARM_DB_PATH, else the default under user.home.
(m/=> get-db-path [:=> [:cat] :string])

(defn now
  {:arglists '([])}
  [& args]
  (apply (impl! 'now) args))

(m/=> now [:=> [:cat] ds-schema/Inst])

(defn reset-conn!
  {:arglists '([])}
  [& args]
  (apply (impl! 'reset-conn!) args))

;; Ends in a log call, or in the coordinator guard's skip log: nil either way.
(m/=> reset-conn! [:=> [:cat] :nil])

(defn set-db-path!
  {:arglists '([path])}
  [& args]
  (apply (impl! 'set-db-path!) args))

;; reset! answers the new value; nil clears the override.
(m/=> set-db-path! [:=> [:cat [:maybe :string]] [:maybe :string]])
