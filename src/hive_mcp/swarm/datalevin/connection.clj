(ns hive-mcp.swarm.datalevin.connection
  "Datalevin LMDB connection management for swarm state.

   Compat shim: moved to hive-agent.swarm.datalevin.connection in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
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

(defn clear-db!
  {:arglists '([confirm])}
  [& args]
  (apply (impl! 'clear-db!) args))

(defn close!
  {:arglists '([])}
  [& args]
  (apply (impl! 'close!) args))

(defn create-conn
  {:arglists '([])}
  [& args]
  (apply (impl! 'create-conn) args))

(defn current-db
  {:arglists '([])}
  [& args]
  (apply (impl! 'current-db) args))

(defn db
  {:arglists '([c])}
  [& args]
  (apply (impl! 'db) args))

(defn ensure-conn
  {:arglists '([])}
  [& args]
  (apply (impl! 'ensure-conn) args))

(defn gen-id
  {:arglists '([] [prefix])}
  [& args]
  (apply (impl! 'gen-id) args))

(defn get-conn
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-conn) args))

(defn get-db-path
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-db-path) args))

(defn now
  {:arglists '([])}
  [& args]
  (apply (impl! 'now) args))

(defn reset-conn!
  {:arglists '([])}
  [& args]
  (apply (impl! 'reset-conn!) args))

(defn set-db-path!
  {:arglists '([path])}
  [& args]
  (apply (impl! 'set-db-path!) args))
