(ns hive-mcp.swarm.datalevin.driver
  "Late-bound facade over datalevin.core for the swarm coordination store.

   Compat shim: moved to hive-agent.swarm.datalevin.driver in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
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

(defn db
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'db) args))

(defn get-conn
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'get-conn) args))

(defn pull
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'pull) args))

(defn q
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'q) args))

(defn transact!
  {:arglists '([& args])}
  [& args]
  (apply (impl! 'transact!) args))
