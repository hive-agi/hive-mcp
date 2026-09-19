(ns hive-mcp.tools.swarm.jvm.memory
  "Memory usage monitoring and threshold checking for resource guard.

   Compat shim: moved to hive-agent.swarm.tools.jvm.memory in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.tools.jvm.memory" sym))

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

(defn get-memory-usage
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-memory-usage) args))

(defn memory-high?
  {:arglists '([mem-info threshold-percent min-available-mb])}
  [& args]
  (apply (impl! 'memory-high?) args))

(defn memory-status
  {:arglists '([mem-info threshold-percent min-available-mb])}
  [& args]
  (apply (impl! 'memory-status) args))

(defn spawn-recommendation
  {:arglists '([can-spawn auto-cleanup cleanup-dry])}
  [& args]
  (apply (impl! 'spawn-recommendation) args))
