(ns hive-mcp.swarm.datascript.schema
  "DataScript schema definitions for swarm hivemind coordination.

   Compat shim: moved to hive-agent.swarm.datascript.schema in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.datascript.schema" sym))

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

(def agent-types @(impl 'agent-types))

(defn claude-model?
  {:arglists '([model])}
  [& args]
  (apply (impl! 'claude-model?) args))

(def coordinator-statuses @(impl 'coordinator-statuses))

(def critical-op-types @(impl 'critical-op-types))

(def daemon-health-levels @(impl 'daemon-health-levels))

(def daemon-statuses @(impl 'daemon-statuses))

(def ling-model-default @(impl 'ling-model-default))

(def olympus-layout-modes @(impl 'olympus-layout-modes))

(def schema @(impl 'schema))

(def slave-statuses @(impl 'slave-statuses))

(def spawn-modes @(impl 'spawn-modes))

(def task-statuses @(impl 'task-statuses))

(def task-types @(impl 'task-types))
