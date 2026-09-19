(ns hive-mcp.tools.agent.dag
  "DAG scheduler subcommand handlers (start, stop, status).

   Compat shim: moved to hive-agent.swarm.tools.agent.dag in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.tools.agent.dag" sym))

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

(defn handle-dag-start
  {:arglists '([{:keys [plan_id cwd max_slots presets project_id]}])}
  [& args]
  (apply (impl! 'handle-dag-start) args))

(defn handle-dag-status
  {:arglists '([_params])}
  [& args]
  (apply (impl! 'handle-dag-status) args))

(defn handle-dag-stop
  {:arglists '([_params])}
  [& args]
  (apply (impl! 'handle-dag-stop) args))
