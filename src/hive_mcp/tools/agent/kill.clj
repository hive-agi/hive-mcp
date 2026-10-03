(ns hive-mcp.tools.agent.kill
  "Agent kill and kill-batch handlers.

   Compat shim: moved to hive-agent.swarm.tools.agent.kill in hive-agent, which
   owns the verb (ownership checks and cascade cancellation of the subtree).
   Every public var delegates there through requiring-resolve; hive-mcp does
   not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.tools.agent.kill" sym))

(defn handle-kill
  {:arglists '([{:keys [agent_id cascade directory force_cross_project]}])}
  [& args]
  (apply (impl 'handle-kill) args))

(defn handle-kill-batch
  {:arglists '([{:keys [agent_ids cascade directory force_cross_project]}])}
  [& args]
  (apply (impl 'handle-kill-batch) args))
