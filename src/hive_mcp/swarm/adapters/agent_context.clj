(ns hive-mcp.swarm.adapters.agent-context
  "hive-mcp adapter for the swarm agent-context port.

   Reifies hive-spi.swarm.ports.agent-context/IAgentCallContext and
   /IBudgetGuardrail over the host's own machinery:

   - IAgentCallContext delegates to hive-mcp.agent.context — the thread-local
     *request-ctx* middleware chain keeps working unchanged, so every
     wrap-handler-context-bound call sees the same agent-id / directory the
     swarm slice reads today via ctx/current-agent-id and ctx/current-directory.
   - IBudgetGuardrail delegates to hive-mcp.agent.hooks.budget — the same
     surface the spawn pipeline already reaches via soft requiring-resolve
     (agent/ling/spawn.clj), so budget behaviour is unchanged; the adapter
     simply formalizes that seam into the port. The budget hook is a
     hive-agent extraction target in the kernel census, so it stays soft here
     too (hive-mcp.swarm.adapters.soft): absent, the methods answer the
     port's Noop (nil, budgets unenforced).

   Install at addon init (the same moment headless/terminal strategies
   register): (install!) — sets the port slot via
   hive-spi.swarm.ports.agent-context/set-agent-context!. Clear it at addon
   shutdown with (uninstall!); without an install the port answers the Noop
   (nil context, unenforced budgets), never throws."
  ;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
  ;;
  ;; SPDX-License-Identifier: AGPL-3.0-or-later
  (:require [hive-spi.swarm.ports.agent-context :as spi]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.swarm.adapters.soft :as soft]))

(defn make-adapter
  "An IAgentCallContext + IBudgetGuardrail reify delegating to hive-mcp's
   thread-local request context and budget hook."
  []
  (reify spi/IAgentCallContext
    (current-agent-id [_this]
      (ctx/current-agent-id))
    (current-directory [_this]
      (ctx/current-directory))
    spi/IBudgetGuardrail
    (register-budget! [_this agent-id max-budget-usd opts]
      (try
        (soft/host-or 'hive-mcp.agent.hooks.budget/register-budget!
                      #(spi/register-budget! spi/noop agent-id max-budget-usd opts)
                      agent-id max-budget-usd opts)
        (catch Exception _ nil)))
    (deregister-budget! [_this agent-id]
      (try
        (soft/host-or 'hive-mcp.agent.hooks.budget/deregister-budget!
                      #(spi/deregister-budget! spi/noop agent-id)
                      agent-id)
        (catch Exception _ nil)))))

(defn install!
  "Install the hive-mcp adapter into the swarm agent-context port.
   Last installer wins. Returns the adapter."
  []
  (spi/set-agent-context! (make-adapter)))

(defn uninstall!
  "Clear the installed adapter so consumers fall back to the port's Noop.
   Returns nil."
  []
  (spi/clear-agent-context!))
