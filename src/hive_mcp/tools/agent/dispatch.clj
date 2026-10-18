(ns hive-mcp.tools.agent.dispatch
  "Agent task dispatch handler with context abstraction support."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- build-dispatch-context
  "Build an IDispatchContext from dispatch parameters."
  [prompt ctx_refs kg_node_ids scope]
  (if (seq ctx_refs)
    (let [refs-map (reduce-kv (fn [m k v] (assoc m (keyword k) v)) {} ctx_refs)]
      (dispatch-ctx/->ref-context prompt
                                  {:ctx-refs    refs-map
                                   :kg-node-ids (vec (or kg_node_ids []))
                                   :scope       scope}))
    (dispatch-ctx/ensure-context prompt)))

(defn dispatch-error-text
  "The MCP error text for an exception E a dispatch threw. A wrapper's own
   message names no cause (the ling dispatch re-throws as \"Failed to
   dispatch to ling\"), so the root cause's message, from the wrapper's
   ex-data :error or its ex-cause, is appended unless the message already
   carries it. Pure."
  [e]
  (let [msg   (or (ex-message e) (str (type e)))
        cause (or (some-> (ex-data e) :error str)
                  (some-> (ex-cause e) ex-message))]
    (str "Failed to dispatch: "
         (if (and cause (not (.contains ^String msg ^String cause)))
           (str msg ": " cause)
           msg))))

(defn handle-dispatch
  "Dispatch a task to an agent. A refused dispatch answers the root cause's
   words (`dispatch-error-text`), not only the wrapper's."
  [{:keys [agent_id prompt files priority ctx_refs kg_node_ids scope]}]
  (cond
    (empty? agent_id)
    (mcp-error "agent_id is required")

    (empty? prompt)
    (mcp-error "prompt is required")

    :else
    (try
      (if-let [agent-data (queries/get-slave agent_id)]
        (let [agent (ling/->ling agent_id {:cwd (:slave/cwd agent-data)
                                           :presets (:slave/presets agent-data)
                                           :project-id (:slave/project-id agent-data)
                                           :model (:ling/model agent-data)
                                           :provider (:ling/provider agent-data)
                                           :token-budget (:ling/token-budget agent-data)
                                           :turn-budget (:ling/turn-budget agent-data)
                                           :spawn-mode (or (:ling/spawn-mode agent-data) :claude)})
              ctx (build-dispatch-context prompt ctx_refs kg_node_ids
                                          (or scope (:slave/project-id agent-data)))
              resolved-prompt (:prompt (dispatch-ctx/resolve-context ctx))
              task-opts {:task resolved-prompt
                         :dispatch-context ctx
                         :files files
                         :priority (keyword (or priority "normal"))}
              task-id (proto/dispatch! agent task-opts)]
          (log/info "Dispatched task to agent" {:agent_id agent_id
                                                :task-id task-id
                                                :context-type (dispatch-ctx/context-type ctx)})
          (mcp-json {:success true
                     :agent-id agent_id
                     :task-id task-id
                     :context-type (name (dispatch-ctx/context-type ctx))
                     :files files}))
        (mcp-error (str "Agent not found: " agent_id)))
      (catch Exception e
        (log/error "Failed to dispatch to agent" {:agent_id agent_id :error (ex-message e)})
        (mcp-error (dispatch-error-text e))))))
