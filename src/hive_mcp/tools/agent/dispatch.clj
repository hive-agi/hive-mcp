(ns hive-mcp.tools.agent.dispatch
  "Agent task dispatch handler with context abstraction support."
  (:require [hive-dsl.result :as r]
            [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.transport.delivery :as delivery]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [taoensso.timbre :as log]
            [malli.core :as m]
            [hive-mcp.tools.result-bridge :as rb]))
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

(defn dispatch-ling!
  "Dispatch a task to the swarm-registry ling `agent_id`: the terminal path
   (proto/dispatch!). Answers the success data map, throws when the ling is
   unknown."
  [{:keys [agent_id prompt files priority ctx_refs kg_node_ids scope deliver-fn]}]
  (if-let [agent-data (queries/get-slave agent_id)]
    (let [agent (ling/->ling agent_id {:cwd (:slave/cwd agent-data)
                                       :presets (:slave/presets agent-data)
                                       :project-id (:slave/project-id agent-data)
                                       :model (:ling/model agent-data)
                                       :provider (:ling/provider agent-data)
                                       :token-budget (:ling/token-budget agent-data)
                                       :turn-budget (:ling/turn-budget agent-data)
                                       :time-budget-ms (:ling/time-budget-ms agent-data)
                                       :spawn-mode (or (:ling/spawn-mode agent-data) :claude)})
          ctx (build-dispatch-context prompt ctx_refs kg_node_ids
                                      (or scope (:slave/project-id agent-data)))
          resolved-prompt (:prompt (dispatch-ctx/resolve-context ctx))
          task-opts (cond-> {:task resolved-prompt
                             :dispatch-context ctx
                             :files files
                             :priority (keyword (or priority "normal"))}
                      deliver-fn (assoc :deliver-fn deliver-fn))
          task-id (proto/dispatch! agent task-opts)]
      (log/info "Dispatched task to agent" {:agent_id agent_id
                                            :task-id task-id
                                            :context-type (dispatch-ctx/context-type ctx)})
      {:success true
       :agent-id agent_id
       :task-id task-id
       :context-type (name (dispatch-ctx/context-type ctx))
       :files files})
    (throw (ex-info (str "Agent not found: " agent_id) {:agent_id agent_id}))))

(defn- chain-annotation
  "The part of a delivery Result the caller sees: :via, :resolved-by, :skipped."
  [res]
  (let [v (if (r/ok? res) (:ok res) res)]
    (select-keys v [:via :resolved-by :skipped])))

(defn- delivered-response
  [agent_id res]
  (let [{:keys [value]} (:ok res)
        data (if (map? value) value {})]
    (mcp-json (merge {:success true :agent-id agent_id}
                     data
                     (chain-annotation res)))))

(defn- failed-response
  [agent_id res]
  (if (= :addressing (:error/kind res))
    (mcp-error (or (:message res) (str "Agent not found: " agent_id)))
    (mcp-error (str "Failed to dispatch: no delivery route reached " agent_id
                    " " (pr-str (chain-annotation res))))))

(defn handle-dispatch
  "Dispatch a task to an agent through the :delivery transport chain. The
   target kind is resolved first (ling, coordinator, else an addressing
   error with no fallback); the answer carries :via, :resolved-by, :skipped."
  [{:keys [agent_id prompt] :as params}]
  (cond
    (empty? agent_id)
    (mcp-error "agent_id is required")

    (empty? prompt)
    (mcp-error "prompt is required")

    :else
    (try
      (let [res (delivery/deliver! {:to agent_id :message prompt :params params})]
        (if (r/ok? res)
          (delivered-response agent_id res)
          (failed-response agent_id res)))
      (catch Exception e
        (log/error "Failed to dispatch to agent" {:agent_id agent_id :error (ex-message e)})
        (mcp-error (str "Failed to dispatch: " (ex-message e)))))))

;; =============================================================================
;; Contracts
;; =============================================================================

(def DispatchParams
  "agent dispatch params. agent_id and prompt are validated by the body (a
   missing one answers an MCP error), so they are optional here."
  [:maybe [:map
           [:agent_id {:optional true} [:maybe :string]]
           [:prompt {:optional true} [:maybe :string]]
           [:files {:optional true} [:maybe [:sequential :string]]]
           [:priority {:optional true} [:maybe [:or :string :keyword]]]
           [:ctx_refs {:optional true} [:maybe [:map-of [:or :string :keyword] :string]]]
           [:kg_node_ids {:optional true} [:maybe [:sequential :string]]]
           [:scope {:optional true} [:maybe :string]]]])

(m/=> handle-dispatch [:=> [:cat DispatchParams] rb/McpResponse])
(m/=> dispatch-ling! [:=> [:cat DispatchParams] :map])
