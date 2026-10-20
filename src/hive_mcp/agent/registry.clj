(ns hive-mcp.agent.registry
  "Tool and agent registry for delegation."
  (:require [hive-mcp.agent.protocol :refer [IAgentRegistry agent-type]]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-dsl.result :as r]
            [taoensso.timbre :as log]
            [hive-mcp.tools.schema-keys :as schema-keys]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce registry (atom {}))

(declare register!)

(defonce ^:private initialized? (atom false))

(defn- advertised-tools
  "The advertised surface, resolved through its var per call so a reload of
   the routes namespace is seen here. Loading it is a require, never a
   :reload: hive-hot is the only reload authority."
  []
  ((requiring-resolve 'hive-mcp.server.routes/advertised-tool-defs)))

(defn- by-name
  "tools -> {name tool}. Pure."
  [tools]
  (into {} (map (juxt :name identity)) tools))

;; ---------------------------------------------------------------------------
;; Schemas (value objects; every schema here generates)
;; ---------------------------------------------------------------------------

(def ToolDef
  "A tool as held by the delegation registry."
  [:map [:name :any]])

(def AgentMap
  "A registered agent: any map, records (Ling, Drone) included."
  [:map-of :any :any])

(defn ensure-registered!
  "Lazily initialize tool registry on first use."
  []
  (when (and (empty? @registry)
             (not @initialized?))
    (reset! initialized? true)
    (let [v (r/guard Exception nil
                     (log/info "Auto-registering tools for agent delegation...")
                     (register! (advertised-tools)))]
      (when-let [err (::r/error (meta v))]
        (log/warn "Failed to auto-register tools:" (:message err))))))

(defn register!
  "Register tools for agent use."
  [tools]
  (doseq [{:keys [name handler] :as tool} tools]
    (swap! registry assoc name (assoc tool :handler handler)))
  (log/info "Registered" (count tools) "tools for agent delegation"))

(def ^:private open-schema-hint
  " Compact schema: only the shared parameters are listed; each command accepts its own. Use command='help' to list them.")

(defn- open-consolidated
  "A consolidated root routes many commands, each reading params the root
   does not list. Opens its inputSchema (`additionalProperties true`) so a
   ling can send them. Any other tool is returned unchanged. Pure."
  [tool]
  (cond-> tool
    (:consolidated tool)
    (-> (assoc-in [:inputSchema :additionalProperties] true)
        (update :description str open-schema-hint))))

(defn get-schemas
  "Get tool schemas for specified tool names (or all if nil). Property keys
   are provider-legal (`schema-keys/legal-tool`): these schemas go straight
   to an LLM provider as a ling's tool list. A consolidated root comes back
   open (`open-consolidated`)."
  [tool-names]
  (let [all-tools @registry
        selected (if tool-names
                   (select-keys all-tools tool-names)
                   all-tools)]
    (mapv #(-> % (dissoc :handler) open-consolidated schema-keys/legal-tool)
          (vals selected))))

(defn get-tool
  "Get a tool by name from the registry."
  [tool-name]
  (get @registry tool-name))

(defn list-tools
  "List all registered tool names."
  []
  (keys @registry))
(def ^:dynamic *advertised-tools*
  "Port for the advertised tool source. Tests bind a stub surface rather than
   redefining the transport or the addon registry."
  advertised-tools)

(defn refresh!
  "Replace the delegation registry with the advertised surface, in ONE reset!
   (no empty window a concurrent lookup could fall into). Returns the tool
   count, including zero for a legitimately empty surface. A failed read
   throws: returning zero used to report success beside the OLD registry."
  []
  (let [table (by-name (*advertised-tools*))]
    (reset! registry table)
    (reset! initialized? true)
    (log/info "[hot-reload] Tool registry refreshed:" (count table) "tools")
    (count table)))

(defonce ^:private agents (atom {}))

(defrecord AgentRegistry []
  IAgentRegistry
  (register! [_ agent]
    (let [id (:id agent)]
      (swap! agents assoc id agent)
      (log/debug "Registered agent:" id)
      id))

  (unregister! [_ agent-id]
    (swap! agents dissoc agent-id)
    (log/debug "Unregistered agent:" agent-id)
    agent-id)

  (get-agent [_ agent-id]
    (get @agents agent-id))

  (list-agents [_]
    (vals @agents))

  (list-agents-by-type [_ t]
    (filter #(= t (agent-type %)) (vals @agents))))

(def agent-registry
  "Singleton agent registry instance."
  (->AgentRegistry))

(defn register-agent!
  "Register an agent in the registry."
  [agent]
  (.register! agent-registry agent))

(defn unregister-agent!
  "Remove an agent from the registry."
  [agent-id]
  (.unregister! agent-registry agent-id))

(defn get-agent-by-id
  "Get an agent by ID."
  [id]
  (.get-agent agent-registry id))

(defn list-all-agents
  "List all registered agents."
  []
  (.list-agents agent-registry))

(defn list-lings
  "List all ling agents."
  []
  (.list-agents-by-type agent-registry :ling))

(defn sync-from-datascript!
  "Sync agent registry with DataScript slave state."
  []
  (let [ling-ns (requiring-resolve 'hive-mcp.agent.ling/->ling)
        slaves (ds-queries/get-all-slaves)
        agents-created (for [slave slaves]
                         (let [id (:slave/id slave)
                               opts {:cwd (:slave/cwd slave)
                                     :project-id (:slave/project-id slave)
                                     :presets (:slave/presets slave)}]
                           (ling-ns id opts)))]
    (doseq [agent agents-created]
      (register-agent! agent))
    (log/info "Synced agent registry from DataScript:" (count agents-created) "agents")
    {:synced-count (count agents-created)
     :agents (vec agents-created)}))

(defn clear-agents!
  "Clear all agents from the registry."
  []
  (reset! agents {})
  (log/info "Cleared agent registry"))

(m/=> ensure-registered! [:=> [:cat] :nil])

(m/=> register! [:=> [:cat [:maybe [:sequential ToolDef]]] :nil])

(m/=> get-schemas
      [:=> [:cat [:maybe [:sequential :any]]] [:vector [:map-of :any :any]]])

(m/=> get-tool [:=> [:cat :any] [:maybe [:map-of :any :any]]])

(m/=> list-tools [:=> [:cat] [:maybe [:sequential :any]]])

(m/=> refresh! [:=> [:cat] :int])

(m/=> register-agent! [:=> [:cat [:map [:id :any]]] :any])

(m/=> unregister-agent! [:=> [:cat :any] :any])

(m/=> get-agent-by-id [:=> [:cat :any] [:maybe AgentMap]])

(m/=> list-all-agents [:=> [:cat] [:maybe [:sequential AgentMap]]])

(m/=> list-lings [:=> [:cat] [:sequential AgentMap]])

(m/=> sync-from-datascript! [:=> [:cat] [:map [:synced-count :int] [:agents [:vector AgentMap]]]])

(m/=> clear-agents! [:=> [:cat] :nil])
