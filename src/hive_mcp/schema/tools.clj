(ns hive-mcp.schema.tools
  "Malli schemas for MCP tool parameters."

  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]
            [malli.util :as mu]
            [malli.json-schema :as json-schema]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [hive-mcp.schema.memory :as mem]
            [hive-mcp.swarm.adapters.soft :as soft]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Common Types
;; =============================================================================

(def NonEmptyString
  "Non-empty string type."
  [:string {:min 1}])

(def NonBlankString
  "String with at least one non-whitespace character."
  [:and NonEmptyString [:re #"(?s).*\S.*"]])

(def OptionalString
  "Optional string (may be nil or string)."
  [:maybe :string])

(def PositiveInt
  "Positive integer (> 0)."
  [:int {:min 1}])

(def NonNegativeInt
  "Non-negative integer (>= 0)."
  [:int {:min 0}])

(def Confidence
  "Confidence score between 0.0 and 1.0."
  [:double {:min 0.0 :max 1.0}])

;; =============================================================================
;; Agent Tool Schemas
;; =============================================================================

(def AgentType
  "Agent type for spawning."
  [:enum "ling"])

(def TerminalType
  "Terminal type for agent spawn."
  [:enum "vterm" "eat"])

(defn- blank->nil
  "Decode rule: a blank JSON string means the param was not given."
  [s]
  (if (and (string? s) (str/blank? s)) nil s))

(defn- drop-absent-resume-keys
  "Decode null branch keys and empty branch points as absent."
  [m]
  (if (map? m)
    (let [at (:at m)
          at (if (map? at) (into {} (remove (comp nil? val)) at) at)]
      (cond-> (assoc m :at at)
        (or (nil? at) (= {} at)) (dissoc :at)
        (and (contains? m :prompt) (nil? (blank->nil (:prompt m)))) (dissoc :prompt)))
    m))

(def ChatPointAt
  "Branch point inside a recorded chat-point run: a history seq OR a turn."
  [:or {:json-schema/description "Optional branch point: give seq OR turn. Omitted = resume at the run's tip."}
   [:map {:closed true}
    [:seq {:json-schema/description "Fork from this recorded history seq."} nat-int?]]
   [:map {:closed true}
    [:turn {:json-schema/description "Fork from the end of this recorded turn."} nat-int?]]])

(def ResumeParam
  "Spawn `resume`: continue run_id at its tip, or fork it at `at`."
  [:map {:closed true
         :decode/string {:leave drop-absent-resume-keys}}
   [:run_id {:json-schema/description "Chat-point run id to continue: a prior collect's chat_point.run_id, or the transcript tool's runs command."}
    NonBlankString]
   [:at {:optional true} ChatPointAt]
   [:prompt {:optional true :json-schema/description "Optional message appended at the resume/branch point."}
    :string]])

(def SpawnLoopParams
  "Spawn params read only by a chat-point capable headless loop."
  [:map
   [:llm_retries {:optional true} [:maybe nat-int?]]
   [:resume {:optional true} [:maybe ResumeParam]]
   [:chat_run_id {:optional true} [:maybe {:decode/string blank->nil} NonEmptyString]]])

(def AgentSpawnParams
  "Parameters for agent spawn."
  (mu/merge
   [:map
    [:name NonEmptyString]
    [:presets {:optional true} [:maybe [:vector NonEmptyString]]]
    [:cwd {:optional true} OptionalString]
    [:role {:optional true} OptionalString]
    [:terminal {:optional true} [:maybe TerminalType]]
    [:kanban_task_id {:optional true} OptionalString]]
   SpawnLoopParams))

(def AgentDispatchParams
  "Parameters for agent dispatch."
  [:map
   [:task NonEmptyString]
   [:ling_id {:optional true} OptionalString]
   [:priority {:optional true} [:enum "high" "medium" "low"]]
   [:context {:optional true} OptionalString]])

(def AgentKillParams
  "Parameters for agent kill."
  [:map
   [:slave_id NonEmptyString]
   [:force {:optional true} [:maybe :boolean]]
   [:directory {:optional true} OptionalString]])

(def AgentStatusParams
  "Parameters for agent status."
  [:map
   [:directory {:optional true} OptionalString]
   [:include_details {:optional true} [:maybe :boolean]]
   [:include_stale {:optional true} [:maybe :boolean]]])

(def AgentCollectParams
  "Parameters for agent collect."
  [:map
   [:ling_id {:optional true} OptionalString]
   [:timeout_ms {:optional true} PositiveInt]])

(def AgentBroadcastParams
  "Parameters for agent broadcast."
  [:map
   [:message NonEmptyString]
   [:scope {:optional true} OptionalString]])

;; =============================================================================
;; Memory Tool Schemas
;; =============================================================================

(def MemoryAddParams
  "Parameters for memory add."
  [:map
   [:type mem/MemoryType]
   [:content [:string {:min 1}]]
   [:tags {:optional true} [:maybe [:or mem/MemoryTags :string]]]  ; String for JSON array coercion
   [:duration {:optional true} [:maybe mem/MemoryDuration]]
   [:directory {:optional true} OptionalString]
   [:agent_id {:optional true} OptionalString]
   ;; KG relationship params
   [:kg_implements {:optional true} [:maybe [:or [:vector :string] :string]]]
   [:kg_supersedes {:optional true} [:maybe [:or [:vector :string] :string]]]
   [:kg_depends_on {:optional true} [:maybe [:or [:vector :string] :string]]]
   [:kg_refines {:optional true} [:maybe [:or [:vector :string] :string]]]
   [:abstraction_level {:optional true} [:maybe mem/AbstractionLevel]]])

(def MemoryQueryParams
  "Parameters for memory query."
  [:map
   [:type {:optional true} [:maybe mem/MemoryType]]
   [:tags {:optional true} [:maybe [:or mem/MemoryTags :string]]]
   [:limit {:optional true} [:maybe [:or PositiveInt :string]]]  ; String for JSON coercion
   [:duration {:optional true} [:maybe mem/MemoryDuration]]
   [:scope {:optional true} OptionalString]
   [:directory {:optional true} OptionalString]])

(def MemoryQueryMetadataParams
  "Parameters for memory metadata query."
  MemoryQueryParams)

(def MemoryGetFullParams
  "Parameters for memory get by ID."
  [:map
   [:id NonEmptyString]])

(def MemoryUpdateTagsParams
  "Parameters for memory tag update."
  [:map
   [:id NonEmptyString]
   [:tags mem/MemoryTags]])

(def MemoryCheckDuplicateParams
  "Parameters for memory duplicate check."
  [:map
   [:type mem/MemoryType]
   [:content NonEmptyString]
   [:directory {:optional true} OptionalString]])

(def MemorySearchSemanticParams
  "Parameters for memory semantic search."
  [:map
   [:query NonEmptyString]
   [:type {:optional true} [:maybe mem/MemoryType]]
   [:limit {:optional true} [:maybe PositiveInt]]
   [:threshold {:optional true} [:maybe Confidence]]
   [:scope {:optional true} OptionalString]])

;; =============================================================================
;; Knowledge Graph Tool Schemas
;; =============================================================================

(defn KGRelationType
  "Valid KG edge relation types.

   The relation VOCABULARY is the knowledge graph's, not the kernel's, and
   `hive-mcp.knowledge-graph.schema` is a hive-memory extraction target, so it
   is resolved BY SYMBOL rather than required: with the KG domain present this
   is the enum it has always been, and without it the schema opens up to any
   non-blank string. Validating against an enum nobody defines would be the
   dishonest branch, not the permissive one.

   Derives dynamically to include registered extensions, and SORTS them.
   relation-types answers a set, so its iteration order follows the hash
   layout of whatever is registered. Order means nothing to malli, but this
   schema is one derivation away from an advertised JSON enum, and there the
   order is bytes in the first span of every request."
  []
  (if-let [relation-types (soft/resolve-soft 'hive-mcp.knowledge-graph.schema/relation-types)]
    (into [:enum] (sort (map name (relation-types))))
    [:string {:min 1}]))

(def KGSourceType
  "How an edge was established."
  [:enum "manual" "automated" "inferred" "co-access"])

(def KGDirection
  "Traversal direction."
  [:enum "outgoing" "incoming" "both"])

(def KGAddEdgeParams
  "Parameters for KG edge creation."
  [:map
   [:from NonEmptyString]
   [:to NonEmptyString]
   [:relation (KGRelationType)]
   [:scope {:optional true} OptionalString]
   [:confidence {:optional true} [:maybe Confidence]]
   [:created_by {:optional true} OptionalString]
   [:source_type {:optional true} [:maybe KGSourceType]]])

(def KGTraverseParams
  "Parameters for KG traversal."
  [:map
   [:start_node NonEmptyString]
   [:direction {:optional true} [:maybe KGDirection]]
   [:relations {:optional true} [:maybe [:or [:vector (KGRelationType)] (KGRelationType)]]]
   [:max_depth {:optional true} [:maybe PositiveInt]]
   [:scope {:optional true} OptionalString]])

(def KGImpactAnalysisParams
  "Parameters for KG impact analysis."
  [:map
   [:node_id NonEmptyString]
   [:max_depth {:optional true} [:maybe PositiveInt]]
   [:scope {:optional true} OptionalString]])

(def KGPromoteParams
  "Parameters for KG promotion."
  [:map
   [:edge_id NonEmptyString]
   [:to_scope NonEmptyString]])

(def KGFindPathParams
  "Parameters for KG path finding."
  [:map
   [:from_node NonEmptyString]
   [:to_node NonEmptyString]
   [:max_depth {:optional true} [:maybe PositiveInt]]
   [:relations {:optional true} [:maybe [:vector (KGRelationType)]]]])

(def KGSubgraphParams
  "Parameters for KG subgraph extraction."
  [:map
   [:scope NonEmptyString]
   [:include_global {:optional true} [:maybe :boolean]]])

(def KGNodeContextParams
  "Parameters for KG node context."
  [:map
   [:node_id NonEmptyString]
   [:depth {:optional true} [:maybe PositiveInt]]])

(def KGRegroundParams
  "Parameters for KG reground."
  [:map
   [:entry_id NonEmptyString]
   [:source_path {:optional true} OptionalString]])

(def KGContradictionsParams
  "Parameters for KG contradiction detection."
  [:map
   [:scope NonEmptyString]])

(def KGStatsParams
  "Parameters for KG stats."
  [:map
   [:scope {:optional true} OptionalString]])

;; =============================================================================
;; Response Schemas
;; =============================================================================

(def MCPSuccessResponse
  "Standard MCP success response."
  [:map
   [:type [:= "text"]]
   [:text :string]
   [:isError {:optional true} [:= false]]])

(def MCPErrorResponse
  "Standard MCP error response."
  [:map
   [:type [:= "text"]]
   [:text :string]
   [:isError [:= true]]])

(def MCPResponse
  "MCP tool response - either success or error."
  [:or MCPSuccessResponse MCPErrorResponse])

;; =============================================================================
;; Schema Registry
;; =============================================================================

(def registry
  "Schema registry entries for tool parameters."
  {;; Agent tools
   :tools/agent-spawn-params AgentSpawnParams
   :tools/agent-dispatch-params AgentDispatchParams
   :tools/agent-kill-params AgentKillParams
   :tools/agent-status-params AgentStatusParams
   :tools/agent-collect-params AgentCollectParams
   :tools/agent-broadcast-params AgentBroadcastParams

   ;; Memory tools
   :tools/memory-add-params MemoryAddParams
   :tools/memory-query-params MemoryQueryParams
   :tools/memory-get-full-params MemoryGetFullParams
   :tools/memory-update-tags-params MemoryUpdateTagsParams
   :tools/memory-check-duplicate-params MemoryCheckDuplicateParams
   :tools/memory-search-semantic-params MemorySearchSemanticParams

   ;; KG tools
   :tools/kg-add-edge-params KGAddEdgeParams
   :tools/kg-traverse-params KGTraverseParams
   :tools/kg-impact-analysis-params KGImpactAnalysisParams
   :tools/kg-promote-params KGPromoteParams
   :tools/kg-find-path-params KGFindPathParams
   :tools/kg-subgraph-params KGSubgraphParams
   :tools/kg-node-context-params KGNodeContextParams
   :tools/kg-reground-params KGRegroundParams

   ;; Common
   :tools/non-empty-string NonEmptyString
   :tools/confidence Confidence
   :tools/mcp-response MCPResponse})

;; =============================================================================
;; Validators
;; =============================================================================

(defn validate-params
  "Validate tool parameters against a schema."
  [schema params]
  (if (m/validate schema params)
    {:valid true}
    {:valid false
     :errors (m/explain schema params)}))

(def json-params-transformer
  "Decodes an MCP JSON param: string keys become keywords, numeric strings
   become numbers. Schema-declared :decode/string rules run here too."
  (mt/transformer (mt/key-transformer {:decode keyword}) mt/string-transformer))

(defn param-coercer
  "Decode raw MCP JSON and validate once; error identifies the failing field."
  [param schema]
  (m/coercer schema json-params-transformer identity
             (fn [{:keys [explain]}]
               (let [errors (me/humanize explain)
                     field (if (and (map? errors) (= 1 (count errors)))
                             (name (ffirst errors))
                             param)]
                 (throw (ex-info (str field " is invalid: " (pr-str errors))
                                 {:param field :errors errors}))))))

(defn json-schema
  "The MCP inputSchema projection of a malli `schema`, string-keyed like the
   hand-written tool schemas, so advertised and validated shape are one
   definition."
  [schema]
  (walk/postwalk (fn [node]
                   (if (map? node)
                     (cond-> node
                       (map? (:properties node)) (update :properties update-keys name)
                       (:required node)          (update :required #(mapv name %)))
                     node))
                 (json-schema/transform schema)))

(defn coerce-and-validate
  "Coerce and validate parameters."
  [schema params]
  ;; For now, just validate - coercion can be added with malli.transform
  (validate-params schema params))

(comment
  ;; Example usage

  (m/validate AgentSpawnParams
              {:name "worker-1"
               :cwd "/home/user/project"
               :presets ["research" "clojure"]})
  ;; => true

  (m/validate MemoryAddParams
              {:type "decision"
               :content "Use Malli for validation"
               :tags ["architecture"]
               :duration "long"})
  ;; => true

  (m/validate KGAddEdgeParams
              {:from "entry-1"
               :to "entry-2"
               :relation "implements"
               :confidence 0.95})
  ;; => true

  (m/explain AgentSpawnParams {:cwd "/tmp"})
  ;; => {:errors [{:path [:name] ...}]}
  )
