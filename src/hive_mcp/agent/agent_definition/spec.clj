(ns hive-mcp.agent.agent-definition.spec
  "Schema definitions, enum values, and record type for agent definitions.

   Leaf namespace — pure malli schemas + the AgentDef defrecord.
   No heavy hive-mcp deps. See parent `hive-mcp.agent.agent-definition`
   for the public façade.

   SLAP role: spec — value objects and schemas. The AgentDef defrecord
   itself lives in the façade namespace so the generated class keeps its
   stable Java name (`hive_mcp.agent.agent_definition.AgentDef`)."

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later
  )

;; =============================================================================
;; Enumeration Schemas (Value Objects)
;; =============================================================================

(def Source
  "Where the agent definition came from.
   Priority (low→high): :built-in < :plugin < :project < :user
   Higher-priority sources override lower ones during merge."
  [:enum :built-in :project :user :plugin])

(def source-priority
  "Numeric priority for merge ordering. Higher wins."
  {:built-in 0
   :plugin   1
   :project  2
   :user     3})

;; =============================================================================
;; Sub-Schemas
;; =============================================================================

(def ToolSpec
  "Tool specification: either a tool name string or \"*\" for all tools.
   When [\"*\"] is specified, the agent has access to all available tools."
  [:vector :string])

(def HooksSpec
  "Hooks configuration map. Keys are lifecycle event names,
   values are hook handler specifications.

   Example:
   {:pre-tool-call  [{:command \"validate.sh\" :timeout 5000}]
    :post-tool-call [{:command \"log.sh\"}]}"
  [:map-of :keyword [:vector [:map
                              [:command :string]
                              [:timeout {:optional true} :int]]]])

(def McpServerSpec
  "MCP server specification — either a reference by name (string)
   or an inline definition map.

   Examples:
   - \"slack\"                         ; reference existing server
   - {\"my-server\" {:command \"node\" :args [\"server.js\"]}} ; inline"
  [:or :string [:map-of :string :any]])

;; =============================================================================
;; Agent Definition Schema
;; =============================================================================

(def AgentDefinition
  "Malli schema for a declarative agent definition.

   Required fields:
   - :agent-type    Unique identifier (e.g. \"general-purpose\", \"explore\")
   - :description   When-to-use description (shown in agent picker)
   - :system-prompt The agent's system prompt (string) or 0-arity fn returning string

   Optional fields:
   - :tools            Allowed tool names, or [\"*\"] for all (default: all)
   - :disallowed-tools Explicitly disallowed tool names
   - :model            Model override (string) or \"inherit\" for parent's model
   - :max-turns        Maximum agentic turns before stopping
   - :source           Where this definition came from
   - :hooks            Lifecycle hooks map
   - :mcp-servers      MCP server specs specific to this agent
   - :skills           Skill names to preload (e.g. [\"simplify\" \"review-pr\"])
   - :filename         Original .md filename (without extension)
   - :base-dir         Directory the definition was loaded from
   - :spawn-mode       Preferred spawn mode keyword (links to spawn-mode-registry)
   - :hivemind-role    Hivemind role ADT keyword (default: :role/standalone)
                       See hive-mcp.agent.hivemind-role for ADT dispatch"
  [:map {:closed false}
   [:agent-type :string]
   [:description :string]
   [:system-prompt [:or :string fn?]]
   [:source {:optional true :default :built-in} Source]
   [:tools {:optional true} ToolSpec]
   [:disallowed-tools {:optional true} ToolSpec]
   [:model {:optional true} [:or :string [:= :inherit]]]
   [:max-turns {:optional true} pos-int?]
   [:hooks {:optional true} HooksSpec]
   [:mcp-servers {:optional true} [:vector McpServerSpec]]
   [:skills {:optional true} [:vector :string]]
   [:filename {:optional true} [:maybe :string]]
   [:base-dir {:optional true} [:maybe :string]]
   [:spawn-mode {:optional true} :keyword]
   [:hivemind-role {:optional true :default :role/standalone}
    [:enum :role/hivemind :role/worker :role/standalone]]])

;; Note: the AgentDef defrecord is declared in the parent façade namespace
;; (hive-mcp.agent.agent-definition) to preserve its original Java class
;; name for downstream Java-style interop and `instance?` checks.

;; =============================================================================
;; Contract Schemas for the composition layer (compose / frontmatter / validate)
;; =============================================================================
;;
;; Distinct from the AgentDefinition SHAPE above (what a definition must
;; satisfy): these describe what the loaders and validators ACCEPT and RETURN,
;; which is wider (a loader result may be an error map, a definition may be a
;; record). Every schema generates. They live in this leaf namespace because
;; hive-mcp.agent.* is a frozen extraction prefix (hive-mcp.kernel.census): no
;; new namespace may be added under it. The definition source keyword is the
;; `Source` enum above.

(def AnyMap
  "Any map (records included)."
  [:map-of :any :any])

(def Definitions
  "A collection of definition maps (vector, list or lazy seq)."
  [:sequential AnyMap])

(def LoadError
  "Error row of a loader: {:error msg :file path}."
  [:map [:error :string] [:file :string]])

(def LoadedAgent
  "A definition produced by the frontmatter coercion."
  [:map
   [:agent-type :string]
   [:description :string]
   [:system-prompt :string]
   [:source :any]
   [:tools {:optional true} [:vector :string]]
   [:disallowed-tools {:optional true} [:vector :string]]
   [:model {:optional true} [:or :string [:= :inherit]]]
   [:max-turns {:optional true} [:int {:min 1}]]
   [:hooks {:optional true} [:map-of :keyword [:vector :any]]]
   [:mcp-servers {:optional true} [:vector [:or :string AnyMap]]]
   [:skills {:optional true} [:vector :string]]
   [:spawn-mode {:optional true} :keyword]
   [:hivemind-role {:optional true} [:enum :role/hivemind :role/worker :role/standalone]]
   [:filename {:optional true} :string]
   [:base-dir {:optional true} :string]])

(def FrontmatterResult
  "parse-frontmatter result."
  [:map
   [:frontmatter AnyMap]
   [:content :string]
   [:parse-error {:optional true} [:maybe :string]]])

(def CoerceResult
  "frontmatter->agent-def result: nil (not an agent file), an error map for a
   named agent missing its description, or the coerced definition."
  [:maybe [:or [:map [:error :string]] LoadedAgent]])

(def LoadResult
  "load-agent-file result: nil, an error row, or a definition."
  [:maybe [:or LoadError LoadedAgent]])

(def LoadedDir
  "{:agents [...] :errors [...]} of a directory load."
  [:map
   [:agents [:vector AnyMap]]
   [:errors [:vector AnyMap]]])

(def ActiveAgents
  [:map
   [:active [:vector AnyMap]]
   [:all [:vector AnyMap]]
   [:errors [:maybe [:sequential AnyMap]]]])

(def Validation
  "validate result: success carries the data, failure the humanized errors."
  [:or
   [:map [:valid [:= true]] [:data AnyMap]]
   [:map [:valid [:= false]] [:errors :any]]])
