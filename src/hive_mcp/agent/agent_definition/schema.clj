(ns hive-mcp.agent.agent-definition.schema
  "Contract schemas for the agent-definition composition layer
   (compose / frontmatter / validate).

   Distinct from `spec` (the AgentDefinition SHAPE a definition must satisfy):
   these describe what the loaders and validators ACCEPT and RETURN, which is
   wider (a loader result may be an error map, a definition may be a record).
   Leaf namespace, malli only; every schema generates.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def AnyMap
  "Any map (records included)."
  [:map-of :any :any])

(def Source
  "A definition source keyword."
  [:enum :built-in :plugin :project :user])

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
