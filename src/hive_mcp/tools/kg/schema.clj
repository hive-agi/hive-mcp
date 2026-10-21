(ns hive-mcp.tools.kg.schema
  "Value-object schemas for the KG tool handlers (tools/kg): handler params,
   the node-id validation error, the bulk edge envelope and the
   synthetic-cleanup reports. Every schema here generates.

   Handler params are MCP boundary input: permissive in. A field the body
   reads through a `case`/`cond` with a default branch is `Loose` (any value,
   generated from the documented shape), never a closed enum."
  (:require [hive-mcp.tools.core :as core]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Shared atoms
;; =============================================================================

(defn- loose
  "Any value, generated from `shape` (the documented spelling)."
  [shape]
  [:any {:gen/schema shape}])

(def NodeIdText
  "A node id as a caller sends it. The generator mixes real ids, an empty
   string and unresolved `$ref` placeholders so both validation arms run."
  [:string {:gen/elements ["a-1" "b-1" "c-2" "node-x" "synth-20260101T000000-x"
                           "" "$ref" "$x.data.id"]}])

(def NodeIdArg
  "Whatever a caller passes as a node id: validate-node-id refuses non-strings."
  (loose [:maybe [:or NodeIdText :int :keyword]]))

(def NodeIdError
  "What `queries/validate-node-id` answers for a refused id."
  [:map {:closed true} [:error :string]])

(def KgResponse
  "What a KG handler that validates node ids answers: an MCP response, or
   the bare `{:error msg}` map `validate-node-id` produced.
   BUG: the refusal escapes the handler as a plain map, not as an MCP error
   (handle-kg-traverse {:start_node nil} => {:error \"start_node must be a
   non-empty string\"}). The schema records current behaviour."
  [:or core/McpResponse NodeIdError])

(def Relations
  "The relations filter a caller may send: absent, one name, a list of names
   or an already-parsed keyword set. The fn accepts any value."
  (let [rel [:enum "implements" "refines" "depends-on" "projects-to"]]
    (loose [:or :nil rel [:vector {:max 3} rel] [:set {:max 3} [:enum :implements :refines]] :int])))

(def RelationSet
  "Parsed relation filter: nil, or a set. Elements are keywords for string
   input; a set input is returned unchanged."
  [:maybe [:set :any]])

(def Depth
  (loose [:maybe [:int {:min 1 :max 4}]]))

(def ScopeText
  (loose [:maybe [:string {:gen/elements ["" "global" "hive-mcp" "hive-mcp:agora"]}]]))

;; =============================================================================
;; Handler params (open maps: MCP params carry extra keys)
;; =============================================================================

(def TraverseParams
  [:map
   [:start_node {:optional true} NodeIdArg]
   [:direction {:optional true} (loose [:maybe [:enum "outgoing" "incoming" "both" :incoming :both]])]
   [:relations {:optional true} Relations]
   [:max_depth {:optional true} Depth]
   [:scope {:optional true} ScopeText]])

(def ImpactParams
  [:map
   [:node_id {:optional true} NodeIdArg]
   [:max_depth {:optional true} Depth]
   [:scope {:optional true} ScopeText]])

(def FindPathParams
  [:map
   [:from_node {:optional true} NodeIdArg]
   [:to_node {:optional true} NodeIdArg]
   [:direction {:optional true} (loose [:maybe [:enum "outgoing" "incoming" "both" :outgoing :incoming]])]
   [:relations {:optional true} Relations]
   [:max_depth {:optional true} Depth]
   [:scope {:optional true} ScopeText]])

(def ScopeParams
  [:map [:scope {:optional true} ScopeText]])

(def NodeParams
  [:map [:node_id {:optional true} NodeIdArg]])

(def AnyParams
  "A handler that ignores its params."
  (loose [:maybe [:map [:scope {:optional true} :string]]]))

(def EdgeOp
  "One requested edge (kg_add_edge params, or one batch-edge op)."
  [:map
   [:from {:optional true} NodeIdArg]
   [:to {:optional true} NodeIdArg]
   [:relation {:optional true}
    (loose [:maybe [:or [:enum "implements" "refines" "depends-on" "nope" ""] :keyword :int]])]
   [:scope {:optional true} ScopeText]
   [:confidence {:optional true} (loose [:maybe [:double {:min 0.0 :max 1.0}]])]
   [:created_by {:optional true} (loose [:maybe :string])]
   [:predicate {:optional true} (loose [:maybe :string])]])

(def AddEdgesParams
  [:map [:operations {:optional true} (loose [:maybe [:vector {:max 4} EdgeOp]])]])

(def RegroundParams
  [:map
   [:entry_id {:optional true} NodeIdArg]
   [:force {:optional true} (loose [:maybe :boolean])]])

(def BackfillParams
  [:map
   [:project_id {:optional true} (loose [:maybe :string])]
   [:limit {:optional true} (loose [:maybe [:int {:min 0 :max 5}]])]
   [:force {:optional true} (loose [:maybe :boolean])]
   [:max_age_days {:optional true} (loose [:maybe [:int {:min 0 :max 30}]])]])

(def Handler
  "An MCP handler: a fn of params."
  [:fn {:gen/elements [(fn [_] {:type "text" :text "ok"})]
        :error/message "must be a handler fn"}
   ifn?])

(def TransactFn
  "The write port `add-edges-with` takes: a fn of tx-data."
  [:fn {:gen/elements [(constantly nil)]
        :error/message "must be a transact fn"}
   ifn?])

;; =============================================================================
;; Bulk edge envelope
;; =============================================================================

(def ClassifiedOp
  "One op after validation: [:ok edge-spec] or [:error message].
   batch-envelope reads only the tag, so the spec is any map (permissive in)."
  [:or
   [:tuple [:= :ok] [:map {:gen/schema [:map [:from :string] [:to :string] [:relation :keyword]]}]]
   [:tuple [:= :error] [:maybe :string]]])

(def BulkWrite
  "Outcome of the single bulk write: {:ids [...]} or {:error msg}."
  [:map
   [:ids {:optional true} [:maybe [:sequential :string]]]
   [:error {:optional true} [:maybe :string]]])

(def EdgeOpResult
  [:or
   [:map {:closed true}
    [:success [:= true]] [:command [:= "edge"]]
    [:result [:map {:closed true} [:success [:= true]] [:edge-id :string]]]]
   [:map {:closed true}
    [:success [:= false]] [:command [:= "edge"]] [:error [:maybe :string]]]])

(def BatchEnvelope
  [:map {:closed true}
   [:results [:vector EdgeOpResult]]
   [:summary [:map {:closed true}
              [:total nat-int?] [:success nat-int?] [:failed nat-int?]]]
   [:write_error {:optional true} :string]])

;; =============================================================================
;; Synthetic cleanup
;; =============================================================================

(def SynthId
  [:string {:gen/elements ["synth-a" "synth-b" "synth-c" "synth-20260101T000000-x"]}])

(def Sources
  "The synth- source universe as passed in: any sequence of ids."
  [:maybe [:sequential (loose SynthId)]])

(def RecordVisibility
  [:map {:closed true}
   [:visible? :boolean]
   [:records [:maybe [:set :any]]]
   [:reason {:optional true} :string]])

(def VisibilityIn
  "The record-visibility map `partition-sources` reads."
  [:map
   [:visible? {:optional true} (loose :boolean)]
   [:records {:optional true} [:maybe [:set (loose SynthId)]]]])

(def Partition
  [:map {:closed true}
   [:registered [:vector :any]]
   [:orphans [:vector :any]]])

(def ContinueFn
  "0-arg fn asked before every chunk; nil means go on."
  [:fn {:gen/elements [(constantly nil) (constantly :deadline)]} ifn?])

(def RetractOpts
  [:map
   [:budget {:optional true} [:maybe [:int {:gen/min 0 :gen/max 10}]]]
   [:chunk-size {:optional true} [:int {:min 1 :gen/max 10}]]
   [:continue? {:optional true} ContinueFn]])

(def RetractResult
  [:map {:closed true}
   [:removed nat-int?]
   [:remaining :int]
   [:complete? :boolean]
   [:stopped {:optional true} some?]])

(def ReapOpts
  [:maybe
   [:map
    [:limit {:optional true} [:int {:gen/min 0 :gen/max 5}]]
    [:edge-budget {:optional true} [:int {:gen/min 0 :gen/max 50}]]
    [:chunk-size {:optional true} [:int {:min 1 :gen/max 10}]]
    [:continue? {:optional true} ContinueFn]
    [:dry-run? {:optional true} (loose [:maybe :boolean])]
    [:sources {:optional true} Sources]]])

(def ReapResult
  [:map {:closed true}
   [:visible? :boolean]
   [:orphans-total nat-int?]
   [:reaped nat-int?]
   [:partial [:vector :any]]
   [:edges-removed :int]
   [:dry-run? :boolean]
   [:reason {:optional true} :string]
   [:stopped {:optional true} some?]])

(def CleanupOpts
  [:maybe
   [:map
    [:threshold {:optional true} [:maybe number?]]
    [:min-live-members {:optional true} [:maybe [:int {:gen/min 0 :gen/max 4}]]]
    [:action {:optional true} (loose [:enum :delete :demote "delete" "demote"])]
    [:limit {:optional true} [:int {:gen/min 0 :gen/max 5}]]
    [:after {:optional true} (loose [:maybe SynthId])]
    [:edge-budget {:optional true} [:int {:gen/min 0 :gen/max 50}]]
    [:chunk-size {:optional true} [:maybe [:int {:min 1 :gen/max 10}]]]
    [:continue? {:optional true} [:maybe ContinueFn]]
    [:dry-run? {:optional true} (loose [:maybe :boolean])]]])

(def BlindClass
  [:map {:closed true}
   [:class [:enum :universe :orphaned-record :liveness]]
   [:reason [:maybe :string]]])

(def CleanupResult
  "`cleanup-synthetics!` report. Open: the opts echo varies."
  [:map
   [:dry-run? :boolean]
   [:action [:maybe :keyword]]
   [:threshold [:maybe number?]]
   [:min-live-members [:maybe :int]]
   [:limit :int]
   [:universe [:map {:closed true}
               [:sources nat-int?] [:orphans nat-int?]
               [:registered nat-int?] [:projection-edges :int]]]
   [:blind [:vector BlindClass]]
   [:scanned nat-int?]
   [:orphaned nat-int?]
   [:pruned nat-int?]
   [:demoted nat-int?]
   [:preserved nat-int?]
   [:errors nat-int?]
   [:details [:vector :map]]
   [:partial [:vector :any]]
   [:edge-budget-left {:optional true} :int]
   [:next-cursor {:optional true} [:maybe :string]]
   [:error {:optional true} :string]])

(def CleanupParams
  "MCP params for kg_cleanup_synthetics (snake_case, loosely typed)."
  [:map
   [:threshold {:optional true} (loose [:maybe [:or [:double {:min -0.5 :max 1.5}] :string]])]
   [:action {:optional true} (loose [:maybe [:or [:enum "delete" "demote" "nuke"] :keyword :int]])]
   [:limit {:optional true} (loose [:maybe [:or [:int {:min -2 :max 5}] :string]])]
   [:dry_run {:optional true} (loose [:maybe :boolean])]
   [:min_live_members {:optional true} (loose [:maybe [:or [:int {:min -1 :max 4}] :string]])]])
