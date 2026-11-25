(ns hive-mcp.tools.kanban.schema
  "Malli value objects for the kanban bounded context (transitions, filters,
   events, effects, catchup block, migration).

   The list pipeline keeps its own shapes in `hive-mcp.tools.kanban.list.schema`;
   this namespace holds the shapes the rest of `tools.kanban` speaks.

   Inputs are permissive (what the bodies accept: entries straight from a
   store, JSON-roundtripped content, nil), outputs are what the functions
   promise. Every schema generates."
  (:require [hive-mcp.tools.result-bridge :as rb]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Scalars
;; =============================================================================

(def StatusText
  "A status as callers send it: an MCP enum spelling, a canonical internal
   tag, or any other string (normalisation passes unknown values through)."
  [:string {:gen/elements ["todo" "doing" "review" "done" "inprogress" "inreview" "bogus"]}])

(def PriorityText
  "A priority as callers send it; the enum is not enforced by the pure fns."
  [:string {:gen/elements ["high" "medium" "low" "urgent"]}])

(def ProjectId
  "A bare project id (the part after `scope:project:`)."
  [:string {:gen/elements ["hive-mcp" "proj-a" "proj-b" "global"]}])

(def KanbanTimestamp
  "`yyyy-MM-dd'T'HH:mm:ssZ`, the colonless-offset stamp kanban writes."
  [:re {:gen/elements ["2026-10-20T19:33:31-0300" "2026-01-01T00:00:00+0000"]}
   #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}[+-]\d{4}$"])

(def TagIn
  "One tag as stores hand it back: normally a string, occasionally junk."
  [:or [:string {:gen/elements ["kanban" "todo" "doing" "done" "priority-high"
                                "priority-low" "scope:project:proj-a"
                                "scope:project:proj-b" "topic" "session-todo"]}]
   :nil :keyword])

(def TagsIn
  "A tag collection as callers pass it: vector, seq, set or nil."
  [:maybe [:or [:sequential TagIn] [:set TagIn]]])

(def StringTags
  "A tag vector of strings."
  [:vector :string])

;; =============================================================================
;; Entries
;; =============================================================================

(def ^:private ContentShape
  "The structured shape content maps are GENERATED from."
  [:map
   [:task-type {:optional true} [:maybe [:string {:gen/elements ["kanban" "note" "research"]}]]]
   [:title {:optional true} [:maybe [:string {:gen/max 8}]]]
   [:status {:optional true} [:maybe StatusText]]
   [:priority {:optional true} [:maybe PriorityText]]
   [:description {:optional true} [:maybe [:string {:gen/max 8}]]]
   [:created {:optional true} [:maybe KanbanTimestamp]]])

(def ContentMap
  "A kanban content map as stored: keyword or JSON-roundtripped string keys,
   any values (the pure fns read them through `content-val`). Generated from
   the structured kanban shape."
  [:map-of {:gen/schema ContentShape} :any :any])

(def EntryIn
  "A memory entry as the store returns it. :content is usually a map but may
   be a JSON string or absent."
  [:map
   [:id {:optional true} [:maybe [:or [:string {:gen/max 12}] :keyword :int]]]
   [:tags {:optional true} TagsIn]
   [:content {:optional true} [:maybe [:or ContentMap [:string {:gen/max 8}]]]]])

(def MaybeEntry [:maybe EntryIn])

(def Task
  "A task map handed to sorting / projection (slim rows, entries)."
  [:map-of :keyword :any])

(def SlimTask
  "`task->slim`: id, title, status, priority; :project with multi-project?."
  [:map
   [:id :any]
   [:title :any]
   [:status :any]
   [:priority :any]
   [:project {:optional true} [:maybe :string]]])

(def DetailTask
  "`task->detail`: the slim shape plus :description and a keyword-keyed
   :context when present."
  [:map
   [:id :any]
   [:title :any]
   [:status :any]
   [:priority :any]
   [:project {:optional true} [:maybe :string]]
   [:description {:optional true} :any]
   [:context {:optional true} [:map-of :any :any]]])

(def Fields
  "A field projection: names as strings or keywords."
  [:maybe [:or [:sequential [:or :string :keyword]] [:set [:or :string :keyword]]]])

(def ListParams
  "The list request keys the post-filter predicates read (boundary shape)."
  [:maybe
   [:map
    [:query {:optional true} :any]
    [:priority {:optional true} :any]
    [:created_after {:optional true} :any]
    [:updated_after {:optional true} :any]
    [:tags {:optional true} TagsIn]
    [:tag_match {:optional true} [:maybe [:or :string :keyword]]]
    [:offset {:optional true} :any]
    [:limit {:optional true} :any]
    [:fields {:optional true} Fields]]])

;; =============================================================================
;; Transition results
;; =============================================================================

(def TransitionResult
  "`transition`: the derived status move."
  [:map {:closed true}
   [:old-status :any]
   [:new-status :any]
   [:priority :any]
   [:title :any]
   [:project-id [:maybe :string]]
   [:new-content [:map-of :any :any]]
   [:new-tags [:vector [:maybe :string]]]])

(def TagDeltas
  "A ±tags delta: tags to add, then tags to drop. Shared by retag and by the
   add_tags/remove_tags of edit and update."
  [:maybe [:map
           [:add-tags {:optional true} TagsIn]
           [:remove-tags {:optional true} TagsIn]]])

(def RetagOpts
  "The ±tags delta of a retag."
  TagDeltas)

(def DeltaTags
  "A tag vector after a ±tags delta: distinct, the base's order kept, added
   tags appended. The elements are whatever the base held (a store can hand
   back junk), so it is not narrowed to strings."
  [:vector TagIn])

(def RetagResult
  "`retag-transition`: the derived tag-only mutation."
  [:map {:closed true}
   [:old-project-id [:maybe :string]]
   [:new-project-id :any]
   [:old-tags [:vector :any]]
   [:new-tags [:vector :any]]
   [:status :any]
   [:priority :any]
   [:title :any]])

(def Edits
  "The editable fields of a kanban edit; non-blank strings take effect."
  [:maybe [:map
           [:title {:optional true} :any]
           [:description {:optional true} :any]
           [:append-description {:optional true} :any]
           [:priority {:optional true} :any]
           [:at {:optional true} :any]
           [:agent-id {:optional true} :any]]])

(def EditResult
  "`edit-transition`: the derived content edit."
  [:map {:closed true}
   [:old-title :any]
   [:new-title :any]
   [:old-priority :any]
   [:new-priority :any]
   [:changed [:set [:enum :title :description :priority]]]
   [:new-content [:map-of :any :any]]
   [:new-tags [:maybe StringTags]]])

(def FieldChanges
  "The content fields `edit-transition` found changed."
  [:set [:enum :title :description :priority]])

(def EditChanged
  "Every field an edit changed: the content fields, plus :tags when an
   add_tags/remove_tags delta moved the tag vector."
  [:set [:enum :title :description :priority :tags]])

(def TagDiff
  "The audit diff of a tag rewrite: the tags that landed, the tags that left."
  [:map {:closed true}
   [:added-tags [:vector TagIn]]
   [:removed-tags [:vector TagIn]]])

(def EditAudit
  "`:data` of a `:kanban-edit` temporal record. :added-tags/:removed-tags are
   present only when the tags changed, so the trail can reconstruct a verdict
   change (RUN-LEDGER-3)."
  [:map {:closed true}
   [:changed [:vector [:enum "description" "priority" "tags" "title"]]]
   [:old-title :any]
   [:new-title :any]
   [:old-priority :any]
   [:new-priority :any]
   [:added-tags {:optional true} [:vector TagIn]]
   [:removed-tags {:optional true} [:vector TagIn]]])

(def EditPlan
  "`edit-plan`: an edit over an entry, as the values its effects are built
   from. :new-tags is nil when the edit writes no tags; :audit is nil when
   nothing changed, so a no-op edit leaves no audit noise."
  [:map {:closed true}
   [:new-content [:map-of :any :any]]
   [:new-tags [:maybe DeltaTags]]
   [:changed EditChanged]
   [:audit [:maybe EditAudit]]])

;; =============================================================================
;; Events and effects
;; =============================================================================

(def Coeffects
  "What the kanban event handlers read from the coeffect map."
  [:map
   [:kanban/entry {:optional true} MaybeEntry]
   [:kanban/project-id {:optional true} [:maybe ProjectId]]])

(def Event
  "A dispatched event tuple `[id payload]`."
  [:tuple :keyword [:maybe [:map-of :keyword :any]]])

(def FacadeUpdate
  [:map [:task-id :any] [:payload [:map-of :keyword :any]]])

(def EditPayload
  "The facade payload of an edit: the content, plus the tags only when the
   edit writes them."
  [:map {:closed true}
   [:content [:map-of :any :any]]
   [:tags {:optional true} DeltaTags]])

(def TemporalOp
  "The audit op of a kanban commit."
  [:enum :kanban-move :kanban-done :kanban-retag :kanban-edit])

(def TemporalRecord
  "The audit record a kanban commit emits (`temporal/record-mutation-silent!`)."
  [:map [:entry-id :any] [:op TemporalOp] [:data :map] [:project-id :any]])

(def ClaimRefused
  "Effect map of a refused card claim: nothing is written."
  [:map {:closed true}
   [:kanban/claim-refused
    [:map [:refused [:= true]] [:holder :string] [:since [:maybe :string]] [:task-id :any]]]])

(def CommitFxMap
  "Effect map of a kanban handler that commits."
  [:map
   [:kanban/facade-update FacadeUpdate]
   [:kanban/track-movement {:optional true}
    [:map [:task-id :any] [:title :any] [:from :any] [:to :any] [:project-id :any]]]
   [:kanban/temporal-record {:optional true} TemporalRecord]
   [:kanban/notify-done {:optional true} [:map [:entry :map] [:task-id :any]]]
   [:kanban/archive-external {:optional true} [:map [:entry :map] [:task-id :any]]]])

(def FxMap
  "Effect map of a kanban handler. The soft commit is always present, except
   in a refused claim (`ClaimRefused`), which writes nothing."
  [:or ClaimRefused CommitFxMap])

(def DispatchPayload
  "Payload of a public kanban dispatch entry point."
  [:map
   [:task-id {:optional true} :any]
   [:new-status {:optional true} :any]
   [:new-project-id {:optional true} :any]
   ;; :any, not TagsIn: update/edit accept one tag string and refuse a bad
   ;; shape themselves (pred/coerce-tags -> :kanban/invalid-tags).
   [:add-tags {:optional true} :any]
   [:remove-tags {:optional true} :any]
   [:title {:optional true} :any]
   [:description {:optional true} :any]
   [:append-description {:optional true} :any]
   [:priority {:optional true} :any]
   [:agent-id {:optional true} :any]
   [:owner {:optional true} :any]
   [:steal-claim? {:optional true} :any]
   [:directory {:optional true} [:maybe :string]]])

(def Result
  "hive-dsl Result at the dispatch boundary."
  rb/Result)

(def HandlerRegistry
  "What a hive.events `reg-*` call answers: the registry it wrote to."
  [:map-of :keyword :any])

(def ArchiveData
  "`archive-task-data`: the :da/archive! payload."
  [:map {:closed true}
   [:id :any]
   [:title :any]
   [:description :any]
   [:priority :any]
   [:scope [:maybe :string]]
   [:agent-id :any]
   [:files :any]
   [:created-at :any]
   [:started-at :any]
   [:context :any]
   [:tags [:vector :any]]])

;; =============================================================================
;; Catchup block
;; =============================================================================

(def Counts [:map-of :keyword [:int {:min 0}]])

(def RecentTodo [:map [:id :any] [:title :any] [:tags :any]])

(def Summary
  "`gather-kanban-summary`: bucket counts, distinct project count, recent
   todos, scope tag. :projects is absent when the board read failed."
  [:map
   [:counts Counts]
   [:projects {:optional true} [:int {:min 0}]]
   [:recent-todos [:vector RecentTodo]]
   [:scope-tag [:maybe :string]]])

(def SummaryIn
  "A summary as `catchup-section` reads it."
  [:maybe [:map
           [:counts {:optional true} [:maybe [:map-of :keyword [:int {:min 0}]]]]
           [:projects {:optional true} [:maybe [:int {:min 0}]]]
           [:recent-todos {:optional true} [:maybe [:sequential RecentTodo]]]
           [:scope-tag {:optional true} [:maybe :string]]]])

(def BoardSummary
  "`summarize-board`: bucket counts, distinct project count, recent todos."
  [:map {:closed true}
   [:counts Counts]
   [:projects [:int {:min 0}]]
   [:recent-todos [:vector {:max 10} RecentTodo]]])

(def ScopeTags
  "`scope:project:*` tags of the visible projects."
  [:set [:string {:gen/elements ["scope:project:proj-a" "scope:project:proj-b"
                                 "scope:project:global"]}]])

(def BoardGate
  "What `gate-board` admits: entries whose scope tag is in :scope-tags, or
   with no scope tag and a :project-id in :visible-ids. nil admits all."
  [:maybe [:map
           [:scope-tags ScopeTags]
           [:visible-ids [:set ProjectId]]]])

(def BoardScopeSpec
  "The IBoardSource scope spec the catchup block reads with."
  [:map {:closed true}
   [:project-id [:maybe ProjectId]]
   [:include-descendants? :boolean]
   [:include-ancestors? [:= false]]])

(def BoardView
  "`board-view`: the scope spec to read with and the gate to apply."
  [:map {:closed true}
   [:scope-spec BoardScopeSpec]
   [:view BoardGate]])

(def Section
  "`catchup-section`: nil for an empty board, else the summary plus :hint."
  [:maybe [:map [:hint :string]]])

(def CatchupBlock
  "A catchup block contribution."
  [:map
   [:block/id :keyword]
   [:block/fn [:fn {:gen/elements [(fn [_] nil)]} ifn?]]
   [:block/order :int]
   [:block/cache {:optional true} :map]])

;; =============================================================================
;; Migration
;; =============================================================================

(def MigrateOpts
  [:maybe [:map
           [:batch-size {:optional true} [:int {:min 1}]]
           [:dry-run? {:optional true} :any]]])

(def SyncReport
  "IBatchMigrator `sync!` report."
  [:map
   [:extracted [:int {:min 0}]]
   [:transformed [:int {:min 0}]]
   [:loaded-ok [:int {:min 0}]]
   [:loaded-fail [:int {:min 0}]]
   [:loaded-queued {:optional true} [:int {:min 0}]]
   [:batches [:int {:min 0}]]
   [:dry-run? :boolean]])

(def VerifyOpts
  [:maybe [:map [:sample-size {:optional true} [:int {:min 0}]]]])

(def VerifyReport
  "IBatchMigrator `verify` report."
  [:map
   [:checked [:int {:min 0}]]
   [:ok [:int {:min 0}]]
   [:missing [:vector :any]]])

(def MigrationStatus
  [:map {:closed true}
   [:default-registered? :boolean]
   [:kanban-registered? :boolean]
   [:source-count [:int {:min 0}]]
   [:sample-ids [:vector :any]]])
