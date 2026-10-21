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

(def RetagOpts
  "The ±tags delta of a retag."
  [:maybe [:map
           [:add-tags {:optional true} TagsIn]
           [:remove-tags {:optional true} TagsIn]]])

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
           [:priority {:optional true} :any]]])

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

(def FxMap
  "Effect map of a kanban handler. The soft commit is always present."
  [:map
   [:kanban/facade-update FacadeUpdate]
   [:kanban/track-movement {:optional true}
    [:map [:task-id :any] [:title :any] [:from :any] [:to :any] [:project-id :any]]]
   [:kanban/temporal-record {:optional true}
    [:map [:entry-id :any] [:op [:enum :kanban-move :kanban-done :kanban-retag :kanban-edit]]
     [:data :map] [:project-id :any]]]
   [:kanban/notify-done {:optional true} [:map [:entry :map] [:task-id :any]]]
   [:kanban/archive-external {:optional true} [:map [:entry :map] [:task-id :any]]]])

(def DispatchPayload
  "Payload of a public kanban dispatch entry point."
  [:map
   [:task-id {:optional true} :any]
   [:new-status {:optional true} :any]
   [:new-project-id {:optional true} :any]
   [:add-tags {:optional true} TagsIn]
   [:remove-tags {:optional true} TagsIn]
   [:title {:optional true} :any]
   [:description {:optional true} :any]
   [:append-description {:optional true} :any]
   [:priority {:optional true} :any]
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
  "`gather-kanban-summary`: bucket counts, recent todos, scope tag."
  [:map
   [:counts Counts]
   [:recent-todos [:vector RecentTodo]]
   [:scope-tag [:maybe :string]]])

(def SummaryIn
  "A summary as `catchup-section` reads it."
  [:maybe [:map
           [:counts {:optional true} [:maybe [:map-of :keyword [:int {:min 0}]]]]
           [:recent-todos {:optional true} [:maybe [:sequential RecentTodo]]]
           [:scope-tag {:optional true} [:maybe :string]]]])

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
