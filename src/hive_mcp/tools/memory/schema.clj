(ns hive-mcp.tools.memory.schema
  "Value-object schemas for the top-level memory tool namespaces
   (tools/memory/*.clj): memory entries as the handlers read them, handler
   params, scope tags, duration plans, decay/promotion reports and the
   batch runner. Every schema here generates.

   Handler params are MCP boundary input: permissive in. Outputs follow
   what the handlers promise."
  (:require [hive-mcp.batch :as batch]
            [hive-mcp.memory.domain :as domain]
            [hive-mcp.memory.reconcile :as reconcile]
            [malli.core :as m]
            [malli.util :as mu]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- loose
  "Any value, generated from `shape` (the documented spelling)."
  [shape]
  [:any {:gen/schema shape}])

;; =============================================================================
;; Atoms
;; =============================================================================

(def DurationName
  [:enum "ephemeral" "short" "medium" "long" "permanent"])

(def DurationText
  "A duration as stored: usually a DurationName, but read from entries as
   whatever string they carry."
  [:string {:gen/elements ["ephemeral" "short" "medium" "long" "permanent"]}])

(def ProjectIdText
  [:string {:gen/elements ["global" "hive-mcp" "hive-mcp:agora" "proj-a"]}])

(def Tag
  [:string {:gen/elements ["scope:global" "scope:project:hive-mcp" "scope:project:proj-a"
                           "kanban" "axiom" "convention" "lint" "xpoll:project:a"
                           "ingestion-chunk" "a" "b"]}])

(def Tags
  "Tags as callers pass them: any sequence or set (the scope helpers skip
   non-string members), or nil. Generated as tag strings. Some stores answer
   :tags as one comma-joined string after an update; `EntryTags` admits it."
  [:maybe [:or {:gen/schema [:maybe [:or [:vector {:max 4} Tag] [:set {:max 3} Tag]]]}
           [:sequential :any] [:set :any]]])

(def EntryTags
  "An entry's :tags as a store answers it: Tags, or a comma-joined string."
  [:or Tags [:string {:gen/elements ["a,b" "scope:global"]}]])


(def ScopeTag
  [:string {:gen/elements ["scope:global" "scope:project:hive-mcp" "scope:project:x"]}])

(def ScopeTagSet
  [:set ScopeTag])

(def Entry
  "A memory entry as the store answers it. Open: stores add fields."
  [:map
   [:id {:optional true} [:maybe [:string {:gen/elements ["e1" "e2" "e3"]}]]]
   [:type {:optional true} [:maybe [:string {:gen/elements ["note" "decision" "convention" "axiom"]}]]]
   [:duration {:optional true} [:maybe DurationText]]
   [:tags {:optional true} EntryTags]

   [:content {:optional true} (loose [:maybe [:or [:string {:gen/max 40}] [:map [:title :string]]]])]
   [:project-id {:optional true} [:maybe ProjectIdText]]
   [:access-count {:optional true} [:maybe [:int {:min 0 :gen/max 9}]]]
   [:staleness-beta {:optional true} [:maybe [:and {:gen/schema [:double {:min 1.0 :max 5.0}]} number?]]]])


(def MaybeEntry
  [:maybe Entry])

(def Entries
  [:sequential Entry])

(def McpParams
  "An MCP handler params map. Open; each handler reads its own keys."
  [:map
   [:id {:optional true} (loose [:maybe [:string {:gen/elements ["e1" "e2" "nope"]}]])]
   [:directory {:optional true} (loose [:maybe [:string {:gen/elements ["/tmp/x" "/home/test/hive-mcp"]}]])]
   [:limit {:optional true} (loose [:maybe [:int {:min 1 :max 20}]])]
   [:dry_run {:optional true} (loose [:maybe :boolean])]])

(defn params
  "McpParams extended with one handler's own keys (a :map schema)."
  [extra]
  (mu/merge McpParams extra))

(m/=> params
      [:=> [:cat [:fn {:gen/elements [[:map [:x {:optional true} :int]]]} some?]]
       [:fn {:gen/elements [McpParams]} m/schema?]])


;; =============================================================================
;; Duration
;; =============================================================================

(def ShiftResult
  [:map {:closed true}
   [:new-duration DurationName]
   [:changed? :boolean]
   [:at-boundary? :boolean]])

(def TierWalk
  [:map {:closed true}
   [:final-duration [:maybe :string]]
   [:tiers-promoted nat-int?]])

;; =============================================================================
;; Decay / promotion
;; =============================================================================

(def DecayOpts
  [:map
   [:access-threshold {:optional true} [:int {:min 0 :gen/max 5}]]
   [:recency-days {:optional true} [:int {:min 0 :gen/max 30}]]])

(def DecayPlan
  [:map {:closed true}
   [:id any?] [:type any?] [:duration any?] [:access-count any?]
   [:old-beta number?] [:delta number?] [:new-beta number?]])

(def DecayApplied
  [:map {:closed true}
   [:id any?] [:delta number?] [:old-beta number?] [:new-beta number?]])

(def DecayCycle
  [:map
   [:decayed nat-int?]
   [:expired nat-int?]
   [:total-scanned nat-int?]
   [:error [:maybe :string]]
   [:cleanup-error {:optional true} [:maybe :string]]])

(def XpollPlan
  [:map {:closed true}
   [:id any?] [:type any?] [:duration any?]
   [:cross_projects [:vector any?]]
   [:cross_project_count nat-int?]
   [:tiers_to_promote nat-int?]
   [:next_duration [:maybe :string]]])

(def Promotion
  [:or
   [:map {:closed true} [:id any?] [:promoted [:= false]] [:duration any?]]
   [:map {:closed true}
    [:id any?] [:promoted [:= true]] [:old_duration any?]
    [:new_duration :string] [:tiers_promoted pos-int?]]])

(def XpollCycle
  [:map
   [:promoted nat-int?]
   [:candidates nat-int?]
   [:total-scanned nat-int?]
   [:entries {:optional true} [:vector :map]]
   [:error {:optional true} [:maybe :string]]])

(def CleanupResult
  [:map {:closed true}
   [:count nat-int?]
   [:deleted-ids [:vector any?]]
   [:repaired any?]])

;; =============================================================================
;; Format
;; =============================================================================

(def EntryMetadata
  [:map {:closed true}
   [:id any?] [:type any?] [:preview :string] [:tags any?] [:created any?]])

(def EntryReceipt
  "What a memory write answers: identity, status and new metadata, never the
   content. :preview is at most 80 chars plus an ellipsis."
  [:map {:closed true}
   [:id any?] [:status [:= "ok"]] [:type any?] [:tags any?]
   [:chars [:int {:min 0}]]
   [:preview [:string {:max 61}]]
   [:content-hash {:optional true} any?]
   [:duration {:optional true} any?]
   [:expires {:optional true} any?]
   [:project-id {:optional true} any?]])

(def PreviewLen
  "entry->metadata preview length. The body truncates the preview to
   (- len 3) and then cuts (- len 6) chars, so a length below 6 throws on a
   long enough content."
  [:int {:min 6 :gen/max 200}])

;; =============================================================================
;; Classify / gaps
;; =============================================================================

(def Level
  [:int {:min 1 :max 4}])

(def SignalLevel
  [:maybe [:enum 1 3 4]])

(def GapText
  [:string {:gen/elements ["What is this thing for?" "TODO: wire the adapter"
                           "FIXME later" "unclear: who owns it" "missing: tests"
                           "assuming a single writer" "plain prose." ""]}])

;; =============================================================================
;; Scope
;; =============================================================================

(def ScopeFilter
  "A scope filter as `matches-scope?` reads it: the ScopeFilter ADT, a scope
   tag set, or a legacy string/nil. The body accepts any value."
  (loose [:or domain/ScopeFilterMalli ScopeTagSet
          [:enum "all" "global" "hive-mcp" "scope:project:x"] :nil]))

(def ScopeMatch
  "`matches-scope?` / `matches-hierarchy-scopes?`: true, nil/false, or the
   matching tag (the body returns `some`'s value)."
  [:maybe [:or :boolean :string]])

(def DirectoryInput
  "`effective-directory` input. :directory is read through a string? check;
   :current is returned as given (callers pass the request directory)."
  [:map
   [:directory {:optional true} (loose [:maybe [:string {:gen/elements ["" "  " "/a" "/home/x"]}]])]
   [:current {:optional true} [:maybe [:string {:gen/elements ["/cwd" "/tmp"]}]]]])

;; =============================================================================
;; Reconcile / review
;; =============================================================================

(def ReconcileSources
  "An IReconcileSources port."
  [:fn {:gen/elements [(reify reconcile/IReconcileSources
                         (store-ids [_] ["a" "b"])
                         (vector-ids [_] ["b" "c"]))]
        :error/message "must satisfy IReconcileSources"}
   #(satisfies? reconcile/IReconcileSources %)])

(def ReconcileResponse
  [:map [:status [:enum "ok" "not-wired"]]])

(def QueueGroups
  [:map-of :string [:vector any?]])

;; =============================================================================
;; Search
;; =============================================================================

(def SearchHit
  [:map
   [:id {:optional true} any?]
   [:metadata {:optional true}
    [:map [:tags {:optional true}
           (loose [:maybe [:or [:string {:gen/elements ["a,b" "a" ""]}] [:vector Tag]]])]]]])

(def SearchPayload
  [:map {:closed true}
   [:results any?]
   [:count nat-int?]
   [:dropped-null-hits any?]
   [:query any?]
   [:scope any?]
   [:excluded-tags [:vector any?]]
   [:excludes-source [:enum :none :default :caller]]])

;; =============================================================================
;; Batch runner
;; =============================================================================

(def RunnerOpts
  "`make-memory-runner` opts: a handler tree and an fx emitter, both optional."
  [:maybe
   [:map
    [:handlers {:optional true} [:maybe [:map-of {:gen/max 0} :any :any]]]
    [:emit-fx {:optional true} [:maybe [:fn {:gen/elements [(fn [_ _] nil)]} ifn?]]]]])

(def RunOps
  [:sequential batch/RawOp])

(def RunOpts
  [:maybe [:map [:dry-run? {:optional true} [:maybe :boolean]]]])
