(ns hive-mcp.tools.catchup.shapes
  "Entry shape contracts for the catchup pipeline.

   Three shapes flow through catchup → enrichment → synthesis:

   1. Raw store entry: {:id :content :type :tags ...}
      Source: IMemoryStore queries in catchup.clj / scope.clj
      Used by: synthesis LLM prompt, axiom/priority piggyback

   2. Catchup meta: {:id :T :P}
      Source: entry->catchup-meta (token-minimized for wire)
      Used by: catchup response blocks, context piggyback

   3. Enriched meta: {:id :T :P :kg}
      Source: KG enrichment adds :kg relations
      Used by: context piggyback blocks, KG insights

   Coercion functions ensure consumers can extract content
   regardless of which shape they receive."
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn entry-content
  "Extract content string from any entry shape.
   Checks :content (raw store), :preview, :P (catchup meta) in order.
   Returns empty string if none found."
  [entry]
  (str (or (:content entry) (:preview entry) (:P entry) "")))

(defn entry-id
  "Extract entry ID, defaulting to \"?\"."
  [entry]
  (or (:id entry) "?"))

(defn entry-type-name
  "Extract type as string from any entry shape.
   Handles :type (raw keyword) and :T (catchup meta string)."
  [entry]
  (let [t (or (:type entry) (:T entry))]
    (if (keyword? t) (name t) (str (or t "note")))))

;; =============================================================================
;; Value schemas shared by the catchup namespaces
;; =============================================================================
;;
;; Entries are OPEN maps: a store row, a catchup meta {:id :T :P} and an
;; enriched meta all flow through the same functions, so the schemas say only
;; what a reader may rely on. The :gen/schema properties draw generated values
;; from a small catchup vocabulary so properties reach the interesting
;; branches (scope tags, priority tags, axiom types) instead of random noise.

(def Tag
  "One memory tag. Tags are strings on every store path; the readers in this
   pipeline coerce with `str` or compare by value, so any value is accepted."
  [:any {:gen/schema [:enum "catchup-priority" "session-summary" "wrap-generated"
                      "scope:project:p" "scope:project:other" "scope:global"
                      "axiom" "agent:ling-1" "clojure" "hive-mcp" "malli-schema"]}])

(def Tags
  "A tag collection as entries carry it: a vector, a seq or a set, or nil."
  [:maybe [:or [:sequential Tag] [:set Tag]]])

(def EntryId
  "An entry id as the stores hand it out."
  [:string {:gen/schema [:enum "e1" "e2" "e3" "e4" "e5" "e6"]}])

(def Entry
  "Any entry shape catchup handles (raw store row, catchup meta, enriched
   meta). Open: only the keys every reader treats alike are typed. :type is
   a keyword on the raw store path and a string elsewhere; readers call
   `name` on it. :tags is a collection."
  [:map {:gen/schema
         [:map
          [:id {:optional true} EntryId]
          [:type {:optional true} [:enum :axiom :decision :convention :note :principle
                                   "axiom" "convention" "note"]]
          [:tags {:optional true} [:vector {:gen/max 4} Tag]]
          [:content {:optional true} :string]
          [:created {:optional true} [:enum "2026-01-01T00:00:00Z" "2026-02-01T00:00:00Z"
                                      "2026-03-01T00:00:00Z"]]
          [:project-id {:optional true} [:enum "p" "other" "global"]]]}
   [:type {:optional true} [:maybe [:or :keyword :string]]]
   [:tags {:optional true} Tags]])

(def Entries
  "A collection of entries as the pipeline passes them: vector, lazy seq or nil."
  [:maybe [:sequential Entry]])

(def ProjectId
  "A project id, \"global\" or nil (nil reads as global)."
  [:maybe [:string {:gen/schema [:enum "p" "other" "global"]}]])

(def Buckets
  "A catchup bundle: bucket keyword -> the entries selected for it
   (`bundle/split-by-type`, `hydration/hydrate-buckets`)."
  [:map-of {:gen/schema [:map-of [:enum :axioms :decisions :conventions :sessions]
                         [:vector {:gen/max 3} Entry]]}
   :keyword [:sequential Entry]])

(def MemoryWrite
  "One write as `write-events` hands it to a listener: {:op :id :memory-type
   :tags ...}. Every key is optional; :memory-type is read with `name`."
  [:map {:gen/schema
         [:map
          [:op [:enum :added :updated :deleted]]
          [:id {:optional true} EntryId]
          [:memory-type {:optional true} [:enum "decision" "axiom" "convention" "kanban" :note]]
          [:tags {:optional true} [:vector {:gen/max 3} [:enum "kanban" "decision" "other"]]]]}
   [:memory-type {:optional true} [:maybe [:or :string :keyword]]]
   [:tags {:optional true} Tags]])

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> entry-content [:=> [:cat [:maybe Entry]] :string])
(m/=> entry-id [:=> [:cat [:maybe Entry]] :some])
(m/=> entry-type-name [:=> [:cat [:maybe Entry]] :string])
