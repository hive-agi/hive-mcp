(ns hive-mcp.knowledge-graph.edges.schema
  "Malli value objects for KG edges, shared by the function contracts of the
   edges.* namespaces.

   An Edge is what the backends PULL: legacy rows are read back as they were
   written, so ids are plain strings and the relation may be a keyword or a
   string. A write goes through `edge-tx-data`, which validates."
  (:require [hive-mcp.knowledge-graph.disc.schema :as ds]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Date ds/Date)

(def Instant ds/Instant)

(def Count ds/Count)

(def EdgeId
  "An edge id: a random UUID string."
  [:re {:gen/schema :uuid :gen/fmap str}
   #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"])

(def Scope
  "An optional scope filter; nil means every scope."
  [:maybe :string])

(def Relation
  "A relation as stored: a keyword, or a string on legacy rows."
  [:or :keyword :string])

(def Confidence
  "An edge confidence score in [0, 1]."
  [:and number? [:>= 0] [:<= 1]])

(def Edge
  "An edge entity as a backend pulls it."
  [:map
   [:db/id {:optional true} :int]
   [:kg-edge/id :string]
   [:kg-edge/from :string]
   [:kg-edge/to :string]
   [:kg-edge/relation Relation]
   [:kg-edge/confidence {:optional true} number?]
   [:kg-edge/scope {:optional true} :string]
   [:kg-edge/created-at {:optional true} Date]
   [:kg-edge/last-verified {:optional true} :any]
   [:kg-edge/source-type {:optional true} :keyword]
   [:kg-edge/created-by {:optional true} :string]
   [:kg-edge/predicate {:optional true} :string]])

(def Edges
  "A sequence of edges."
  [:sequential Edge])

(def NodeIds
  "A collection of node ids for the batch readers and record-co-access!: a
   vector or list, or an empty set.

   BUG (recorded, not fixed): each of these calls `(distinct node-ids)`, and
   clojure.core/distinct destructures its collection with nth, so a NON-EMPTY
   set throws UnsupportedOperationException (nth not supported on this type:
   PersistentHashSet), e.g. (batch-get-edges-from #{\"n1\"}). An empty set
   short-circuits before distinct and returns {}. The schema states that
   current domain."
  [:maybe [:or [:sequential :string] [:set {:max 0} :string]]])

(def EdgesByNode
  "Edges grouped by the node id they hang off."
  [:map-of :string [:vector Edge]])

(def Neighbor
  "A co-access neighbor and the strength of the co-access."
  [:map [:entry-id :string] [:confidence number?]])

(def Arc
  "An edge projected to its arc."
  [:map [:from :string] [:to :string] [:relation Relation] [:confidence number?]])

(def EdgeSpec
  "What add-edge! takes. Open on purpose: `edge-tx-data` validates every key
   and throws ex-info on a bad one, so malformed specs are in its domain."
  [:maybe :map])

(def EdgeSpecs
  "A batch of edge specs, as a vector, list or set."
  [:maybe [:or [:sequential EdgeSpec] [:set EdgeSpec]]])

(def EdgeTx
  "The validated transaction map of one edge."
  [:map
   [:kg-edge/id EdgeId]
   [:kg-edge/from :string]
   [:kg-edge/to :string]
   [:kg-edge/relation :keyword]
   [:kg-edge/confidence Confidence]
   [:kg-edge/created-at Date]
   [:kg-edge/last-verified :some]
   [:kg-edge/scope {:optional true} :some]
   [:kg-edge/created-by {:optional true} :some]
   [:kg-edge/source-type {:optional true} :keyword]
   [:kg-edge/predicate {:optional true} :string]])

(def TransactFn
  "A transaction fn: conn/transact! or conn/transact-sync!."
  [:fn {:error/message "should be a transaction fn"
        :gen/elements [identity (constantly nil)]}
   ifn?])

(def Thunk
  "A zero-arg fn."
  [:fn {:error/message "should be a fn"
        :gen/elements [(constantly nil) (constantly :fallback)]}
   ifn?])

(def CycleOutcome
  "The tally edge-cycle/run-cycle! returns, without its outcome keys."
  [:map [:evaluated Count] [:errors Count]])

(def DecayTally
  "What decay-unverified-edges! returns."
  [:map [:evaluated Count] [:errors Count]
   [:decayed Count] [:pruned Count] [:fresh Count]])

(def PromotionTally
  "What promote-co-access-edges! returns."
  [:map [:evaluated Count] [:errors Count]
   [:promoted Count] [:skipped Count] [:below Count]])

(def DecayOpts
  "The optional opts map of decay-unverified-edges!."
  [:maybe [:map
           [:staleness-days {:optional true} number?]
           [:limit {:optional true} :int]
           [:scope {:optional true} Scope]
           [:created-by {:optional true} [:maybe :string]]]])

(def PromotionOpts
  "The optional opts map of promote-co-access-edges!."
  [:maybe [:map
           [:threshold {:optional true} number?]
           [:confidence {:optional true} number?]
           [:limit {:optional true} :int]
           [:scope {:optional true} Scope]
           [:created-by {:optional true} [:maybe :string]]]])

(def CoAccessOpts
  "The optional opts map of record-co-access!."
  [:maybe [:map
           [:scope {:optional true} Scope]
           [:created-by {:optional true} [:maybe :string]]]])

(def AllEdgesOpts
  "The opts map of get-all-edges."
  [:maybe [:map [:batch-size {:optional true} number?]]])

(def SinceOpts
  "The trailing options of get-edges-since, as key/value pairs or one map."
  [:alt
   [:* [:alt
        [:cat [:= :scope] Scope]
        [:cat [:= :limit] :int]
        [:cat [:= :exclude-relations] [:maybe [:or [:set :keyword] [:sequential :keyword]]]]]]
   [:cat [:maybe [:map
                  [:scope {:optional true} Scope]
                  [:limit {:optional true} :int]
                  [:exclude-relations {:optional true}
                   [:maybe [:or [:set :keyword] [:sequential :keyword]]]]]]]])

(def ScopeMigration
  "What migrate-edge-scopes! returns."
  [:map [:migrated Count] [:old-scope :string] [:new-scope :string]])

(def StatsCache
  "The edge-stats cache state the delta fns swap in."
  [:map
   [:initialized? :boolean]
   [:scanned? :boolean]
   [:scan-delay [:maybe [:fn {:gen/elements [(delay :ok)]} delay?]]]
   [:total-edges Count]
   [:by-relation [:map-of :any [:int {:min 1}]]]
   [:by-scope [:map-of :any [:int {:min 1}]]]])

(def StatsSnapshot
  "What edges.stats/snapshot returns."
  [:map
   [:total-edges Count]
   [:by-relation [:map-of :any [:int {:min 1}]]]
   [:by-scope [:map-of :any [:int {:min 1}]]]])

(def EventId
  "A hive-events event id."
  :keyword)
