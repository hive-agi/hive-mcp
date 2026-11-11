(ns hive-mcp.memory.reconcile
  "Vector-index vs entry-store reconciliation: the pure core.

   A memory entry lives in two places, the entry store (the source of truth)
   and the vector index (a derived projection used by semantic search). When
   the two drift, search surfaces ids the store no longer holds (an orphan
   vector, the id:null / missing-id hit of the 2026-07-23 restart loss) or
   the store holds entries search can never find (an unindexed entry).

   This namespace is the CPPB core of the reconciliation job and nothing
   else: `diff-ids` is a pure function over two id collections and `report`
   is its public shape. The boundary is the `IReconcileSources` port, which
   answers the two id listings; an adapter backed by the live stores is a
   separate slice. Nothing here writes to a store, so a report can never
   change what it measures. Purging orphan vectors goes through the separate
   `IOrphanVectorPurge` port and `purge-plan`; the verb in
   hive-mcp.tools.memory.reconcile gates it behind purge + i_mean_it.

   Ids are compared as strings; nil and blank ids are not ids. A nil vector
   id is counted under :nil-vector-ids so it stays visible instead of being
   silently dropped."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private -reconcile-sources-defined? (atom false))

(when (compare-and-set! -reconcile-sources-defined? false true)
  (defprotocol IReconcileSources
    "The two id listings reconciliation compares."
    (store-ids [this]
      "Ids of every entry in the entry store (the source of truth).")
    (vector-ids [this]
      "Ids of every record in the vector index, nil ids included.")))

(defonce ^:private -orphan-vector-purge-defined? (atom false))

(when (compare-and-set! -orphan-vector-purge-defined? false true)
  (defprotocol IOrphanVectorPurge
    "Optional and vector-side only: delete vector-index records by id. A
     sources value whose vector index is separate from the entry store
     implements it next to IReconcileSources. It never deletes an
     entry-store row: the caller only passes ids the entry store does not
     hold (`purge-plan`)."
    (purge-vector-ids! [this ids]
      "Delete the vector-index records whose ids are IDS (strings). Answers
       how many were deleted.")))

(defonce ^:private -listing-completeness-defined? (atom false))

(when (compare-and-set! -listing-completeness-defined? false true)
  (defprotocol IListingCompleteness
    "Optional: lets a sources value say POSITIVELY that its listings were
     exhaustive. A purge needs that answer and fails closed without it:
     sources that do not implement this port are refused, because a listing
     that cannot vouch for its own end may be a cut one, and every id past
     the cut would read as an orphan."
    (listings-complete? [this]
      "True only when the most recent `store-ids` read AND the most recent
       `vector-ids` read both reached the end of their listing. False before
       either was read, when either was cut, and whenever it is not known.")))

(defn- id-string
  "The id as a non-blank string, or nil when it is not an id."
  [id]
  (when (some? id)
    (let [s (str id)]
      (when-not (= "" (.trim ^String s)) s))))

(defn- id-set
  "The set of real ids in COLL."
  [coll]
  (into #{} (keep id-string) coll))

(def IdColl
  "An id listing: any collection of values, compared as strings; nil and
   blank ones are not ids."
  [:maybe [:or [:sequential :any] [:set :any]]])

(def DiffReport
  "What `diff-ids` and `report` answer."
  [:map
   [:orphan-vector-ids [:vector [:string {:min 1}]]]
   [:unindexed-store-ids [:vector [:string {:min 1}]]]
   [:nil-vector-ids [:int {:min 0}]]
   [:store-count [:int {:min 0}]]
   [:vector-count [:int {:min 0}]]
   [:consistent-count [:int {:min 0}]]
   [:consistent? :boolean]])

(def ReconcileSources
  "An IReconcileSources port."
  [:fn {:error/message "must satisfy IReconcileSources"
        :gen/elements [(reify IReconcileSources
                         (store-ids [_] ["a" "lost"])
                         (vector-ids [_] ["a" "ghost" nil]))]}
   #(satisfies? IReconcileSources %)])

(def TruncatableReport
  "A report as `truncate-report` reads it: the two id listings, either may
   be absent."
  [:map
   [:orphan-vector-ids {:optional true} [:maybe [:sequential :any]]]
   [:unindexed-store-ids {:optional true} [:maybe [:sequential :any]]]])

(def TruncatedReport
  "REPORT with its listings capped and the full sizes kept."
  [:map
   [:orphan-vector-ids [:vector :any]]
   [:unindexed-store-ids [:vector :any]]
   [:orphan-count [:int {:min 0}]]
   [:unindexed-count [:int {:min 0}]]
   [:truncated? :boolean]])

(defn diff-ids
  "Compare the entry-store ids with the vector-index ids. Pure.

   Answers a map:
     :orphan-vector-ids   sorted ids in the index and absent from the store
     :unindexed-store-ids sorted ids in the store and absent from the index
     :nil-vector-ids      count of index records carrying no usable id
     :store-count         distinct store ids
     :vector-count        distinct usable vector ids
     :consistent-count    ids present on both sides
     :consistent?         true when no orphan, unindexed or nil id exists"
  [store-id-coll vector-id-coll]
  (let [store   (id-set store-id-coll)
        vectors (id-set vector-id-coll)
        orphans (vec (sort (remove store vectors)))
        missing (vec (sort (remove vectors store)))
        nils    (count (remove id-string vector-id-coll))]
    {:orphan-vector-ids   orphans
     :unindexed-store-ids missing
     :nil-vector-ids      nils
     :store-count         (count store)
     :vector-count        (count vectors)
     :consistent-count    (- (count store) (count missing))
     :consistent?         (and (empty? orphans) (empty? missing) (zero? nils))}))

(m/=> diff-ids [:=> [:cat IdColl IdColl] DiffReport])

(defn report
  "Read both listings from SOURCES (an IReconcileSources) and diff them.
   Reads only: SOURCES is asked for ids and never written to."
  [sources]
  (diff-ids (store-ids sources) (vector-ids sources)))

(m/=> report [:=> [:cat ReconcileSources] DiffReport])

(defn truncate-report
  "Cap the two id listings of REPORT at LIMIT entries each, so a badly
   drifted index cannot produce an unbounded response. The full sizes stay
   in :orphan-count and :unindexed-count and :truncated? says whether
   anything was cut. Pure."
  [report limit]
  (let [orphans (:orphan-vector-ids report)
        missing (:unindexed-store-ids report)]
    (assoc report
           :orphan-count    (count orphans)
           :unindexed-count (count missing)
           :orphan-vector-ids   (vec (take limit orphans))
           :unindexed-store-ids (vec (take limit missing))
           :truncated? (boolean (or (> (count orphans) limit)
                                    (> (count missing) limit))))))

(m/=> truncate-report [:=> [:cat TruncatableReport :int] TruncatedReport])

(def max-purge-fraction
  "The largest share of the distinct usable vector ids one purge may delete.
   A plan above it is refused (`purge-refusal`): orphaning more than half of
   the index at once is far more likely a faulty entry-store listing (a
   swallowed error, a disconnected or half-loaded store) than real drift. A
   real mass drift is repaired by rebuilding the index, not by this purge."
  1/2)

(defn purge-refusal
  "Why a purge of PLAN must not run, as a reason string, or nil when it may.
   Pure. STORE-ID-COLL is the entry-store listing PLAN was derived from and
   VECTOR-ID-COLL the vector-index listing.

   Refuses when PLAN is not empty and either
     the store listing holds no usable id (an empty source of truth makes
     every vector an orphan; that is a listing fault until proven otherwise),
     or PLAN is more than `max-purge-fraction` of the distinct usable
     vector ids."
  [store-id-coll vector-id-coll plan]
  (let [plan-n   (count (id-set plan))
        store-n  (count (id-set store-id-coll))
        vector-n (count (id-set vector-id-coll))]
    (cond
      (zero? plan-n)
      nil

      (zero? store-n)
      (str "the entry-store listing is empty while " plan-n
           " vector id(s) would be purged; an empty source of truth is treated"
           " as a listing fault; nothing deleted")

      (> plan-n (* max-purge-fraction vector-n))
      (str "the purge would delete " plan-n " of " vector-n
           " vector ids, more than the safety cap of "
           (long (* 100 max-purge-fraction))
           " percent; a mass orphaning is treated as a listing fault;"
           " nothing deleted")

      :else nil)))

(m/=> purge-refusal [:=> [:cat IdColl IdColl IdColl] [:maybe :string]])

(defn purge-plan
  "The ids a purge may delete: those of CANDIDATE-IDS absent from
   STORE-ID-COLL, as sorted strings. Pure. Set difference, so an id the
   entry store holds is never in the plan. The purge path calls it with a
   FRESH store listing read just before the delete, so an entry written
   between the report and the purge is kept."
  [store-id-coll candidate-ids]
  (:orphan-vector-ids (diff-ids store-id-coll candidate-ids)))

(m/=> purge-plan [:=> [:cat IdColl IdColl] [:vector [:string {:min 1}]]])
