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
   change what it measures. Purging orphan vectors is a follow-up that
   consumes `:orphan-vector-ids` from this report.

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
