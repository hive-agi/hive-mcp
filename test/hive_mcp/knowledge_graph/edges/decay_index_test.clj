(ns hive-mcp.knowledge-graph.edges.decay-index-test
  "The hourly edge decay reads the :kg-edge/last-verified index.

   Gate (card 20261021120722-3a73c4a4):
     1. get-oldest-verified-edges returns exactly what the old
        fetch-every-edge -> sort -> take produced, on a stub store.
     2. A decay cycle on a 10k-edge fixture touches O(limit) entities,
        measured by a recording store (no with-redefs, no clocks)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.connection.avet :as avet]
            [hive-mcp.knowledge-graph.connection.store :as cstore]
            [hive-mcp.knowledge-graph.edges.decay :as decay]
            [hive-mcp.knowledge-graph.edges.queries :as queries]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.knowledge-graph.store.harness :as harness]
            [hive-spi.kg.protocol :as spi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Recording store: a DataScript delegate that counts what a read touches
;; =============================================================================

(defrecord RecordingStore [delegate counts]
  spi/IKGStore
  (ensure-conn! [_] (spi/ensure-conn! delegate))
  (db-snapshot [_] (spi/db-snapshot delegate))
  (reset-conn! [_] (spi/reset-conn! delegate))
  (close! [_] (spi/close! delegate))
  (transact! [_ tx] (spi/transact! delegate tx))
  (entid [_ lookup] (spi/entid delegate lookup))
  (entity [_ eid] (swap! counts update :entity inc) (spi/entity delegate eid))
  (eids-by-attr [_ attr]
    (swap! counts update :eids-by-attr inc)
    (spi/eids-by-attr delegate attr))
  (query [_ q] (swap! counts update :query inc) (spi/query delegate q))
  (query [_ q inputs] (swap! counts update :query inc) (spi/query delegate q inputs))
  (pull-entity [_ pattern eid]
    (swap! counts update :pulled conj eid)
    (spi/pull-entity delegate pattern eid))

  avet/IAttrValueIndex
  (attr-values-ascending [_ attr]
    (map (fn [pair] (swap! counts update :index-datoms inc) pair)
         (avet/attr-values-ascending delegate attr))))

(def ^:private zero-counts
  {:entity 0 :eids-by-attr 0 :index-datoms 0 :query 0 :pulled #{}})

(defrecord RecordingStoreFactory []
  harness/StoreFactory
  (available? [_] true)
  (create [_]
    (let [ds (ds-store/create-store)]
      (spi/ensure-conn! ds)
      (harness/->EphemeralStore (->RecordingStore ds (atom zero-counts)) nil)))
  (dispose! [_ ephemeral] (spi/reset-conn! (:delegate (:store ephemeral)))))

(use-fixtures :each
  (fn [f]
    (harness/with-disposable-store
      (->RecordingStoreFactory)
      (harness/thread-local-isolation :sync-writes? true)
      f)))

(defn- reset-counts! [] (reset! (:counts cstore/*test-store*) zero-counts))
(defn- counts [] @(:counts cstore/*test-store*))

;; =============================================================================
;; Fixture data: deterministic, no wall clock
;; =============================================================================

(def ^:private day-ms (* 24 60 60 1000))
(def ^:private epoch-ms 1800000000000)

(defn- edge-tx
  "Edge i verified `age-days(i)` days before epoch-ms. Ages repeat so ties
   exist and must break the same way in both paths."
  [i]
  {:kg-edge/id            (format "edge-%05d" i)
   :kg-edge/from          (str "n" i)
   :kg-edge/to            (str "n" (inc i))
   :kg-edge/relation      (if (even? i) :co-accessed :depends-on)
   :kg-edge/scope         (if (zero? (mod i 3)) "proj-a" "proj-b")
   :kg-edge/confidence    0.8
   :kg-edge/created-at    (java.util.Date. (long epoch-ms))
   :kg-edge/last-verified (java.util.Date.
                           (long (- epoch-ms (* day-ms (mod (* i 7919) 400)))))})

(defn- seed-edges! [n]
  (doseq [batch (partition-all 1000 (range n))]
    (conn/transact! (mapv edge-tx batch))))

(defn- old-candidates
  "The pre-index candidate set: every edge pulled, filtered, sorted, taken
   (the former decay :fetch + edge-cycle sort/limit)."
  [{:keys [scope limit]}]
  (let [all (cond->> (queries/get-all-edges)
              scope (filter #(= scope (:kg-edge/scope %))))]
    (vec (take limit (sort-by decay/last-verified-millis all)))))

;; =============================================================================
;; Gate 1: same candidates as the old full sort
;; =============================================================================

(deftest oldest-verified-equals-old-full-sort
  (seed-edges! 2000)
  (doseq [opts [{:limit 100}
                {:limit 1}
                {:limit 7 :scope "proj-a"}
                {:limit 100 :scope "proj-b"}
                {:limit 5000}]]
    (testing (pr-str opts)
      (is (= (old-candidates opts)
             (queries/get-oldest-verified-edges opts))))))

(deftest oldest-verified-on-empty-graph
  (is (= [] (queries/get-oldest-verified-edges {:limit 100}))))

;; =============================================================================
;; Gate 2: a decay cycle on 10k edges touches O(limit) entities
;; =============================================================================

(deftest decay-cycle-on-10k-edges-touches-o-limit-entities
  (seed-edges! 10000)
  (let [limit 100]
    (testing "before: the old fetch pulls every edge"
      (reset-counts!)
      (old-candidates {:limit limit})
      (is (= 10000 (count (:pulled (counts))))))
    (testing "after: the decay cycle reads the index and pulls only the chosen edges"
      (reset-counts!)
      (let [tally (decay/decay-unverified-edges! {:limit limit})
            c     (counts)]
        (is (= limit (:evaluated tally)))
        (is (= limit (count (:pulled c))) "only the chosen edges are pulled")
        ;; Lazy seq layers realize ahead in 32-datom chunks, a constant
        ;; overshoot independent of graph size.
        (is (<= (:index-datoms c) (* 3 limit))
            "the index walk stops near limit")
        (is (< (:index-datoms c) (quot 10000 10))
            "the index walk is nowhere near the whole graph")
        (is (zero? (:eids-by-attr c)) "no full :kg-edge/id enumeration")
        (is (zero? (:query c)) "no Datalog scan")))))

(deftest unindexed-attribute-falls-back-to-a-sorted-read
  (testing "a database whose attribute carries no :db/index (a DB created before
            the index existed) still answers every datom in value order, rather
            than an empty walk of an AVET index that holds nothing for it"
    (let [ds (ds-store/create-store)]
      (spi/ensure-conn! ds)
      (spi/transact! ds [{:test/rank 3} {:test/rank 1} {:test/rank 2}])
      (is (= [1 2 3] (map second (avet/attr-values-ascending ds :test/rank)))))))
