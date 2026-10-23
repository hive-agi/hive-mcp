(ns hive-mcp.knowledge-graph.queries-scoped-test
  "subgraph and find-orphan-nodes read the KG indexes, not every edge.

   Gate (card 20261021120723-4a239f16):
     1. On a fixture graph, queries/subgraph (scope-wide and node-bounded) and
        queries/find-orphan-nodes return exactly what the former
        get-all-edges -> filter implementations produced (kept below as the
        reference).
     2. A recording store shows how many edge entities each path pulls,
        before and after (no with-redefs, no clocks)."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.connection.store :as cstore]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.edges.scoped :as scoped]
            [hive-mcp.knowledge-graph.queries :as queries]
            [hive-mcp.knowledge-graph.scope :as scope]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.knowledge-graph.store.harness :as harness]
            [hive-spi.kg.protocol :as spi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Recording store: a DataScript delegate that records which entities are pulled
;; =============================================================================

(defrecord RecordingStore [delegate pulled]
  spi/IKGStore
  (ensure-conn! [_] (spi/ensure-conn! delegate))
  (db-snapshot [_] (spi/db-snapshot delegate))
  (reset-conn! [_] (spi/reset-conn! delegate))
  (close! [_] (spi/close! delegate))
  (transact! [_ tx] (spi/transact! delegate tx))
  (entid [_ lookup] (spi/entid delegate lookup))
  (entity [_ eid] (spi/entity delegate eid))
  (eids-by-attr [_ attr] (spi/eids-by-attr delegate attr))
  (query [_ q] (spi/query delegate q))
  (query [_ q inputs] (spi/query delegate q inputs))
  (pull-entity [_ pattern eid]
    (swap! pulled conj eid)
    (spi/pull-entity delegate pattern eid)))

(defrecord RecordingStoreFactory []
  harness/StoreFactory
  (available? [_] true)
  (create [_]
    (let [ds (ds-store/create-store)]
      (spi/ensure-conn! ds)
      (harness/->EphemeralStore (->RecordingStore ds (atom #{})) nil)))
  (dispose! [_ ephemeral] (spi/reset-conn! (:delegate (:store ephemeral)))))

(use-fixtures :each
  (fn [f]
    (harness/with-disposable-store
      (->RecordingStoreFactory)
      (harness/thread-local-isolation :sync-writes? true)
      f)))

(defn- pulled-by
  "Distinct entities pulled while running thunk."
  [thunk]
  (let [pulled (:pulled cstore/*test-store*)]
    (reset! pulled #{})
    (thunk)
    (count @pulled)))

;; =============================================================================
;; Fixture: a binary tree of edges over four scope classes, plus a self-loop
;; =============================================================================

(def ^:private n-edges 2000)

(defn- scope-of [i]
  (case (int (mod i 4))
    0 (scope/scope->tag "proj-a")
    1 (scope/scope->tag "proj-b")
    2 nil
    3 "scope:global"))

(defn- edge-tx [i]
  (cond-> {:kg-edge/id         (format "edge-%05d" i)
           :kg-edge/from       (str "n" (quot (dec i) 2))
           :kg-edge/to         (str "n" i)
           :kg-edge/relation   (if (even? i) :implements :depends-on)
           :kg-edge/confidence 0.8}
    (scope-of i) (assoc :kg-edge/scope (scope-of i))))

(defn- seed! []
  (doseq [batch (partition-all 500 (range 1 (inc n-edges)))]
    (conn/transact! (mapv edge-tx batch)))
  (conn/transact! [{:kg-edge/id "edge-loop" :kg-edge/from "n3" :kg-edge/to "n3"
                    :kg-edge/relation :refines :kg-edge/scope (scope/scope->tag "proj-a")}]))

;; =============================================================================
;; Reference: the former implementations, over every edge
;; =============================================================================

(defn- in-scope? [edge scope-set]
  (or (nil? (:kg-edge/scope edge)) (contains? scope-set (:kg-edge/scope edge))))

(defn- old-subgraph [s]
  (let [scope-set (set (scope/visible-scope-tags s))
        visible   (filter #(in-scope? % scope-set) (edges/get-all-edges))
        nodes     (set (concat (map :kg-edge/from visible) (map :kg-edge/to visible)))]
    {:nodes nodes :edges (vec visible)}))

(defn- old-neighborhood
  "Every edge of the old scope subgraph incident to a node at undirected
   distance < max-depth from node-id."
  [s node-id max-depth]
  (let [visible (:edges (old-subgraph s))
        adj     (reduce (fn [m {:kg-edge/keys [from to]}]
                          (-> m (update from (fnil conj #{}) to)
                              (update to (fnil conj #{}) from)))
                        {} visible)
        within  (if (zero? max-depth)
                  #{}
                  (loop [frontier #{node-id} reached #{node-id} d 1]
                    (if (or (empty? frontier) (>= d max-depth))
                      reached
                      (let [nxt (set/difference (set (mapcat adj frontier)) reached)]
                        (recur nxt (into reached nxt) (inc d))))))]
    (filterv #(or (within (:kg-edge/from %)) (within (:kg-edge/to %))) visible)))

(defn- old-orphans []
  (let [all (edges/get-all-edges)]
    (set (keys (filter #(= 1 (val %))
                       (frequencies (concat (map :kg-edge/from all)
                                            (map :kg-edge/to all))))))))

(defn- shape [{:keys [nodes edges]}] {:nodes (set nodes) :edges (vec edges)})

;; =============================================================================
;; Gate 1: equivalence
;; =============================================================================

(deftest subgraph-by-scope-equals-old-full-scan
  (seed!)
  (doseq [s ["proj-a" "proj-b" "unknown-proj" nil]]
    (testing (pr-str s)
      (let [new (queries/subgraph s)]
        (is (= (old-subgraph s) (shape new)))
        (is (= (count (:edges new)) (:edge-count new)))
        (is (= (count (:nodes new)) (:node-count new)))))))

(deftest subgraph-by-node-equals-old-bfs
  (seed!)
  (doseq [[s node depth] [["proj-a" "n0" 3] ["proj-b" "n0" 4] ["proj-a" "n3" 2]
                          ["proj-a" "n1" 0] ["proj-b" "n999" 2] ["proj-a" "absent" 3]]]
    (testing (pr-str [s node depth])
      (let [new (queries/subgraph s {:node-id node :max-depth depth})]
        (is (= (old-neighborhood s node depth) (:edges new)))
        (is (= (set (mapcat (juxt :kg-edge/from :kg-edge/to) (:edges new)))
               (set (:nodes new))))))))

(deftest find-orphan-nodes-equals-old-frequencies
  (seed!)
  (let [orphans (queries/find-orphan-nodes)]
    (is (= (old-orphans) orphans))
    (is (contains? orphans (str "n" n-edges)) "a tree leaf is an orphan")
    (is (not (contains? orphans "n3")) "the self-loop node is not")))

(deftest empty-graph
  (is (= {:nodes [] :edges [] :node-count 0 :edge-count 0} (queries/subgraph "proj-a")))
  (is (= [] (scoped/neighborhood-edges "n0" {:scope-set nil :max-depth 3})))
  (is (= #{} (queries/find-orphan-nodes))))

;; =============================================================================
;; Gate 2: entities pulled, before and after
;; =============================================================================

(deftest indexed-paths-pull-only-what-they-return
  (seed!)
  (let [total (inc n-edges)]
    (testing "scope subgraph: old pulls every edge, new pulls the visible ones"
      (let [before  (pulled-by #(old-subgraph "proj-b"))
            visible (count (:edges (queries/subgraph "proj-b")))
            after   (pulled-by #(queries/subgraph "proj-b"))]
        (is (= total before))
        (is (= visible after))
        (is (< after total))))
    (testing "node subgraph: pulls only the edges met within the hop bound"
      (let [after (pulled-by #(queries/subgraph "proj-a" {:node-id "n0" :max-depth 3}))]
        (is (<= after 15)
            "2+4+8 tree edges incident to depth < 3, plus the n3 self-loop")))
    (testing "orphans: old pulls every edge, new pulls none"
      (is (= total (pulled-by old-orphans)))
      (is (zero? (pulled-by queries/find-orphan-nodes))))))

(deftest edges-by-scope-limit-pulls-at-most-limit
  (seed!)
  (let [tag   (scope/scope->tag "proj-b")
        full  (edges/get-edges-by-scope tag)
        limit 25]
    (testing "the unbounded arity is unchanged"
      (is (= 500 (count full)))
      (is (= full (edges/get-edges-by-scope tag nil)))
      (is (= full (edges/get-edges-by-scope tag {}))))
    (testing "a limited read pulls at most :limit entities and returns that many"
      (let [rows   (atom nil)
            pulled (pulled-by #(reset! rows (edges/get-edges-by-scope tag {:limit limit})))]
        (is (= limit (count @rows)))
        (is (<= pulled limit))
        (is (every? #(= tag (:kg-edge/scope %)) @rows))
        (is (= (count (distinct (map :kg-edge/id @rows))) limit))
        (is (every? (set (map :kg-edge/id full)) (map :kg-edge/id @rows)))))
    (testing "a limit past the scope's size answers the whole scope"
      (is (= (set (map :kg-edge/id full))
             (set (map :kg-edge/id (edges/get-edges-by-scope tag {:limit 100000}))))))
    (testing "an unknown scope answers empty without pulling"
      (is (zero? (pulled-by #(is (= [] (edges/get-edges-by-scope "nope" {:limit 10})))))))))
