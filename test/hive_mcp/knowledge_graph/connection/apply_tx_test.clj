(ns hive-mcp.knowledge-graph.connection.apply-tx-test
  "Trifecta for `IWriteStrategy/apply-tx!` (hive-mcp.knowledge-graph.connection.strategy).

   The three strategies are driven against the real in-memory DataScript KG
   store the global fixture installs; nothing is redefined.

     golden    : what each strategy leaves behind for the same tx-data
     property  : BatchAccumulator keeps every datum in order; SyncWriter and
                 CoalescingWriter land every edge in the store
     mutation  : a strategy that drops or reorders the data is caught"
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.connection.strategy :as strategy]
            [hive-mcp.knowledge-graph.connection.writer :as writer]
            [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.protocols.kg :as kg-proto]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (reset! writer/in-flight 0)
    (try (fixtures/global-datascript-fixture f :sync-writes? true)
         (finally (reset! writer/in-flight 0)))))

(defn- edge [id] {:kg-edge/id id :kg-edge/from (str id "-a") :kg-edge/to (str id "-b")
                  :kg-edge/relation :implements :kg-edge/confidence 1.0})

(defn- stored-ids []
  (into (sorted-set) (map first)
        (conn/query '[:find ?id :where [_ :kg-edge/id ?id]])))

(def ^:private gen-ids
  (gen/fmap #(vec (distinct %))
            (gen/vector (gen/fmap #(str "e-" %) (gen/choose 0 999)) 0 6)))

(deftest-golden apply-tx-strategies-golden
  "test/golden/hive-mcp/knowledge_graph/apply-tx.edn"
  (let [store  (proto/get-store)
        batch  (atom [])
        _      (strategy/apply-tx! (strategy/->BatchAccumulator batch) [(edge "b1") (edge "b2")])
        _      (strategy/apply-tx! (strategy/->SyncWriter store) [(edge "s1")])
        _      (strategy/apply-tx! (strategy/->CoalescingWriter store) [(edge "c1")])
        flush  (writer/flush-pending!)]
    {:batch-ids   (mapv :kg-edge/id @batch)
     :stored      (vec (stored-ids))
     :flush       flush}))

(defspec batch-accumulator-keeps-every-datum-in-order 100
  (prop/for-all [ids gen-ids]
    (let [batch (atom [])]
      (strategy/apply-tx! (strategy/->BatchAccumulator batch) (mapv edge ids))
      (and (= ids (mapv :kg-edge/id @batch))
           (empty? (stored-ids))))))

(defspec sync-writer-lands-every-edge 50
  (prop/for-all [ids gen-ids]
    (conn/reset-conn!)
    (strategy/apply-tx! (strategy/->SyncWriter (proto/get-store)) (mapv edge ids))
    (= (set ids) (stored-ids))))

;; An EMPTY tx-data vector is enqueued but never flushed (flush-batch! skips an
;; empty batch, so in-flight is not decremented): flush-pending! then reports
;; :weave/timeout. Characterised below, not fixed here; the property therefore
;; draws non-empty data.
(deftest coalescing-writer-leaves-an-empty-tx-in-flight
  (strategy/apply-tx! (strategy/->CoalescingWriter (proto/get-store)) [])
  (try (is (= :weave/timeout (writer/flush-pending! 150)))
       (finally (reset! writer/in-flight 0))))

(defspec coalescing-writer-lands-every-edge 50
  (prop/for-all [ids (gen/such-that seq gen-ids)]
    (conn/reset-conn!)
    (strategy/apply-tx! (strategy/->CoalescingWriter (proto/get-store)) (mapv edge ids))
    (and (= :ok (writer/flush-pending!))
         (= (set ids) (stored-ids)))))

(deftest sync-writer-swallows-a-store-failure-into-nil
  (let [bad (reify kg-proto/IKGStore
              (transact! [_ _] (throw (ex-info "store down" {}))))]
    (is (nil? (strategy/apply-tx! (strategy/->SyncWriter bad) [(edge "x")])))))

(hmut/deftest-mutations apply-tx-batch-mutations
  hive-mcp.knowledge-graph.connection.strategy/select-strategy
  [["always sync"       (fn [_ _ ensure] (strategy/->SyncWriter (ensure)))]
   ["always coalescing" (fn [_ _ ensure] (strategy/->CoalescingWriter (ensure)))]]
  (fn []
    (let [batch (atom [])
          s     (strategy/select-strategy batch false (constantly (proto/get-store)))]
      (strategy/apply-tx! s [(edge "m1")])
      (is (= ["m1"] (mapv :kg-edge/id @batch)))
      (is (empty? (stored-ids))))))
