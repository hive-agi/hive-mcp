(ns hive-mcp.knowledge-graph.edges.add-edges-sync-test
  "add-edges-sync! writes a batch in one synchronous transaction and never
   waits on the coalescing writer's backlog (no flush-pending! barrier)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.connection.writer :as writer]
            [hive-mcp.knowledge-graph.edges.queries :as q]
            [hive-mcp.knowledge-graph.edges.write :as w]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]))

(use-fixtures :each (fn [f] (fixtures/global-datascript-fixture f)))

(def specs
  [{:from "node-a" :to "node-b" :relation :relates :scope "test-scope"}
   {:from "node-b" :to "node-c" :relation :depends-on :scope "test-scope"}])

(deftest writes-the-batch-and-returns-ids
  (let [ids (w/add-edges-sync! specs)]
    (is (= 2 (count ids)))
    (is (every? some? (map q/get-edge ids)) "edges are readable when the call returns")))

(deftest empty-input-writes-nothing
  (is (= [] (w/add-edges-sync! []))))

(deftest does-not-wait-on-pending-coalesced-writes
  ;; Simulate a backlog the global barrier would wait on: flush-pending!
  ;; would spin until its deadline while in-flight stays positive.
  (writer/ensure-writer!)
  (swap! writer/in-flight + 1000)
  (try
    (is (= :weave/timeout (writer/flush-pending! 200)) "the barrier waits on the backlog")
    (let [t0  (System/nanoTime)
          ids (w/add-edges-sync! specs)
          ms  (/ (- (System/nanoTime) t0) 1e6)]
      (is (= 2 (count ids)))
      (is (< ms 1000) (str "returned in " ms " ms despite the backlog")))
    (finally (swap! writer/in-flight - 1000))))
