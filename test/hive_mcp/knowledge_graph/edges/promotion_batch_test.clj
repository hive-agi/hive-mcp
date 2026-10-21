(ns hive-mcp.knowledge-graph.edges.promotion-batch-test
  "record-co-access! reads the existing co-access edges once per call. The
   property pins it to the pairwise reference it replaced (one find-edge per
   pair): same return count, same edges, same confidences, from any seeded
   graph. The count test pins the read to O(1) KG queries per call."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.promotion :as promotion]
            [hive-mcp.knowledge-graph.edges.queries :as queries]
            [hive-mcp.knowledge-graph.edges.write :as write]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- pairwise-record!
  "The pre-batch record-co-access!: one find-edge per pair."
  [entry-ids & [{:keys [scope created-by]}]]
  (let [ids (vec (distinct (seq entry-ids)))]
    (when (>= (count ids) 2)
      (conn/with-tx-batch
        (let [pairs (for [i (range (count ids))
                          j (range (inc i) (count ids))]
                      [(nth ids i) (nth ids j)])]
          (count
           (for [[from-id to-id] (take 50 pairs)]
             (if-let [existing (queries/find-edge from-id to-id :co-accessed)]
               (do (write/increment-confidence! (:kg-edge/id existing) 0.1)
                   (write/verify-edge! (:kg-edge/id existing))
                   :reinforced)
               (do (write/add-edge! (cond-> {:from from-id
                                             :to to-id
                                             :relation :co-accessed
                                             :confidence 0.3
                                             :source-type :co-access}
                                      scope (assoc :scope scope)
                                      created-by (assoc :created-by created-by)))
                   :created)))))))))

(def ^:private node-gen
  (gen/elements (mapv #(str "n" %) (range 12))))

(def ^:private seed-edge-gen
  (gen/hash-map :from node-gen
                :to node-gen
                :relation (gen/elements [:co-accessed :co-accessed :depends-on])
                :confidence (gen/elements [0.3 0.5 0.95])))

(def ^:private seed-gen
  "At most one edge per [from to relation]: record-co-access! never writes a
   duplicate, and with two the reinforced one is the store's arbitrary pick."
  (gen/fmap #(vals (into {} (map (juxt (juxt :from :to :relation) identity)) %))
            (gen/vector seed-edge-gen 0 15)))

(defn- run-in-fresh-store
  [record! seed ids opts]
  (sup/in-fresh-store
   (fn []
     (doseq [e seed] (write/add-edge! e))
     (let [n (record! ids opts)]
       {:count n
        :edges (sup/shown-all (queries/get-all-edges))}))))

(defspec batched-read-matches-pairwise-reference 60
  (prop/for-all [seed seed-gen
                 ids  (gen/vector node-gen 0 14)
                 opts (gen/elements [nil {:scope "s"} {:scope "s" :created-by "t"}])]
    (= (run-in-fresh-store pairwise-record! seed ids opts)
       (run-in-fresh-store promotion/record-co-access! seed ids opts))))

(defn- query-count
  "KG queries one record-co-access! call issues over `ids`."
  [seed ids]
  (sup/in-fresh-store
   (fn []
     (doseq [e seed] (write/add-edge! e))
     (let [n (atom 0)
           q conn/query]
       (with-redefs [conn/query (fn [& args] (swap! n inc) (apply q args))
                     queries/find-edge (fn [& _] (throw (ex-info "find-edge per pair" {})))]
         (promotion/record-co-access! ids))
       @n))))

(deftest co-access-reads-once-per-call
  (let [few  (mapv #(str "n" %) (range 3))
        many (mapv #(str "n" %) (range 12))
        seed (for [i (range 0 10 2)]
               {:from (str "n" i) :to (str "n" (inc i)) :relation :co-accessed})]
    (testing "query count does not grow with the number of pairs"
      (is (= (query-count [] few) (query-count [] many)))
      (is (= (query-count seed few) (query-count seed many))))
    (testing "one read covers every pair"
      (is (<= (query-count seed many) 1)))))
