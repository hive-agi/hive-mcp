(ns hive-mcp.knowledge-graph.edges.co-access-cap-test
  "record-co-access! caps the :co-accessed degree of every node at the writer
   [NATIVE-RSS-0.4]. A trifecta pins the pure cap predicate; the store tests
   pin that a capped node gains no new edge, that reinforcement is never
   capped, and that the cap counts edges written earlier in the same call."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.promotion :as promotion]
            [hive-mcp.knowledge-graph.edges.queries :as queries]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [hive-schemas.test :as hst]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private node [:enum "a" "b" "c" "d"])

(hst/deftrifecta-from-schema under-degree-cap-contract
  hive-mcp.knowledge-graph.edges.promotion/under-degree-cap?
  {:in [:cat [:map-of node [:int {:min 0 :max 6}]] [:int {:min 0 :max 6}] node node]
   :out :boolean
   :rel (fn [[degree cap from to] out]
          (= out (and (< (get degree from 0) cap)
                      (< (get degree to 0) cap))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

(defn- co-accessed-edges []
  (queries/get-edges-by-relation :co-accessed nil))

(defn- degree-of [id]
  (count (filter #(or (= id (:kg-edge/from %)) (= id (:kg-edge/to %)))
                 (co-accessed-edges))))

(deftest capped-node-gains-no-new-edge
  (sup/in-fresh-store
   (fn []
     (doseq [i (range 3)]
       (write/add-edge! {:from "hub" :to (str "old" i) :relation :co-accessed
                         :confidence 0.3 :source-type :co-access}))
     (testing "hub is at the cap: the hub pairs are skipped, the rest are written"
       (is (= 1 (promotion/record-co-access! ["hub" "x" "y"] {:max-degree 3})))
       (is (= 3 (degree-of "hub")))
       (is (some? (queries/find-edge "x" "y" :co-accessed)))
       (is (nil? (queries/find-edge "hub" "x" :co-accessed))))
     (testing "reinforcing an existing hub edge is not capped"
       (is (= 1 (promotion/record-co-access! ["hub" "old0"] {:max-degree 3})))
       (is (< 0.35 (:kg-edge/confidence (queries/find-edge "hub" "old0" :co-accessed))))
       (is (= 3 (degree-of "hub")))))))

(deftest cap-counts-edges-written-in-the-same-call
  (sup/in-fresh-store
   (fn []
     (promotion/record-co-access! ["a" "b" "c" "d" "e"] {:max-degree 2})
     (let [degrees (map degree-of ["a" "b" "c" "d" "e"])]
       (is (every? #(<= % 2) degrees) (str "degrees " (vec degrees)))
       (is (pos? (count (co-accessed-edges))))))))

(deftest default-cap-leaves-small-batches-untouched
  (sup/in-fresh-store
   (fn []
     (is (= 10 (promotion/record-co-access! ["a" "b" "c" "d" "e"])))
     (is (= 10 (count (co-accessed-edges))))
     (is (= 50 promotion/default-co-access-max-degree)))))
