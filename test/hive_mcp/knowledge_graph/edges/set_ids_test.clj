(ns hive-mcp.knowledge-graph.edges.set-ids-test
  "The batch readers and record-co-access! take any collection of node ids:
   a non-empty SET answers the same as the vector of its members."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.edges.batch :as batch]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.promotion :as promotion]
            [hive-mcp.knowledge-graph.edges.queries :as queries]
            [hive-mcp.knowledge-graph.edges.schema :as es]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- norm
  "Result map with each node's edges as a set of edge ids, so the order the
   ids were handed in cannot matter."
  [m]
  (into {} (map (fn [[k v]] [k (set (map #(or (:kg-edge/id %) %) v))])) m))

(deftest node-ids-schema-admits-a-non-empty-set
  (is (m/validate es/NodeIds #{"n1" "n2"}))
  (is (m/validate es/NodeIds ["n1"]))
  (is (m/validate es/NodeIds nil)))

(deftest batch-readers-accept-a-set
  (sup/in-fresh-store
   (fn []
     (sup/seed! write/add-edge!)
     (write/add-edge! {:from "c1" :to "c2" :relation :co-accessed :confidence 0.6})
     (let [ids #{"n1" "n2" "n3" "c1"}
           v (vec ids)
           db (conn/db-snapshot)]
       (testing "batch-get-edges-from"
         (is (= (norm (batch/batch-get-edges-from v))
                (norm (batch/batch-get-edges-from ids))))
         (is (seq (batch/batch-get-edges-from ids))))
       (testing "batch-get-edges-to"
         (is (= (norm (batch/batch-get-edges-to v "s2"))
                (norm (batch/batch-get-edges-to ids "s2")))))
       (testing "with-db variants"
         (is (= (norm (batch/batch-get-edges-from-with-db db v))
                (norm (batch/batch-get-edges-from-with-db db ids))))
         (is (= (norm (batch/batch-get-edges-to-with-db db v))
                (norm (batch/batch-get-edges-to-with-db db ids)))))
       (testing "batch-get-co-accessed"
         (is (= (norm (batch/batch-get-co-accessed v))
                (norm (batch/batch-get-co-accessed ids))))
         (is (contains? (batch/batch-get-co-accessed ids) "c1")))))))

(deftest record-co-access-accepts-a-set
  (sup/in-fresh-store
   (fn []
     (is (= 1 (promotion/record-co-access! #{"a1" "a2"})))
     (is (some? (or (queries/find-edge "a1" "a2" :co-accessed)
                    (queries/find-edge "a2" "a1" :co-accessed)))))))
