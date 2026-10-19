(ns hive-mcp.knowledge-graph.edges.schema-facet-test
  "The edge value objects: every one generates, and a golden of which probe
   values each accepts."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.edges.schema :as es]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest every-edge-schema-generates
  (doseq [[sym v] (ns-publics 'hive-mcp.knowledge-graph.edges.schema)]
    (testing (str sym)
      (let [xs (mg/sample @v {:size 10 :seed 14})]
        (is (= 10 (count xs)))
        (is (every? #(m/validate @v %) xs))))))

(def ^:private probes
  {:uuid "123e4567-e89b-12d3-a456-426614174000" :uuid-upper "123E4567-E89B-12D3-A456-426614174000"
   :uuid-embedded "x123e4567-e89b-12d3-a456-426614174000" :nil nil
   :ids-vec ["a" "b"] :ids-empty-set #{} :ids-set #{"a"} :ids-list '("a")
   :edge {:kg-edge/id "e" :kg-edge/from "a" :kg-edge/to "b" :kg-edge/relation :implements}
   :edge-string-rel {:kg-edge/id "e" :kg-edge/from "a" :kg-edge/to "b" :kg-edge/relation "implements"}
   :edge-no-id {:kg-edge/from "a" :kg-edge/to "b" :kg-edge/relation :implements}
   :half 0.5 :two 2.0 :keyword :implements})

(golden/deftest-golden edge-schema-acceptance-golden
  "test/golden/hive-mcp/m14-edges-schema.edn"
  (into (sorted-map)
        (for [[sname s] {:EdgeId es/EdgeId :NodeIds es/NodeIds :Edge es/Edge :Relation es/Relation
                         :Confidence es/Confidence :Scope es/Scope :EdgeSpec es/EdgeSpec}]
          [sname (into (sorted-map) (for [[p v] probes] [p (m/validate s v)]))])))
