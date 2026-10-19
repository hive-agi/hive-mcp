(ns hive-mcp.crystal.harvest.collect-contract-test
  "Schema-derived facets for the pure assembly layer of
   `hive-mcp.crystal.harvest.collect` and for the reified harvest sources."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.crystal.harvest.collect :as coll]
            [hive-mcp.crystal.harvest.protocol :as proto]
            [hive-mcp.crystal.harvest.sources :as src]
            [hive-schemas.test :as hst]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:enum "coord" "ling-a" "ling-b" "ling-c" "stranger"])

(hst/deftrifecta-from-schema lineage-agent-ids-contract
  hive-mcp.crystal.harvest.collect/lineage-agent-ids
  {:in  [:cat [:enum "coord" "ling-a" "" "coordinator" nil]
         [:vector [:map [:slave/id Id] [:slave/parent-id {:optional true} Id]]]]
   :out [:maybe [:set :string]]
   :rel (fn [[root slaves] out]
          (if (contains? #{"" "coordinator" nil} root)
            (nil? out)
            (let [parent (into {} (keep (fn [{:slave/keys [id parent-id]}]
                                          (when (and parent-id (not= id parent-id)) [id parent-id])))
                               slaves)
                  reaches? (fn [id] (loop [c id seen #{}]
                                      (cond (= c root) true
                                            (or (nil? c) (seen c)) false
                                            :else (recur (parent c) (conj seen c)))))]
              (= out (into #{root} (filter reaches?) (keys parent))))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/harvest/lineage-agent-ids-contract.edn"})

(hst/deftrifecta-from-schema scope-created-ids-contract
  hive-mcp.crystal.harvest.collect/scope-created-ids
  {:in  [:cat [:vector [:map [:id :string] [:project-id {:optional true} [:enum "hive" "funeraria"]]]]
         [:enum "hive" "funeraria"]]
   :out [:vector :map]
   :rel (fn [[entries pid] out]
          (= out (filterv #(= pid (:project-id %)) entries)))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/harvest/scope-created-ids-contract.edn"})

(hst/deftrifecta-from-schema scope-recalls-contract
  hive-mcp.crystal.harvest.collect/scope-recalls
  {:in  [:cat [:map-of [:enum "m1" "m2" "m3"]
               [:vector [:map [:agent-id Id] [:project [:enum "hive" "funeraria"]]]]]
         [:set Id]
         [:enum "hive" "funeraria"]]
   :out [:map-of :string [:vector :map]]
   :rel (fn [[recalls lineage pid] out]
          (and (every? seq (vals out))
               (every? (fn [[id evs]]
                         (= evs (filterv #(and (lineage (:agent-id %)) (= pid (:project %)))
                                         (get recalls id))))
                       out)))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/harvest/scope-recalls-contract.edn"})

(deftest assemble-harvest-result-counts
  (testing "summary counts mirror the components; created-by-type counts :type"
    (let [r (coll/assemble-harvest-result
             {:progress {:notes [{:a 1}] :count 1}
              :tasks {:tasks [] :count 0}
              :commits {:commits ["x" "y"] :count 2}
              :recalls {"m1" []}
              :memory-ids-created [{:type "decision"} {:type "decision"} {:id "n"}]
              :errors []})]
      (is (= 2 (get-in r [:summary :commit-count])))
      (is (= 1 (get-in r [:summary :recall-count])))
      (is (= {"decision" 2} (get-in r [:summary :created-by-type])))
      (is (nil? (:errors r)) "an empty error list reads as nil"))))

(deftest default-sources-are-the-four-ids
  (is (= [:memory :hivemind :kanban :git] (mapv proto/source-id (src/default-sources))))
  (is (true? (proto/available? (src/git-source)))))
