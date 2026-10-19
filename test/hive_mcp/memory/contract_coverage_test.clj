(ns hive-mcp.memory.contract-coverage-test
  "Memory domain schema facets and the memory directory coverage gate."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.memory.domain :as domain]
            [hive-mcp.memory.ingest-search :as ingest]
            [hive-mcp.memory.temporal :as temporal]
            [hive-mcp.memory.types :as types]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]))

(hst/deftrifecta-from-schema scope-parsing-contract
  hive-mcp.memory.domain/parse-scope
  {:in [:cat [:enum nil "all" "global" "hive"] [:maybe :string]]
   :out [:map [:adt/type :keyword] [:adt/variant :keyword]]
   :rel (fn [[scope _] out]
          (= (:adt/variant out)
             (case scope "all" :scope/all "global" :scope/global
                   nil :scope/auto :scope/project)))
   :golden-path "test/golden/hive-mcp/memory/scope-parsing-contract.edn"
   :num-tests 100})

(hst/deftrifecta-from-schema type-coercion-contract
  hive-mcp.memory.types/from-string
  {:in [:enum "axiom" "note" "unknown-safe-type"]
   :out [:maybe [:map [:adt/type :keyword] [:adt/variant :keyword]]]
   :rel (fn [input out] (if (= input "unknown-safe-type")
                         (nil? out) (= (keyword input) (:adt/variant out))))
   :golden-path "test/golden/hive-mcp/memory/type-coercion-contract.edn"
   :num-tests 100})

(hst/deftrifecta-from-schema merge-and-rerank-contract
  hive-mcp.memory.ingest-search/merge-and-rerank
  {:in [:cat [:vector [:map [:id [:enum "a" "b"]] [:distance [:double {:min 0.0 :max 2.0}]]]]
        [:vector [:map [:id [:enum "a" "b"]] [:distance [:double {:min 0.0 :max 2.0}]]]]
        [:int {:min 0 :max 5}]]
   :out [:vector [:map [:id :string] [:distance :double]]]
   :rel (fn [[a b n] out]
          (and (<= (count out) n)
               (= (count out) (count (set (map :id out))))
               (= (map :distance out) (sort (map :distance out)))
               (every? (set (concat a b)) out)))
   :golden-path "test/golden/hive-mcp/memory/merge-and-rerank-contract.edn"
   :num-tests 100})

(deftest-contract-coverage memory-contracts
  {:paths ["src/hive_mcp/memory/domain.clj"
           "src/hive_mcp/memory/ids.clj"
           "src/hive_mcp/memory/ingest_search.clj"
           "src/hive_mcp/memory/synthesis_protection.clj"
           "src/hive_mcp/memory/temporal.clj"
           "src/hive_mcp/memory/type_registry.clj"
           "src/hive_mcp/memory/types.clj"
           "src/hive_mcp/memory/write_events.clj"]})

(hst/deftrifecta-from-schema prune-victims-contract
  hive-mcp.memory.temporal/prune-victims
  {:in [:cat [:vector [:tuple [:int {:min 0 :max 100}] :string
                             [:fn {:gen/elements [(java.util.Date. 0)
                                                  (java.util.Date. 1000)
                                                  (java.util.Date. 2000)]}
                              #(instance? java.util.Date %)]
                             [:enum :feedback :kanban-done]]]
        [:fn {:gen/elements [(java.util.Date. 1000)]} #(instance? java.util.Date %)]
        [:map [:max-per-entry [:int {:min 0 :max 4}]]
         [:batch-cap [:int {:min 0 :max 6}]]]]
   :out [:vector :int]
   :rel (fn [[rows _ {:keys [batch-cap]}] victims]
          (and (<= (count victims) batch-cap)
               (= (count victims) (count (distinct victims)))
               (every? (set (map first (remove #(= :kanban-done (last %)) rows))) victims)))
   :num-tests 100})
