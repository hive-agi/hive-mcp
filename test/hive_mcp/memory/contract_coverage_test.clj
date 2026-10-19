(ns hive-mcp.memory.contract-coverage-test
  "Memory domain schema facets and the memory directory coverage gate."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.memory.domain :as domain]
            [hive-mcp.memory.ingest-search :as ingest]
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
