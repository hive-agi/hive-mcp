(ns hive-mcp.multi.contract-coverage-test
  "Multi routing schema facets and root directory coverage gate."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.multi.param-coerce :as coerce]
            [hive-mcp.multi.util :as util]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]))

(deftest-contract-coverage multi-contracts
  {:paths ["src/hive_mcp/multi/batchable_adapter.clj"
           "src/hive_mcp/multi/batchables.clj"
           "src/hive_mcp/multi/core_seed.clj"
           "src/hive_mcp/multi/param_coerce.clj"
           "src/hive_mcp/multi/plan.clj"
           "src/hive_mcp/multi/registry.clj"
           "src/hive_mcp/multi/types.clj"
           "src/hive_mcp/multi/util.clj"]})

(hst/deftrifecta-from-schema coercion-spec-contract
  hive-mcp.multi.param-coerce/coercion-spec
  {:in [:map [:properties [:map-of [:enum "a" "b" "c"]
                          [:map [:type [:enum "array" "object" "integer"
                                              "number" "boolean" "string"]]]]]]
   :out [:map-of :keyword [:enum "array" "object" "integer" "number" "boolean"]]
   :rel (fn [input out]
          (= out (into {} (keep (fn [[k {:keys [type]}]]
                                  (when (#{"array" "object" "integer" "number" "boolean"} type)
                                    [(keyword k) type])))
                       (:properties input))))
   :num-tests 100})

(hst/deftrifecta-from-schema mcp-text-decode-contract
  hive-mcp.multi.util/decode-mcp-text
  {:in [:enum {:text "{\"id\":\"a\"}"} {:text "not-json"}
         {:id "a"} "raw"]
   :out [:or :map :string]
   :rel (fn [input output]
          (if (= input {:text "{\"id\":\"a\"}"})
            (= output {:id "a"})
            (= output input)))
   :golden-path "test/golden/hive-mcp/multi/mcp-text-decode-contract.edn"
   :num-tests 100})
