(ns hive-mcp.memory.store.contract-coverage-test
  "Chroma adapter contract coverage and schema-derived field-key facets."
  (:require [hive-mcp.memory.store.chroma :as chroma]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]))

(deftest-contract-coverage store-contracts
  {:paths ["src/hive_mcp/memory/store"]})

(hst/deftrifecta-from-schema field-key-contract
  hive-mcp.memory.store.chroma/field-key
  {:in [:enum "project_id" "foo_bar" "id" :project-id :some-field]
   :out :keyword
   :rel (fn [in out]
          (= (name out)
             (clojure.string/replace (if (instance? clojure.lang.Named in)
                                       (name in) (str in)) "_" "-")))
   :golden-path "test/golden/hive-mcp/memory/store/field-key-contract.edn"
   :num-tests 100})

(hst/deftrifecta-from-schema project-fields-contract
  hive-mcp.memory.store.chroma/project-fields
  {:in [:cat [:vector [:map [:id :string] [:project-id :string] [:content :string]]]
        [:vector [:enum "project_id" "content" "missing"]]]
   :out [:vector :map]
   :rel (fn [[entries fields] out]
          (and (= (count entries) (count out))
               (every? #(contains? % :id) out)
               (every? #(every? (if (seq fields)
                                 (set (concat [:id] (map chroma/field-key fields)))
                                 (set (mapcat keys entries)))
                                 (keys %)) out)))
   :golden-path "test/golden/hive-mcp/memory/store/project-fields-contract.edn"
   :num-tests 100})
