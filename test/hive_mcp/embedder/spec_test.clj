(ns hive-mcp.embedder.spec-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.embedder.spec :as spec]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.generator :as mg]))

(deftrifecta-from-schema cache-key-trifecta spec/cache-key
  {:in [:cat spec/schema]
   :out [:tuple [:enum :ollama :venice :openai :openrouter] :string pos-int?]
   :rel (fn [[provider] key]
          (= key ((juxt :provider/impl :provider/model :provider/dim) provider)))
   :num-tests 500})

(deftest provider-spec-is-generative-and-round-trips
  (let [sample (mg/generate spec/schema)]
    (is (spec/validator sample))
    (is (= sample (spec/make sample)))
    (is (= ((juxt :provider/impl :provider/model :provider/dim) sample)
           (spec/cache-key sample)))))
