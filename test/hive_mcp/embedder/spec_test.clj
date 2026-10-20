(ns hive-mcp.embedder.spec-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.embedder.spec :as spec]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.generator :as mg]
            [malli.instrument :as mi]))

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

;; make's docstring promises a TAGGED error for a malformed spec, so its
;; contract must admit the malformed map. Under instrumentation a too-tight
;; :in schema throws malli's invalid-input before make can tag anything.
(deftest malformed-spec-gets-the-promised-tagged-error
  (let [bad   {:provider/key :k :provider/impl :not-a-provider}
        tag-of (fn [] (try (spec/make bad) nil
                           (catch clojure.lang.ExceptionInfo e (:err/tag (ex-data e)))))]
    (is (= :embedder/invalid-spec (tag-of)))
    (mi/instrument! {:filters [(mi/-filter-var #{#'spec/make})]})
    (try
      (is (= :embedder/invalid-spec (tag-of)))
      (finally (mi/unstrument! {:filters [(mi/-filter-var #{#'spec/make})]})))))
