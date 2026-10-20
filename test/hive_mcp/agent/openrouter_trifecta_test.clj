(ns hive-mcp.agent.openrouter-trifecta-test
  "Facets for the OpenAI-compatible boundary: `parse-response` (pure) over its
   contract, the metrics counters, and the coverage gate for the namespace."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.openrouter :as openrouter]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Choice
  "A provider message reaching each arm: text, blank, tool calls, nil."
  [:or
   :nil
   [:map [:content [:string {:min 1 :gen/fmap #(str "t" %)}]]]
   [:map [:content [:maybe [:string {:max 3 :gen/fmap #(str/replace % #"\S" " ")}]]]]
   [:map
    [:tool_calls [:vector {:min 1 :max 3}
                  [:map
                   [:id :string]
                   [:function [:map
                               [:name :string]
                               [:arguments [:= "{\"a\": 1}"]]]]]]]]])

(deftrifecta-from-schema parse-response-trifecta
  hive-mcp.agent.openrouter/parse-response
  {:in Choice
   :out openrouter/ParsedResponse
   :rel (fn [choice out]
          (cond
            (nil? choice) (= :error (:type out))
            (seq (:tool_calls choice)) (and (= :tool_calls (:type out))
                                            (= (count (:tool_calls choice)) (count (:calls out)))
                                            (every? #(= {:a 1} (:arguments %)) (:calls out)))
            (str/blank? (:content choice)) (= :error (:type out))
            :else (= {:type :text :content (:content choice)} out)))
   :num-tests 300})

(deftest parse-response-arms-are-disjoint
  (let [arm-schemas {:error [:map [:type [:= :error]] [:error :string]]
                     :tool_calls [:map [:type [:= :tool_calls]] [:calls [:vector :map]]]
                     :text [:map [:type [:= :text]] [:content :string]]}
        outs {:error (openrouter/parse-response nil)
              :tool_calls (openrouter/parse-response
                           {:tool_calls [{:id "1" :function {:name "f" :arguments "{}"}}]})
              :text (openrouter/parse-response {:content "hi"})}]
    (doseq [[arm out] outs
            [other sch] arm-schemas]
      (is (= (= arm other) (m/validate sch out))
          (str arm " output vs " other " schema")))
    (is (m/validate openrouter/ParsedResponse (:error outs)))))

(deftest metrics-counters-reset-and-average
  (is (m/validate openrouter/MetricCounters (openrouter/reset-metrics!)))
  (let [mm (openrouter/get-metrics)]
    (is (= 0 (:avg-latency-ms mm)))
    (is (= 0.0 (:error-rate mm)))
    (is (m/validate openrouter/Metrics mm))))

(golden/deftest-golden-fn parse-response-golden
  "test/golden/agent/openrouter-parse-response.edn"
  (fn []
    {:nil (openrouter/parse-response nil)
     :blank (openrouter/parse-response {:content "   "})
     :whitespace-nil-content (openrouter/parse-response {:content nil})
     :text (openrouter/parse-response {:content "hello"})
     :tools (openrouter/parse-response
             {:tool_calls [{:id "c1" :function {:name "memory" :arguments "{\"q\":\"x\"}"}}]})}))

(deftest-contract-coverage openrouter-contract-coverage
  {:paths ["src/hive_mcp/agent/openrouter.clj"]})
