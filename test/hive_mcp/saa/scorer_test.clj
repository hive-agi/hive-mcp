(ns hive-mcp.saa.scorer-test
  "Facets for DefaultObservationScorer: the IObservationScorer law suite run
   against the adapter ->default-scorer builds. score ranks every observation
   once, highest first, by the Korzybski keyword heuristic; grounding-score
   is the documented capped sum in [0, 1]."
  (:require [clojure.string :as str]
            [hive-mcp.protocols.saa :as psaa]
            [hive-mcp.saa.scorer :as scorer]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Observation
  [:or
   [:enum "a pattern here" "fix the bug" "add a test" "plain note" "decision: bug in spec"]
   [:map [:data [:enum "convention" "error path" "nothing"]]]])

(defn- expected-score [obs]
  (let [content (str (or (:data obs) obs))
        has? (fn [words] (some #(str/includes? content %) words))]
    (cond-> 1.0
      (has? ["pattern" "convention" "decision"]) (+ 2.0)
      (has? ["bug" "error" "issue" "fix"]) (+ 3.0)
      (has? ["test" "spec" "assert"]) (+ 1.5))))

(deftest-port-contract observation-scorer-port-laws
  {:port/methods
   {psaa/score           {:in  [:cat [:vector {:max 5} Observation]]
                          :out [:vector [:map [:observation Observation] [:score :double]]]
                          :rel (fn [[obs] out]
                                 (and (= (frequencies obs) (frequencies (map :observation out)))
                                      (every? #(= (:score %) (expected-score (:observation %))) out)
                                      (apply >= (map :score (cons {:score ##Inf} out)))))}
    psaa/grounding-score {:in  [:cat [:vector {:max 6} :any] [:maybe [:int {:min 0 :max 5}]]]
                          :out [:double {:min 0.0 :max 1.0}]
                          :rel (fn [[obs files] out]
                                 (== out (min 1.0 (+ (if (seq obs) 0.3 0.0)
                                                     (min 0.4 (* 0.1 (count obs)))
                                                     (if (pos? (or files 0)) 0.3 0.0)))))}}}
  (scorer/->default-scorer))

(golden/deftest-golden default-scorer-golden
  "test/golden/hive-mcp/saa/scorer-default.edn"
  (let [s (scorer/->default-scorer)]
    {:ranked (psaa/score s ["plain note" "fix the bug in the test" {:data "convention"}])
     :empty (psaa/score s [])
     :grounding {:none (psaa/grounding-score s [] nil)
                 :files-only (psaa/grounding-score s [] 2)
                 :some (psaa/grounding-score s [1 2] 0)
                 :saturated (psaa/grounding-score s (range 10) 1)}
     :satisfies? (satisfies? psaa/IObservationScorer s)}))
