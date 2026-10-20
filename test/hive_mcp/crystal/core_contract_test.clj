(ns hive-mcp.crystal.core-contract-test
  "Schema-derived facets for the pure crystallization rules in
   `hive-mcp.crystal.core`. The contracts live beside the fns; these facets
   draw their inputs from those schemas and check each rule against an
   independent oracle. No extension is registered, so the inline fallbacks
   are the subject."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.crystal.core :as core]
            [hive-schemas.test :as hst]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Context
  [:enum :catchup-structural :wrap-structural :explicit-reference :cross-session
   :cross-project :user-feedback :behavioral-success :behavioral-failure
   :behavioral-correction :unknown-context])

(def ^:private Recalls
  [:vector [:map [:context Context] [:count {:optional true} [:int {:min 0 :max 20}]]]])

(hst/deftrifecta-from-schema calculate-promotion-score-contract
  hive-mcp.crystal.core/calculate-promotion-score
  {:in  Recalls
   :out core/PromotionScore
   :rel (fn [recalls {:keys [score breakdown]}]
          (let [w (core/recall-weights)]
            (and (= (count recalls) (count breakdown))
                 (== score (reduce + 0.0 (map (fn [{:keys [context count] :or {count 1}}]
                                                (* (get w context 1.0) count))
                                              recalls))))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/calculate-promotion-score-contract.edn"})

(hst/deftrifecta-from-schema current-duration->next-contract
  hive-mcp.crystal.core/current-duration->next
  {:in  [:enum :ephemeral :short :medium :long :permanent
         "ephemeral" "short-term" "short" "medium" "long-term" "long" "permanent" "bogus" nil]
   :out core/Duration
   :rel (fn [d out]
          (= out (get {:ephemeral :short :short :medium :medium :long :long :permanent
                       :permanent :permanent "ephemeral" :short "short-term" :medium
                       "short" :medium "medium" :long "long-term" :permanent
                       "long" :permanent "permanent" :permanent}
                      d :medium)))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/current-duration-next-contract.edn"})

(hst/deftrifecta-from-schema should-demote-contract
  hive-mcp.crystal.core/should-demote?
  {:in  [:cat [:map [:duration [:enum :permanent :long :medium :short :ephemeral :weird]]]
         [:double {:min -10.0 :max 10.0 :gen/NaN? false :gen/infinite? false}]]
   :out core/DemoteVerdict
   :rel (fn [[_ adj] {:keys [demote? prev-duration reason]}]
          (and (= demote? (< adj -3.0))
               (= (some? prev-duration) demote?)
               (= (some? reason) demote?)))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/should-demote-contract.edn"})

(hst/deftrifecta-from-schema extract-xpoll-projects-contract
  hive-mcp.crystal.core/extract-xpoll-projects
  {:in  [:map [:tags [:vector [:enum "xpoll:project:hive" "xpoll:project:funeraria"
                               "xpoll:project:" "scope:project:hive" "agent:x"]]]]
   :out [:set :string]
   :rel (fn [{:keys [tags]} out]
          (= out (set (for [t tags :when (.startsWith ^String t "xpoll:project:")]
                        (subs t 14)))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/extract-xpoll-projects-contract.edn"})

(hst/deftrifecta-from-schema meaningful-harvest-contract
  hive-mcp.crystal.core/meaningful-harvest?
  {:in  [:map [:summary [:map
                         [:progress-count [:maybe [:int {:min 0 :max 3}]]]
                         [:task-count [:maybe [:int {:min 0 :max 3}]]]
                         [:commit-count [:maybe [:int {:min 0 :max 3}]]]
                         [:accessed-count [:int {:min 0 :max 3}]]]]]
   :out :boolean
   :rel (fn [{:keys [summary]} out]
          (= out (boolean (some #(and % (pos? %))
                                ((juxt :progress-count :task-count :commit-count) summary)))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/crystal/meaningful-harvest-contract.edn"})

(deftest boolean-rule-arms
  (testing "mechanical / behavioral classifiers cover their sets and nothing else"
    (is (true? (core/mechanical-recall? :catchup-structural)))
    (is (false? (core/mechanical-recall? :cross-session)))
    (is (true? (core/behavioral-recall? :behavioral-failure)))
    (is (false? (core/behavioral-recall? :user-feedback))))
  (testing "endorsement / behavioral signal answer true or nil, never false"
    (is (true? (core/has-user-endorsement? [{:context :user-feedback}])))
    (is (nil? (core/has-user-endorsement? [{:context :cross-session}])))
    (is (true? (core/has-behavioral-signal? [{:context :behavioral-success}])))
    (is (nil? (core/has-behavioral-signal? []))))
  (testing "decay delta is zero inside the recency window and positive past it"
    (is (= 0.0 (core/calculate-decay-delta {:last-accessed (str (java.time.ZonedDateTime/now))})))
    (is (pos? (core/calculate-decay-delta {:last-accessed "2020-01-01T00:00:00Z" :duration "short"}))))
  (testing "summaries: nil without content, a :short note with it"
    (is (nil? (core/summarize-memory-activity {:created 0 :accessed 0})))
    (is (= :short (:duration (core/summarize-memory-activity {:created 2}))))
    (is (nil? (core/summarize-session-progress [] [])))
    (is (= :short (:duration (core/summarize-session-progress [] ["abc fix"]))))))
