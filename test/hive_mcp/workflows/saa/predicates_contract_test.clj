(ns hive-mcp.workflows.saa.predicates-contract-test
  "Contracts and hive-test facets for the SAA dispatch predicates.

   The numeric predicates are relational: each trifecta checks the predicate
   against the documented threshold arithmetic (grounding threshold 0.6 by
   default, 3 silence iterations, 2 abstract retries) over generated state that
   straddles every boundary. The complementary pairs are asserted to partition
   the state space, which is what makes the FSM's dispatch total."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.workflows.saa.predicates :as pred]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.generator :as mg]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  (set (for [s '[has-required-fields? has-startup-error? context-loaded? has-observations?
                 has-error? grounding-sufficient? grounding-insufficient-retryable?
                 grounding-max-iterations? has-plan? plan-valid? plan-invalid-retryable?
                 plan-invalid-final? plan-only? full-execution? has-execution-result?
                 tests-passed? tests-failed? plan-nil-with-error? context-not-loaded?
                 noop-subscription]]
         (ns-resolve 'hive-mcp.workflows.saa.predicates s))))

(use-fixtures :each
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument!)))))

(def ^:private State
  "SAA state reaching both sides of every threshold."
  [:map
   [:grounding-score {:optional true} [:enum 0.0 0.3 0.59 0.6 0.61 1.0]]
   [:grounding-threshold {:optional true} [:enum 0.5 0.6 0.8]]
   [:silence-iterations {:optional true} [:enum 0 1 2 3 4]]
   [:abstract-retries {:optional true} [:enum 0 1 2 3]]
   [:plan-valid? {:optional true} [:maybe :boolean]]
   [:plan-only? {:optional true} [:maybe :boolean]]
   [:tests-passed? {:optional true} [:maybe :boolean]]
   [:context-loaded? {:optional true} [:maybe :boolean]]
   [:plan {:optional true} [:maybe [:map [:steps [:vector {:gen/max 2} :string]]]]]
   [:error {:optional true} [:maybe [:enum "boom"]]]
   [:agent-id {:optional true} [:maybe [:enum "a"]]]
   [:task {:optional true} [:maybe [:enum "t"]]]])

(defn- score [d] (get d :grounding-score 0.0))
(defn- thr [d] (get d :grounding-threshold 0.6))
(defn- iters [d] (get d :silence-iterations 0))
(defn- retries [d] (get d :abstract-retries 0))

(deftrifecta-from-schema grounding-sufficient-threshold
  hive-mcp.workflows.saa.predicates/grounding-sufficient?
  {:in State :out :boolean
   :rel (fn [d out] (= out (>= (score d) (thr d))))
   :golden-path "test/golden/workflows/saa-grounding-sufficient.edn"
   :num-tests 200})

(deftrifecta-from-schema grounding-retryable-threshold
  hive-mcp.workflows.saa.predicates/grounding-insufficient-retryable?
  {:in State :out :boolean
   :rel (fn [d out] (= out (and (< (score d) (thr d)) (< (iters d) 3))))
   :num-tests 200})

(deftrifecta-from-schema grounding-max-iterations-threshold
  hive-mcp.workflows.saa.predicates/grounding-max-iterations?
  {:in State :out :boolean
   :rel (fn [d out] (= out (>= (iters d) 3)))
   :num-tests 200})

(deftrifecta-from-schema plan-invalid-retryable-threshold
  hive-mcp.workflows.saa.predicates/plan-invalid-retryable?
  {:in State :out :boolean
   :rel (fn [d out] (= out (and (not (:plan-valid? d)) (< (retries d) 2))))
   :golden-path "test/golden/workflows/saa-plan-invalid-retryable.edn"
   :num-tests 200})

(deftrifecta-from-schema plan-invalid-final-threshold
  hive-mcp.workflows.saa.predicates/plan-invalid-final?
  {:in State :out :boolean
   :rel (fn [d out] (= out (and (not (:plan-valid? d)) (>= (retries d) 2))))
   :num-tests 200})

(deftrifecta-from-schema startup-error-detection
  hive-mcp.workflows.saa.predicates/has-startup-error?
  {:in State :out :boolean
   :rel (fn [d out] (= out (or (nil? (:agent-id d)) (nil? (:task d)) (some? (:error d)))))
   :num-tests 200})

(deftrifecta-from-schema plan-nil-with-error-detection
  hive-mcp.workflows.saa.predicates/plan-nil-with-error?
  {:in State :out :boolean
   :rel (fn [d out] (= out (and (nil? (:plan d)) (some? (:error d)))))
   :num-tests 200})

(defspec complementary-pairs-partition-the-state 200
  (prop/for-all [d (mg/generator State)]
    (and (not= (boolean (pred/has-required-fields? d)) (pred/has-startup-error? d))
         (not= (pred/context-loaded? d) (pred/context-not-loaded? d))
         (not= (pred/plan-only? d) (pred/full-execution? d))
         (not= (pred/tests-passed? d) (pred/tests-failed? d))
         ;; silence-review routes: exactly one of sufficient / retry / give-up
         ;; fires first, and at least one always fires
         (or (pred/grounding-sufficient? d)
             (pred/grounding-insufficient-retryable? d)
             (pred/grounding-max-iterations? d))
         ;; validate-plan routes: valid, or exactly one of retry / final
         (or (pred/plan-valid? d)
             (not= (pred/plan-invalid-retryable? d) (pred/plan-invalid-final? d))))))

(deftest presence-predicates
  (testing "some? predicates answer booleans, including over nil data"
    (doseq [p [pred/has-observations? pred/has-error? pred/has-plan?
               pred/has-execution-result? pred/plan-nil-with-error?]]
      (is (false? (p nil)))
      (is (false? (p {}))))
    (is (true? (pred/has-observations? {:observations []})) "an empty vector is present")
    (is (true? (pred/has-error? {:error false})) "false is a present error value")
    (is (true? (pred/has-execution-result? {:execution-result {:status :ok}}))))
  (testing "true?-predicates do not accept mere truthiness"
    (is (false? (pred/context-loaded? {:context-loaded? "yes"})))
    (is (false? (pred/plan-valid? {:plan-valid? 1})))
    (is (false? (pred/plan-only? {:plan-only? :yes})))
    (is (false? (pred/tests-passed? {:tests-passed? "true"}))))
  (testing "has-required-fields? answers a generalized boolean"
    (is (true? (pred/has-required-fields? {:agent-id "a" :task "t"})))
    (is (nil? (pred/has-required-fields? {:task "t"})))
    (is (false? (pred/has-required-fields? {:agent-id "a" :task "t" :error "e"}))))
  (testing "a non-numeric counter is outside the contract"
    (is (thrown? Exception (pred/grounding-max-iterations? {:silence-iterations "3"}))))
  (is (nil? (pred/noop-subscription [:x] 0 1)))
  (is (true? (pred/always {:anything 1})) "always is the shared support seam"))
