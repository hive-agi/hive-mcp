(ns hive-mcp.workflows.saa-workflow-contract-test
  "Contracts and hive-test facets for the SAA workflow aggregate.

   compile-saa / load-edn-spec are checked for shape; the runners are driven
   end to end through stub resources under instrumentation, with a golden over
   the terminal summary of each route (plan-only, full execution, verify
   failure) so a change to the summary shape is caught."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.saa-workflow :as saa]
            [hive-mcp.workflows.schema :as wfs]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'saa/load-edn-spec #'saa/compile-saa #'saa/run-saa #'saa/run-full-saa #'saa/run-plan-only})

(use-fixtures :each
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument!)))))

(defn- resources [overrides]
  (merge {:clock-fn      (constantly "T0")
          :catchup-fn    (constantly {:axioms [] :conventions [] :decisions []})
          :explore-fn    (fn [_ _ _] {:observations [{:type :read} {:type :grep} {:type :read}]
                                      :files-read 3})
          :synthesize-fn (fn [_ _ _] {:steps ["a" "b"]})
          :store-plan-fn (constantly {:memory-id "m1" :kanban-ids ["k1"] :kg-edges 1})
          :dispatch-fn   (fn [_ _ _ _] {:wave-id "w1" :result {:status :done}})
          :verify-fn     (constantly {:passed? true :details {:tests 3}})}
         overrides))

(defn- summarize [out]
  (select-keys out [:phase :plan-only? :plan-valid? :tests-passed? :plan-memory-id
                    :kanban-task-ids :execution-result :silence-iterations :abstract-retries
                    :grounding-score]))

(deftest compiled-spec-shape
  (is (m/validate wfs/CompiledFsm (saa/compile-saa)))
  (let [spec (saa/load-edn-spec)]
    (is (contains? (:fsm spec) :hive.events.fsm/start))
    (testing "every dispatch predicate is resolved to a function"
      (is (every? fn? (for [[_ st] (:fsm spec) [_ p] (:dispatches st)] p))))))

(deftest-golden saa-routes
  "test/golden/workflows/saa-routes.edn"
  {:plan-only   (summarize (saa/run-plan-only (resources {}) {:agent-id "a" :task "t"}))
   :full        (summarize (saa/run-full-saa (resources {}) {:agent-id "a" :task "t"}))
   ;; a failed verify routes to ::fsm/error; error-response-fn returns its context
   :verify-fail (-> (saa/run-full-saa (resources {:verify-fn (constantly {:passed? false})
                                                  :error-response-fn identity})
                                      {:agent-id "a" :task "t"})
                    (update :data summarize))})

(deftest runner-arms
  (let [compiled (saa/compile-saa)]
    (testing "2-arity defaults the opts"
      (is (thrown? clojure.lang.ExceptionInfo (saa/run-saa compiled (resources {})))
          "no :agent-id/:task routes to ::fsm/error, which throws without an error-response-fn"))
    (testing "error-response-fn turns the error route into data"
      (let [out (saa/run-saa compiled (resources {:error-response-fn identity}) {})]
        (is (= "Missing required fields: :agent-id and :task" (get-in out [:data :error])))
        (is (nil? (:agent-id out))))))
  (testing "full run reaches act-verify"
    (is (= :act-verify (:phase (saa/run-full-saa (resources {}) {:agent-id "a" :task "t"})))))
  (testing "plan-only stops after store-plan"
    (let [out (saa/run-plan-only (resources {}) {:agent-id "a" :task "t"})]
      (is (= [:store-plan true nil] ((juxt :phase :plan-only? :execution-result) out)))))
  (testing "the contract rejects a non-map opts"
    (is (thrown? Exception (saa/run-full-saa (resources {}) "opts")))))
