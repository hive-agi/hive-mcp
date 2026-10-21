(ns hive-mcp.workflows.saa.handlers-contract-test
  "Contracts and hive-test facets for the SAA state handlers.

   Every side effect is a resources fn, so each handler is driven through
   stub resources (no with-redefs) under instrumentation, one test per arm:
   resource present / absent / throwing. silence-review is pure given a clock
   and gets a schema trifecta over generated observations."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.saa.handlers :as h]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  (set (for [s '[handle-start handle-catchup handle-silence handle-silence-review
                 handle-abstract handle-validate-plan handle-store-plan
                 handle-act-dispatch handle-act-verify handle-end handle-error]]
         (ns-resolve 'hive-mcp.workflows.saa.handlers s))))

(use-fixtures :each
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument!)))))

(def ^:private clock {:clock-fn (constantly "T0")})
(defn- boom [& _] (throw (ex-info "boom" {})))

(defn- out-schema [v]
  (-> (m/function-schemas) (get-in ['hive-mcp.workflows.saa.handlers v :schema])
      m/schema m/children second))

;; =============================================================================
;; silence-review — pure scoring given a clock
;; =============================================================================

(def ^:private ReviewIn
  [:cat
   [:map [:clock-fn [:= (constantly "T0")]]]
   [:map
    [:observations [:vector {:gen/max 6} [:map [:type [:enum :read :grep]]]]]
    [:files-read [:maybe [:int {:min 0 :max 5}]]]
    [:grounding-threshold [:enum 0.6]]
    [:silence-iterations [:int {:min 0 :max 3}]]]])

(defn- default-score [obs files]
  (min 1.0 (+ (if (seq obs) 0.3 0.0)
              (min 0.4 (* 0.1 (count obs)))
              (if (pos? (or files 0)) 0.3 0.0))))

(deftrifecta-from-schema silence-review-scores
  hive-mcp.workflows.saa.handlers/handle-silence-review
  {:in ReviewIn
   :out [:map [:phase [:= :silence-review]] [:grounding-score :double] [:silence-ended :string]]
   :rel (fn [[_ data] out]
          (and (= (:grounding-score out) (double (default-score (:observations data) (:files-read data))))
               (<= 0.0 (:grounding-score out) 1.0)
               (= "T0" (:silence-ended out))))
   :num-tests 200})

(deftest silence-review-honours-an-injected-scorer
  (is (= 0.9 (:grounding-score (h/handle-silence-review
                                (assoc clock :score-grounding-fn (constantly 0.9)) {})))))

;; =============================================================================
;; start / catchup — both [:or] arms, and each arm fails the other
;; =============================================================================

(deftest start-arms
  (let [[ok-arm err-arm] (m/children (out-schema 'handle-start))
        ok  (h/handle-start (assoc clock :scope-fn (constantly "p")) {:agent-id "a" :task "t"})
        bad (h/handle-start clock {:task "t"})]
    (is (= {:agent-id "a" :task "t" :directory nil :project-id "unknown" :started-at "T0" :phase :start
            :grounding-threshold 0.6 :silence-iterations 0 :abstract-retries 0
            :plan-only? false :error nil}
           ok)
        "no directory: scope-fn is never asked, project falls back to \"unknown\"")
    (is (= "unknown" (:project-id (h/handle-start (assoc clock :scope-fn (constantly nil))
                                                  {:agent-id "a" :task "t" :directory "/d"}))))
    (is (= "p" (:project-id (h/handle-start (assoc clock :scope-fn (constantly "p"))
                                            {:agent-id "a" :task "t" :directory "/d"}))))
    (is (and (m/validate ok-arm ok) (not (m/validate err-arm ok))))
    (is (and (m/validate err-arm bad) (not (m/validate ok-arm bad))))))

(deftest catchup-arms
  (let [[ok-arm err-arm] (m/children (out-schema 'handle-catchup))
        loaded (h/handle-catchup {:catchup-fn (constantly {:axioms [1]})} {:agent-id "a"})
        absent (h/handle-catchup {} {})
        failed (h/handle-catchup {:catchup-fn boom} {})]
    (is (= {:agent-id "a" :phase :catchup :context-loaded? true :axioms [1] :conventions [] :decisions []}
           loaded))
    (is (= [] (:axioms absent)))
    (is (= {:phase :catchup :context-loaded? false :error "Catchup failed: boom"} failed))
    (is (and (m/validate ok-arm loaded) (not (m/validate err-arm loaded))))
    (is (and (m/validate err-arm failed) (not (m/validate ok-arm failed))))))

;; =============================================================================
;; silence / abstract
;; =============================================================================

(deftest silence-arms
  (is (= {:phase :silence :observations [{:type :task-description :content "t"}] :files-read 0
          :discoveries 0 :silence-started "T0" :silence-iterations 1 :task "t"}
         (h/handle-silence clock {:task "t"})))
  (let [out (h/handle-silence (assoc clock :explore-fn (fn [_ _ obs] {:observations (conj obs :o) :files-read 2}))
                              {:task "t" :silence-iterations 1 :observations [] :silence-started "S"})]
    (is (= [2 [:o] "S"] [(:silence-iterations out) (:observations out) (:silence-started out)])))
  (is (= "Silence failed: boom" (:error (h/handle-silence (assoc clock :explore-fn boom) {})))))

(deftest abstract-arms
  (is (= "No synthesize-fn provided in resources" (:error (h/handle-abstract clock {}))))
  (let [nil-plan (h/handle-abstract (assoc clock :synthesize-fn (constantly nil)) {})]
    (is (= [nil "Synthesis returned nil plan" 1] ((juxt :plan :error :abstract-retries) nil-plan))))
  (is (= {:steps [1]} (:plan (h/handle-abstract (assoc clock :synthesize-fn (constantly {:steps [1]}))
                                                {:abstract-retries 1}))))
  (is (= "Synthesis failed: boom" (:error (h/handle-abstract (assoc clock :synthesize-fn boom) {})))))

;; =============================================================================
;; validate / store / act
;; =============================================================================

(deftest validate-plan-arms
  (is (true? (:plan-valid? (h/handle-validate-plan {} {:plan {:steps [1]}}))))
  (is (= ["Plan has no steps"] (:validation-errors (h/handle-validate-plan {} {:plan {}}))))
  (is (= {:phase :validate-plan :plan-valid? false
          :validation-errors ["Validation exception: boom"]}
         (h/handle-validate-plan {:validate-plan-fn boom} {}))))

(deftest store-plan-arms
  (let [skip {:phase :store-plan :plan-memory-id nil :kanban-task-ids [] :kg-edges-created 0}]
    (is (= skip (h/handle-store-plan {} {})))
    (is (= skip (h/handle-store-plan {:store-plan-fn boom} {})) "degrade is the skip shape")
    (is (= {:phase :store-plan :plan-memory-id "m" :kanban-task-ids ["k"] :kg-edges-created 2}
           (h/handle-store-plan {:store-plan-fn (constantly {:memory-id "m" :kanban-ids ["k"] :kg-edges 2})} {})))))

(deftest act-arms
  (let [seen (atom nil)
        out (h/handle-act-dispatch (assoc clock :dispatch-fn (fn [& args] (reset! seen args) {:wave-id "w" :result :r}))
                                   {:plan :p :agent-id "a" :run-id "run" :plan-memory-id "m"})]
    (is (= [:p :direct "a" {:run-id "run" :plan-memory-id "m"}] @seen))
    (is (= ["w" :r :direct "T0"] ((juxt :wave-id :execution-result :execution-mode :act-started) out))))
  (is (= {:status :no-dispatch-fn} (:execution-result (h/handle-act-dispatch clock {:execution-mode :wave}))))
  (is (= "Dispatch failed: boom" (:error (h/handle-act-dispatch (assoc clock :dispatch-fn boom) {}))))
  (is (true? (:tests-passed? (h/handle-act-verify clock {}))))
  (is (= [true :d] ((juxt :tests-passed? :verification)
                    (h/handle-act-verify (assoc clock :verify-fn (constantly {:passed? 1 :details :d})) {}))))
  (is (= [false {:error "boom"}] ((juxt :tests-passed? :verification)
                                  (h/handle-act-verify (assoc clock :verify-fn boom) {})))))

;; =============================================================================
;; terminals
;; =============================================================================

(deftest terminal-handlers
  (is (= {:agent-id "a" :phase :end} (h/handle-end {} {:data {:agent-id "a" :phase :end :junk 1}})))
  (is (= :built (h/handle-end {:build-summary-fn (constantly :built)} {:data {}})))
  (is (= {:phase :x :agent-id "a" :task nil :data {:phase :x :agent-id "a"} :error "e"}
         (h/handle-error {:error-response-fn identity} {:error "e" :data {:phase :x :agent-id "a"}})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"SAA workflow error"
                        (h/handle-error {} {:error "e" :data {}}))))
