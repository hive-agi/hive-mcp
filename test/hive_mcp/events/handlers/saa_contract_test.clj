(ns hive-mcp.events.handlers.saa-contract-test
  "Contract facets for the SAA workflow event handlers. Every generated event
   names its :agent-id, so no case falls back to the configured slave id."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.saa :as saa]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])
(def ^:private NoCoeffects [:map {:closed true}])
(def ^:private Phase [:enum :silence :silence-review :abstract :validate-plan :act-dispatch])
(def ^:private Score [:double {:min 0.0 :max 1.0}])
(def ^:private Mode [:enum "plan-only" "full"])

(deftrifecta-from-schema saa-started-announces-the-mode
  #'saa/handle-saa-started
  {:in [:cat NoCoeffects
        [:tuple [:= :saa/started]
         [:map [:agent-id Id] [:task [:string {:max 120}]]
          [:directory {:optional true} Id] [:plan-only? {:optional true} :boolean]]]]
   :out [:map
         [:log [:map [:level [:= :info]] [:message :string]]]
         [:shout [:map [:agent-id Id] [:event-type [:= :started]]
                  [:data [:map [:task :string] [:directory [:maybe Id]]
                          [:workflow [:= :saa]] [:mode Mode]]]]]]
   :rel (fn [[_ [_ {:keys [agent-id task directory plan-only?]}]] out]
          (let [mode (if plan-only? "plan-only" "full")]
            (and (= {:task task :directory directory :workflow :saa :mode mode}
                    (get-in out [:shout :data]))
                 (= agent-id (get-in out [:shout :agent-id]))
                 (= (str "SAA workflow started: " agent-id " task="
                         (subs task 0 (min 80 (count task))) " mode=" mode)
                    (get-in out [:log :message])))))
   :golden-path "test/golden/hive-mcp/events/handlers/saa-started.edn"
   :num-tests 100})

(deftrifecta-from-schema saa-phase-complete-carries-the-detail
  #'saa/handle-saa-phase-complete
  {:in [:cat NoCoeffects
        [:tuple [:= :saa/phase-complete]
         [:map [:agent-id Id] [:phase Phase]
          [:grounding-score {:optional true} Score]
          [:plan-valid? {:optional true} :boolean]
          [:silence-iterations {:optional true} [:int {:min 0 :max 5}]]
          [:abstract-retries {:optional true} [:int {:min 0 :max 3}]]]]]
   :out [:map
         [:log [:map [:level [:= :info]] [:message :string]]]
         [:shout [:map [:agent-id Id] [:event-type [:= :progress]]
                  [:data [:map [:workflow [:= :saa]] [:phase Phase] [:detail :map]]]]]
         [:channel-publish [:map [:event-type [:= :saa-phase-complete]]
                            [:data [:map [:agent-id Id] [:phase Phase]
                                    [:grounding-score [:maybe Score]]
                                    [:plan-valid? [:maybe :boolean]]]]]]]
   :rel (fn [[_ [_ {:keys [agent-id phase] :as data}]] out]
          (and (= (dissoc data :agent-id) (get-in out [:shout :data :detail]))
               (str/starts-with? (get-in out [:log :message])
                                 (str "SAA phase complete: " (name phase) " agent=" agent-id))))
   :golden-path "test/golden/hive-mcp/events/handlers/saa-phase-complete.edn"
   :num-tests 100})

(deftrifecta-from-schema saa-completed-summarises-the-run
  #'saa/handle-saa-completed
  {:in [:cat NoCoeffects
        [:tuple [:= :saa/completed]
         [:map [:agent-id Id] [:task Id]
          [:plan-memory-id {:optional true} Id]
          [:kanban-task-ids {:optional true} [:vector Id]]
          [:plan-only? {:optional true} :boolean]
          [:tests-passed? {:optional true} :boolean]
          [:grounding-score {:optional true} Score]]]]
   :out [:map
         [:log [:map [:level [:= :info]] [:message [:re #"^SAA complete \((full|plan-only)\).*"]]]]
         [:shout [:map [:agent-id Id] [:event-type [:= :completed]]
                  [:data [:map [:task Id] [:workflow [:= :saa]] [:mode Mode]]]]]
         [:channel-publish [:map [:event-type [:= :saa-completed]] [:data :map]]]]
   :rel (fn [[_ [_ data]] out]
          (and (= (select-keys data [:agent-id :task :plan-memory-id :kanban-task-ids
                                     :plan-only? :tests-passed? :grounding-score])
                  (get-in out [:channel-publish :data]))
               (= (if (:plan-only? data) "plan-only" "full") (get-in out [:shout :data :mode]))
               (str/ends-with? (get-in out [:log :message]) (str " agent=" (:agent-id data)))))
   :golden-path "test/golden/hive-mcp/events/handlers/saa-completed.edn"
   :num-tests 100})

(deftrifecta-from-schema saa-failed-emits-structured-telemetry
  #'saa/handle-saa-failed
  {:in [:cat NoCoeffects
        [:tuple [:= :saa/failed]
         [:map [:agent-id Id] [:task Id] [:phase Phase] [:error Id]
          [:data {:optional true} [:map [:attempt [:int {:min 1 :max 3}]]]]]]]
   :out [:map
         [:log [:map [:level [:= :error]] [:message :string]]]
         [:shout [:map [:agent-id Id] [:event-type [:= :error]] [:data :map]]]
         [:emit-system-error [:map [:error-type [:= :saa-workflow-failed]]
                              [:source [:re #"^saa/[a-z-]+$"]] [:message :string]
                              [:context [:map [:agent-id Id] [:task Id] [:phase Phase] [:data :map]]]]]
         [:channel-publish [:map [:event-type [:= :saa-failed]]
                            [:data [:map [:agent-id Id] [:phase Phase] [:error Id]]]]]]
   :rel (fn [[_ [_ {:keys [agent-id task phase error] :as data}]] out]
          (= {:error-type :saa-workflow-failed
              :source (str "saa/" (name phase))
              :message (str "SAA failed: " error)
              :context {:agent-id agent-id :task task :phase phase
                        :data (dissoc data :agent-id :task :phase :error)}}
             (:emit-system-error out)))
   :golden-path "test/golden/hive-mcp/events/handlers/saa-failed.edn"
   :num-tests 100})

(deftest register-handlers-installs-the-saa-events
  (ev/with-clean-registry
    (let [events [:saa/started :saa/phase-complete :saa/completed :saa/failed]
          reg (saa/register-handlers!)]
      (is (every? #(contains? reg %) events))
      (is (every? ev/handler-registered? events)))))
