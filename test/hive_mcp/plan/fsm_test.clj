(ns hive-mcp.plan.fsm-test
  "Facets for the plan lifecycle FSM: trifectas on the pure guards and the
   status-tagging handlers, and goldens over the full lifecycle with the
   :execute-fn resource injected as a stub (no kanban, no KG). The plan field
   registry is snapshotted and emptied around each test so handle-validate
   sees core fields only."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.plan.field-registry :as fr]
            [hive-mcp.plan.fsm :as fsm]
            [hive-mcp.plan.schema :as schema]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out-schema [fsym]
  (-> (m/function-schemas) (get-in ['hive-mcp.plan.fsm fsym :schema]) m/children second))

(defn- registry-fixture [f]
  (let [saved @@#'fr/registry]
    (fr/reset-registry!)
    (try (f) (finally (reset! @#'fr/registry saved)))))

(use-fixtures :each registry-fixture)

;; -----------------------------------------------------------------------------
;; Guards
;; -----------------------------------------------------------------------------

(def ^:private TaskStatuses
  ;; A uniform draw is all-done in under 10% of cases; on a coin flip mark
  ;; every task done so both the done and the pending branch are reached.
  [:map {:gen/schema [:tuple [:map [:task-statuses {:optional true}
                                    [:map-of {:gen/max 3} [:enum "t1" "t2" "t3"] [:enum "done" "todo"]]]]
                      :boolean]
         :gen/fmap (fn [[m all-done?]]
                     (if all-done?
                       (update m :task-statuses #(update-vals (or % {"t1" "todo"}) (constantly "done")))
                       m))}
   [:task-statuses {:optional true}
    [:map-of {:gen/max 3} [:enum "t1" "t2" "t3"] [:enum "done" "todo"]]]])

(hst/deftrifecta-from-schema all-tasks-done-contract
  hive-mcp.plan.fsm/all-tasks-done?
  {:in [:cat TaskStatuses]
   ;; contract output is :any; the body is (and (seq ..) (every? ..)) => nil|boolean
   :out [:maybe :boolean]
   :rel (fn [[{:keys [task-statuses]}] out]
          (= (boolean out)
             (and (boolean (seq task-statuses))
                  (every? #{"done"} (vals task-statuses)))))
   :classify (fn [_ out] (if out :done :pending))
   :classify-domain #{:done :pending}
   :golden-path "test/golden/hive-mcp/plan/fsm-all-tasks-done.edn"})

(hst/deftrifecta-from-schema human-approved-contract
  hive-mcp.plan.fsm/human-approved?
  ;; :approved is one of three statuses, so both outcomes clear the floor.
  {:in [:cat [:map [:status [:enum :approved :draft :executing]]]]
   :out (out-schema 'human-approved?)
   :rel (fn [[{:keys [status]}] out] (= out (= :approved status)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/plan/fsm-human-approved.edn"})

(def ^:private FsmData
  [:map
   [:status {:optional true} [:enum :draft :validated]]
   [:plan-id {:optional true} [:enum "p1" "p2"]]])

(hst/deftrifecta-from-schema handle-approve-contract
  hive-mcp.plan.fsm/handle-approve
  {:in [:cat :nil FsmData]
   :out [:map [:status [:= :approved]]]
   :rel (fn [[_ data] out] (= out (assoc data :status :approved)))
   :golden-path "test/golden/hive-mcp/plan/fsm-handle-approve.edn"})

(hst/deftrifecta-from-schema handle-complete-contract
  hive-mcp.plan.fsm/handle-complete
  {:in [:cat :nil FsmData]
   :out [:map [:status [:= :completed]]]
   :rel (fn [[_ data] out] (= out (assoc data :status :completed)))
   :golden-path "test/golden/hive-mcp/plan/fsm-handle-complete.edn"})

;; -----------------------------------------------------------------------------
;; Handlers and lifecycle, with the execute port stubbed
;; -----------------------------------------------------------------------------

(def ^:private valid-plan schema/example-plan)
(def ^:private cyclic-plan
  {:id "p-cyc" :title "Cyclic"
   :steps [{:id "a" :title "A" :depends-on ["b"]} {:id "b" :title "B" :depends-on ["a"]}]})

(defn- stub-execute
  "Execute port stub: answers task ids derived from the plan's step ids, the
   shape run-plan-fsm's docstring promises an :execute-fn returns."
  [data]
  {:task-ids (mapv #(str "task-" (:id %)) (get-in data [:plan :steps]))
   :kg-edges []})

(defn- thrown-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         {:message (ex-message e)
          :data (select-keys (ex-data e) [:phase :error :status])})))

(golden/deftest-golden handlers-golden
  "test/golden/hive-mcp/plan/fsm-handlers.edn"
  {:draft-plan (fsm/handle-draft nil {:plan valid-plan})
   :draft-none (fsm/handle-draft nil {})
   :draft-content (select-keys (fsm/handle-draft nil {:content "{:id \"x\" :title \"X\" :steps [{:id \"s1\" :title \"S\"}]}"})
                               [:status :parse-format])
   :draft-bad (select-keys (fsm/handle-draft nil {:content "not a plan"}) [:status :parse-failed])
   :validate-ok (select-keys (fsm/handle-validate nil {:plan valid-plan}) [:status :valid?])
   :validate-cycle (-> (fsm/handle-validate nil {:plan cyclic-plan})
                       (select-keys [:status :valid? :validation])
                       (update :validation select-keys [:cycles]))
   :execute-stub (select-keys (fsm/handle-execute {:execute-fn stub-execute} {:plan valid-plan})
                              [:status :task-ids])
   :execute-none (fsm/handle-execute {} {:x 1})
   :execute-throws (fsm/handle-execute {:execute-fn (fn [_] (throw (ex-info "boom" {:errors ["e1"]})))} {:x 1})
   :guards {:schema-valid (boolean (fsm/schema-valid? {:plan valid-plan}))
            :schema-invalid (boolean (fsm/schema-valid? {:plan {:id 1}}))
            :has-tasks (boolean (fsm/has-tasks? {:plan valid-plan}))
            :no-tasks (boolean (fsm/has-tasks? {:plan {:steps []}}))}})

(golden/deftest-golden lifecycle-golden
  "test/golden/hive-mcp/plan/fsm-lifecycle.edn"
  {:run (select-keys (fsm/run-plan-fsm {:plan valid-plan} {:execute-fn stub-execute})
                     [:status :task-ids :valid?])
   :run-no-exec-fn (thrown-data #(fsm/run-plan-fsm {:plan valid-plan}))
   :run-cyclic (thrown-data #(fsm/run-plan-fsm {:plan cyclic-plan} {:execute-fn stub-execute}))
   :dry-run (select-keys (fsm/draft->validated {:plan valid-plan}) [:status :valid?])
   :dry-run-parse-fail (thrown-data #(fsm/draft->validated {:content "not a plan" :plan-id "p9"}))})

(deftest fsm-states-are-the-documented-lifecycle
  (is (= #{:draft :validated :approved :executing :completed} fsm/states)))
