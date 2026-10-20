(ns hive-mcp.protocols.workflow-contract-test
  "Contract facets for the workflow-engine slot, its predicates and the
   capability summary. Every test owns the slot and puts the prior engine back."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.workflow :as wf]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.workflow.engine :as engine]
            [hive-test.golden :as golden])
  (:import [hive_mcp.protocols.workflow NoopWorkflowEngine]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-slot [f]
  (let [prior (when (wf/workflow-engine-set?) (wf/get-workflow-engine))]
    (wf/clear-workflow-engine!)
    (try (f)
         (finally (wf/clear-workflow-engine!)
                  (when prior (wf/set-workflow-engine! prior))))))

(use-fixtures :each own-the-slot)

(defrecord StubEngine []
  engine/IWorkflowEngine
  (load-workflow [_ n _] {:success? true :workflow {:name n}})
  (validate-workflow [_ _] {:valid? true :errors []})
  (execute-step [_ _ _ _] {:success? true})
  (execute-workflow [_ _ _] {:success? true})
  (get-status [_ _] {:status :done})
  (cancel-workflow [_ _ _] {:success? true})
  engine/IWorkflowPersistence
  (save-state [_ _ _] true)
  (load-state [_ _] nil)
  (list-workflows [_ _] []))

(def ^:private AnyEngine
  [:or wf/WorkflowEngine
   [:fn {:gen/schema :nil :gen/fmap (fn [_] (->StubEngine))} #(instance? StubEngine %)]
   :int :string])

(deftrifecta-from-schema workflow-engine-pred-recognises-engines
  #'wf/workflow-engine?
  {:in [:cat AnyEngine]
   :out :boolean
   :rel (fn [[x] out] (= out (or (instance? NoopWorkflowEngine x) (instance? StubEngine x))))
   :num-tests 60})

(deftrifecta-from-schema only-the-persistent-stub-is-persistent
  #'wf/persistent-engine?
  {:in [:cat AnyEngine]
   :out :boolean
   :rel (fn [[x] out] (= out (instance? StubEngine x)))
   :classify (fn [_ out] (if out :persistent :not))
   :classify-domain #{:persistent :not}
   :num-tests 60})

(golden/deftest-golden slot-lifecycle-golden
  "test/golden/hive-mcp/protocols/workflow-lifecycle.edn"
  (let [before {:set? (wf/workflow-engine-set?) :enhanced? (wf/enhanced?)
                :noop? (instance? NoopWorkflowEngine (wf/get-workflow-engine))
                :caps (wf/capabilities)}
        installed (wf/set-workflow-engine! (->StubEngine))
        during {:set? (wf/workflow-engine-set?) :enhanced? (wf/enhanced?)
                :same? (identical? installed (wf/get-workflow-engine))
                :caps (wf/capabilities)}
        cleared (wf/clear-workflow-engine!)]
    {:before before :during during :cleared cleared
     :after {:set? (wf/workflow-engine-set?) :caps (wf/capabilities)}}))

(deftest set-rejects-a-non-engine
  (is (thrown? AssertionError (wf/set-workflow-engine! {:engine :fake})))
  (is (false? (wf/workflow-engine-set?))))
