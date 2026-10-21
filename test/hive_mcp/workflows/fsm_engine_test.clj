(ns hive-mcp.workflows.fsm-engine-test
  "Facets for FSMWorkflowEngine: the IWorkflowEngine law suite run against
   the engine create-engine builds, over workflows this namespace installs in
   the registry, and a golden over a whole load / validate / execute / step /
   status / cancel round. The fixture snapshots the workflow registry's state
   atom and the engine's status atom, and restores both."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.protocols.workflow :as proto]
            [hive-mcp.workflows.fsm-engine :as engine]
            [hive-mcp.workflows.registry :as registry]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private two-step-spec
  {:fsm {:hive.events.fsm/start {:handler :start :dispatches [[:wf/work '(fn [_] true)]]}
         :wf/work {:handler :work :dispatches [[:hive.events.fsm/end '(fn [_] true)]]}
         :hive.events.fsm/end {:handler :end}
         :hive.events.fsm/error {:handler :error}}})

(def ^:private failing-spec
  {:fsm {:hive.events.fsm/start {:handler :boom :dispatches [[:hive.events.fsm/end '(fn [_] true)]]}
         :hive.events.fsm/end {:handler :end}
         :hive.events.fsm/error {:handler :error}}})

(def ^:private handlers
  {:start (fn [_ data] (assoc data :started true))
   :work (fn [_ data] (update data :n (fnil inc 0)))
   :boom (fn [_ _] (throw (ex-info "boom" {})))
   :end (fn [_ {:keys [data]}] data)
   :error (fn [_ fsm] (throw (ex-info "workflow failed" {:data (:data fsm)})))})

(defn- install-workflows! []
  (reset! @#'registry/registry {:two-step {:spec two-step-spec} :failing {:spec failing-spec}
                                :spec-only {:spec two-step-spec}})
  (registry/register-handlers! :two-step handlers)
  (registry/register-handlers! :failing handlers)
  (registry/compile-registry!))

(defn- registry-fixture [f]
  (let [saved-registry @@#'registry/registry
        saved-statuses @@#'engine/workflow-statuses]
    (install-workflows!)
    (try (f)
         (finally
           (reset! @#'registry/registry saved-registry)
           (reset! @#'engine/workflow-statuses saved-statuses)))))

(use-fixtures :each registry-fixture)

(def ^:private WorkflowName [:enum :two-step :failing :spec-only :missing "two-step"])

(deftest-port-contract workflow-engine-port-laws
  {:port/methods
   {proto/load-workflow   {:in  [:cat WorkflowName [:map [:scope {:optional true} [:enum "proj"]]]]
                           :out [:map [:workflow-id :string] [:name :string] [:steps [:vector :map]]
                                 [:loaded? :boolean] [:errors [:vector :string]]]
                           :rel (fn [[wf-name] out]
                                  (= (:loaded? out)
                                     (contains? #{:two-step :failing "two-step"} wf-name)))}
    proto/get-status      {:in  [:cat [:enum "never-loaded-id"]]
                           :out :nil}
    proto/cancel-workflow {:in  [:cat [:enum "never-loaded-id"] [:map]]
                           :out [:map [:success? [:= false]] [:status [:= :unknown]]]}}}
  (engine/create-engine))

(defn- strip
  "Drop the run-specific values (ids, timestamps, durations, compiled FSMs)."
  [m]
  (when (map? m)
    (dissoc m :workflow-id :duration-ms :_compiled :_spec :started-at :completed-at :result)))

(golden/deftest-golden fsm-engine-round-golden
  "test/golden/hive-mcp/workflows/fsm-engine-round.edn"
  (let [e (engine/create-engine)
        wf (proto/load-workflow e :two-step {:scope "proj"})
        validated (proto/validate-workflow e wf)
        pending (strip (proto/get-status e (:workflow-id wf)))
        executed (proto/execute-workflow e wf {:initial-data {:n 0}})
        completed (strip (proto/get-status e (:workflow-id wf)))
        ;; BUG (recorded, not fixed): execute-step cannot run a step of a
        ;; loaded workflow. load-workflow names steps (str state-id), e.g.
        ;; ":wf/work"; parse-step-id keeps that leading colon, so the state
        ;; is not found ("no handler for state"), and the colon-less "wf/work"
        ;; fails too (a NullPointerException inside fsm/step). Pinned as the
        ;; current behaviour; the NPE text is JVM-specific, so only the flag.
        stepped-own-id (proto/execute-step e wf (:step-id (first (:steps wf))) {:context {:n 5}})
        stepped-bare-id (proto/execute-step e wf "wf/work" {:context {:n 5}})
        cancel-done (strip (proto/cancel-workflow e (:workflow-id wf) {}))
        fresh (proto/load-workflow e :two-step {})
        cancelled (strip (proto/cancel-workflow e (:workflow-id fresh) {:reason "test"}))
        cancelled-status (:errors (proto/get-status e (:workflow-id fresh)))
        failing (proto/load-workflow e :failing {})
        failed (strip (proto/execute-workflow e failing {}))
        failed-status (:status (proto/get-status e (:workflow-id failing)))
        missing (proto/load-workflow e :missing {})
        unloaded (proto/validate-workflow e missing)
        reset (engine/reset-statuses!)]
    {:loaded (strip wf) :validated validated :pending pending
     :executed (update (strip executed) :results update-vals #(dissoc % :trace))
     :completed completed
     :stepped-own-id (select-keys stepped-own-id [:success? :step-id :context :errors])
     :stepped-bare-id (select-keys stepped-bare-id [:success? :step-id])
     :cancel-done cancel-done :cancelled cancelled :cancelled-status cancelled-status
     :failed failed :failed-status failed-status
     :missing (strip missing) :unloaded unloaded :reset reset}))
