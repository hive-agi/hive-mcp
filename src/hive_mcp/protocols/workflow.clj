(ns hive-mcp.protocols.workflow
  "Active-engine slot + Noop fallback for workflow execution engines.

   The PROTOCOLS themselves moved to hive-spi.workflow.engine (HWF2-D1b) so that
   hive-workflows can implement IWorkflowEngine without depending on hive-mcp
   (HWF2-M9). This namespace keeps every historical qualified name resolving for
   existing callers, and owns the two things that are NOT pure contract: the
   NoopWorkflowEngine impl and the mutable active-engine slot.

   The protocol and method vars below are plain `def` ALIASES of the hive-spi
   originals. Do NOT turn them back into `defprotocol` — a second `defprotocol`
   mints a DISTINCT protocol, and every record implementing the original then
   fails `satisfies?` silently."
  (:require [hive-mcp.protocols.registry :as reg]
            [hive-spi.workflow.engine :as engine]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; IWorkflowEngine — re-export from hive-spi.workflow.engine
;;; =============================================================================

(def IWorkflowEngine    engine/IWorkflowEngine)

(def load-workflow      engine/load-workflow)
(def validate-workflow  engine/validate-workflow)
(def execute-step       engine/execute-step)
(def execute-workflow   engine/execute-workflow)
(def get-status         engine/get-status)
(def cancel-workflow    engine/cancel-workflow)

;;; =============================================================================
;;; IWorkflowPersistence — re-export from hive-spi.workflow.engine
;;; =============================================================================

(def IWorkflowPersistence engine/IWorkflowPersistence)

(def save-state         engine/save-state)
(def load-state         engine/load-state)
(def list-workflows     engine/list-workflows)

;;; =============================================================================
;;; NoopWorkflowEngine (No-Op Fallback Implementation)
;;; =============================================================================

(defrecord NoopWorkflowEngine []
  engine/IWorkflowEngine

  (load-workflow [_ workflow-name _opts]
    {:workflow-id (str "noop-" workflow-name "-" (System/currentTimeMillis))
     :name workflow-name
     :steps []
     :metadata {:engine :noop}
     :loaded? false
     :errors ["NoopWorkflowEngine: No workflow engine configured. Set one via set-workflow-engine!"]})

  (validate-workflow [_ _workflow]
    {:valid? false
     :errors ["NoopWorkflowEngine: No workflow engine configured."]
     :warnings []
     :dependency-order []})

  (execute-step [_ _workflow step-id _opts]
    {:success? false
     :step-id step-id
     :result nil
     :duration-ms 0
     :errors ["NoopWorkflowEngine: No workflow engine configured."]
     :context {}})

  (execute-workflow [_ workflow _opts]
    {:success? false
     :workflow-id (:workflow-id workflow)
     :steps-completed 0
     :steps-total (count (:steps workflow))
     :results {}
     :duration-ms 0
     :errors ["NoopWorkflowEngine: No workflow engine configured."]
     :final-context {}})

  (get-status [_ workflow-id]
    {:workflow-id workflow-id
     :name nil
     :status :unknown
     :current-step nil
     :steps-completed 0
     :steps-total 0
     :started-at nil
     :completed-at nil
     :errors ["NoopWorkflowEngine: No workflow engine configured."]
     :progress 0.0})

  (cancel-workflow [_ workflow-id _opts]
    {:success? false
     :workflow-id workflow-id
     :status :unknown
     :steps-completed 0
     :errors ["NoopWorkflowEngine: No workflow engine configured."]}))

;;; =============================================================================
;;; Active Engine Management
;;; =============================================================================

(defonce ^:private slot
  (reg/single-slot {:validate #(satisfies? engine/IWorkflowEngine %)
                    :on-empty ->NoopWorkflowEngine}))

(defn set-workflow-engine!
  "Set the active workflow engine implementation."
  [engine]
  (reg/install! slot engine))

(def WorkflowEngine
  "Any value satisfying hive-spi.workflow.engine/IWorkflowEngine; generates
   the NoopWorkflowEngine."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (->NoopWorkflowEngine))}
   #(satisfies? engine/IWorkflowEngine %)])

;; The input is loose: the slot's :validate assert rejects a non-engine.
(m/=> set-workflow-engine! [:=> [:cat :any] WorkflowEngine])

(defn get-workflow-engine
  "Get the active workflow engine, or NoopWorkflowEngine if none set."
  []
  (reg/current slot))

(m/=> get-workflow-engine [:=> [:cat] WorkflowEngine])

(defn workflow-engine-set?
  "Check if an active workflow engine is configured."
  []
  (reg/present? slot))

(m/=> workflow-engine-set? [:=> [:cat] :boolean])

(defn clear-workflow-engine!
  "Clear the active workflow engine."
  []
  (reg/clear! slot))

(m/=> clear-workflow-engine! [:=> [:cat] :nil])

;;; =============================================================================
;;; Utility Functions
;;; =============================================================================

(defn workflow-engine?
  "Check if object implements IWorkflowEngine protocol."
  [x]
  (engine/workflow-engine? x))

(m/=> workflow-engine? [:=> [:cat :any] :boolean])

(defn persistent-engine?
  "Check if workflow engine supports persistence."
  [x]
  (engine/persistent-engine? x))

(m/=> persistent-engine? [:=> [:cat :any] :boolean])

(defn enhanced?
  "Check if a non-noop workflow engine is active."
  []
  (and (workflow-engine-set?)
       (not (instance? NoopWorkflowEngine (get-workflow-engine)))))

(m/=> enhanced? [:=> [:cat] :boolean])

(defn capabilities
  "Get a summary of available workflow capabilities."
  []
  (let [engine (get-workflow-engine)]
    {:engine-type (if (enhanced?)
                    (-> engine class .getSimpleName)
                    :noop)
     :enhanced? (enhanced?)
     :load? true            ;; Always available (may be no-op)
     :validate? true
     :execute-step? true
     :execute-workflow? true
     :get-status? true
     :cancel? true
     :persistence? (persistent-engine? engine)}))

(def WorkflowCapabilities
  "The summary `capabilities` answers: :engine-type is the engine's simple
   class name when a non-noop engine is active, :noop otherwise."
  [:map
   [:engine-type [:or [:= :noop] :string]]
   [:enhanced? :boolean]
   [:load? [:= true]]
   [:validate? [:= true]]
   [:execute-step? [:= true]]
   [:execute-workflow? [:= true]]
   [:get-status? [:= true]]
   [:cancel? [:= true]]
   [:persistence? :boolean]])

(m/=> capabilities [:=> [:cat] WorkflowCapabilities])
