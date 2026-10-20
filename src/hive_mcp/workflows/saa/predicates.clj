(ns hive-mcp.workflows.saa.predicates
  "Dispatch predicates and FSM interceptors for the SAA workflow.

   All predicates are pure functions of state data — no side effects.
   Used by the FSM spec (both inline and EDN) to determine state transitions.

   Also contains FSM pre/post interceptors for trace logging and
   no-op subscription handlers.

   DDD: Value Objects — pure data predicates, no domain logic."
  (:require [hive-mcp.workflows.support :as support]
            [malli.core :as m]
            [hive-mcp.workflows.schema :as wfs]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Dispatch Predicates (pure functions of state data)
;; =============================================================================

(def SaaData
  "SAA state data as the numeric dispatch predicates read it: the counters and
   the grounding score default when absent and are compared with < / >=, so a
   present value must be a number."
  [:maybe
   [:map
    [:grounding-score {:optional true} number?]
    [:grounding-threshold {:optional true} number?]
    [:silence-iterations {:optional true} number?]
    [:abstract-retries {:optional true} number?]
    [:plan-valid? {:optional true} :any]]])

(defn has-required-fields?
  "Check that agent-id and task are present, and no startup error."
  [data]
  (and (:agent-id data)
       (:task data)
       (not (:error data))))

(m/=> has-required-fields? [:=> [:cat wfs/Data] [:maybe :boolean]])

(defn has-startup-error?
  "Check for missing required fields or startup error."
  [data]
  (or (not (:agent-id data))
      (not (:task data))
      (some? (:error data))))

(m/=> has-startup-error? [:=> [:cat wfs/Data] :boolean])

(defn context-loaded?
  "Check if catchup loaded project context."
  [data]
  (true? (:context-loaded? data)))

(m/=> context-loaded? [:=> [:cat wfs/Data] :boolean])

(defn has-observations?
  "Check if Silence phase produced observations."
  [data]
  (some? (:observations data)))

(m/=> has-observations? [:=> [:cat wfs/Data] :boolean])

(defn has-error?
  "Check if data contains an error."
  [data]
  (some? (:error data)))

(m/=> has-error? [:=> [:cat wfs/Data] :boolean])

(defn grounding-sufficient?
  "Check if observation grounding score meets threshold."
  [data]
  (>= (get data :grounding-score 0.0)
      (get data :grounding-threshold 0.6)))

(m/=> grounding-sufficient? [:=> [:cat SaaData] :boolean])

(defn grounding-insufficient-retryable?
  "Check if grounding is insufficient but retries remain."
  [data]
  (and (< (get data :grounding-score 0.0)
          (get data :grounding-threshold 0.6))
       (< (get data :silence-iterations 0) 3)))

(m/=> grounding-insufficient-retryable? [:=> [:cat SaaData] :boolean])

(defn grounding-max-iterations?
  "Check if max silence iterations reached (proceed anyway)."
  [data]
  (>= (get data :silence-iterations 0) 3))

(m/=> grounding-max-iterations? [:=> [:cat SaaData] :boolean])

(defn has-plan?
  "Check if Abstract phase produced a plan."
  [data]
  (some? (:plan data)))

(m/=> has-plan? [:=> [:cat wfs/Data] :boolean])

(defn plan-valid?
  "Check if plan passed validation."
  [data]
  (true? (:plan-valid? data)))

(m/=> plan-valid? [:=> [:cat wfs/Data] :boolean])

(defn plan-invalid-retryable?
  "Check if plan is invalid but retries remain."
  [data]
  (and (not (:plan-valid? data))
       (< (get data :abstract-retries 0) 2)))

(m/=> plan-invalid-retryable? [:=> [:cat SaaData] :boolean])

(defn plan-invalid-final?
  "Check if plan is invalid and no retries remain."
  [data]
  (and (not (:plan-valid? data))
       (>= (get data :abstract-retries 0) 2)))

(m/=> plan-invalid-final? [:=> [:cat SaaData] :boolean])

(defn plan-only?
  "Check if we should skip Act phase (plan-only mode)."
  [data]
  (true? (:plan-only? data)))

(m/=> plan-only? [:=> [:cat wfs/Data] :boolean])

(defn full-execution?
  "Check if we should proceed to Act phase."
  [data]
  (not (:plan-only? data)))

(m/=> full-execution? [:=> [:cat wfs/Data] :boolean])

(defn has-execution-result?
  "Check if Act dispatch produced a result."
  [data]
  (some? (:execution-result data)))

(m/=> has-execution-result? [:=> [:cat wfs/Data] :boolean])

(defn tests-passed?
  "Check if verification succeeded."
  [data]
  (true? (:tests-passed? data)))

(m/=> tests-passed? [:=> [:cat wfs/Data] :boolean])

(defn tests-failed?
  "Check if verification failed."
  [data]
  (not (:tests-passed? data)))

(m/=> tests-failed? [:=> [:cat wfs/Data] :boolean])

(defn plan-nil-with-error?
  "Check if plan is nil AND an error occurred (abstract failed fatally)."
  [data]
  (and (nil? (:plan data)) (has-error? data)))

(m/=> plan-nil-with-error? [:=> [:cat wfs/Data] :boolean])

(defn context-not-loaded?
  "Complement of context-loaded? — catchup failed to load context."
  [data]
  (not (context-loaded? data)))

(m/=> context-not-loaded? [:=> [:cat wfs/Data] :boolean])

(def always
  "Dispatch predicate — always true. Shared seam (hive-mcp.workflows.support/always)."
  #'support/always)


;; =============================================================================
;; FSM Interceptors & Subscription Handlers
;; =============================================================================

(defn noop-subscription
  "No-op subscription handler. Placeholder for future monitoring."
  [_path _old _new]
  nil)

(m/=> noop-subscription [:=> [:cat :any :any :any] :nil])

(def trace-log-enter
  "Pre-interceptor: append :enter trace entry (clock-injectable via support/now-str)."
  #'support/trace-log-enter)

(def trace-log-exit
  "Post-interceptor: append :exit trace entry (clock-injectable via support/now-str)."
  #'support/trace-log-exit)
