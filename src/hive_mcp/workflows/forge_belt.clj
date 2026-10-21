(ns hive-mcp.workflows.forge-belt
  "Forge belt workflow stubs.

   Delegates to extension if available. Returns noop defaults otherwise.

   Extension points are resolved via the extensions registry at startup.
   When no extensions are registered, all functions gracefully degrade
   to empty/noop results."

  (:require [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]
            [malli.core :as m]
            [hive-mcp.workflows.schema :as wfs]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension Delegation Helpers
;; =============================================================================

(defn- delegate-or-noop
  "Try to delegate to extension fn, fall back to default value."
  [ext-key default-val args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (do
      (log/warn "forge-belt extension not registered for" ext-key
                "— returning noop. Check register-forge-belt-defaults! startup logs.")
      default-val)))

;; =============================================================================
;; Noop Defaults
;; =============================================================================

(def ^:private noop-result
  {:success       false
   :message       "Extension not available"
   :strike-count  0
   :total-smited  0
   :total-sparked 0
   :last-strike   nil
   :quenched?     false
   :continuous?   false
   :smite-result  {:smited [] :failed [] :count 0}
   :survey-result {:tasks [] :count 0}
   :spark-result  {:spawned [] :failed [] :count 0}})

(def BeltResult
  "What a belt run answers: the extension's summary, or noop-result. An
   extension's :fb/run may answer a halted FSM state or nil, so only map-or-nil
   is promised."
  [:maybe :map])

;; =============================================================================
;; Dispatch Predicates — delegates to extension or returns noop
;; =============================================================================

(defn quenched?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q1 false [data]))

(m/=> quenched? [:=> [:cat wfs/Data] wfs/Truthy])

(defn has-tasks?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q2 false [data]))

(m/=> has-tasks? [:=> [:cat wfs/Data] wfs/Truthy])

(defn no-tasks?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q3 true [data]))

(m/=> no-tasks? [:=> [:cat wfs/Data] wfs/Truthy])

(defn continuous?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q4 false [data]))

(m/=> continuous? [:=> [:cat wfs/Data] wfs/Truthy])

(defn single-shot?
  "Delegates to extension if available."
  [data]
  (delegate-or-noop :fb/q5 true [data]))

(m/=> single-shot? [:=> [:cat wfs/Data] wfs/Truthy])

(defn always
  "Trivial predicate. Returns true for any input."
  [_data]
  true)

(m/=> always [:=> [:cat :any] [:= true]])

;; =============================================================================
;; Handlers — delegates to extension or returns noop
;; =============================================================================

(defn handle-start
  "Delegates to extension if available."
  [resources data]
  (delegate-or-noop :fb/h1 data [resources data]))

(m/=> handle-start [:=> [:cat wfs/Resources wfs/Data] wfs/Data])

(defn handle-smite
  "Delegates to extension if available."
  [resources data]
  (delegate-or-noop :fb/h2
                    (assoc data :smite-result {:smited [] :failed [] :count 0})
                    [resources data]))

(m/=> handle-smite [:=> [:cat wfs/Resources wfs/Data] [:map [:smite-result :any]]])

(defn handle-survey
  "Delegates to extension if available."
  [resources data]
  (delegate-or-noop :fb/h3
                    (assoc data :survey-result {:tasks [] :count 0})
                    [resources data]))

(m/=> handle-survey [:=> [:cat wfs/Resources wfs/Data] [:map [:survey-result :any]]])

(defn handle-spark
  "Delegates to extension if available."
  [resources data]
  (delegate-or-noop :fb/h4
                    (assoc data :spark-result {:spawned [] :failed [] :count 0})
                    [resources data]))

(m/=> handle-spark [:=> [:cat wfs/Resources wfs/Data] [:map [:spark-result :any]]])

(defn handle-end
  "Delegates to extension if available."
  [_resources fsm]
  (delegate-or-noop :fb/h5 {} [_resources fsm]))

(m/=> handle-end [:=> [:cat wfs/Resources wfs/Fsm] [:maybe :map]])

(defn handle-halt
  "Delegates to extension if available."
  [_resources fsm]
  (delegate-or-noop :fb/h6 fsm [_resources fsm]))

(m/=> handle-halt [:=> [:cat wfs/Resources wfs/Fsm] wfs/Fsm])

(defn handle-error
  "Delegates to extension if available."
  [_resources fsm]
  (delegate-or-noop :fb/h7 nil [_resources fsm]))

(m/=> handle-error [:=> [:cat wfs/Resources wfs/Fsm] :any])

;; =============================================================================
;; Subscription Handlers — delegates to extension or returns noop
;; =============================================================================

(defn on-smite-count-change
  "Delegates to extension if available."
  [path old-value new-value]
  (delegate-or-noop :fb/s1 nil [path old-value new-value]))

(m/=> on-smite-count-change [:=> [:cat [:sequential :any] :any :any] :any])

(defn on-spark-count-change
  "Delegates to extension if available."
  [path old-value new-value]
  (delegate-or-noop :fb/s2 nil [path old-value new-value]))

(m/=> on-spark-count-change [:=> [:cat [:sequential :any] :any :any] :any])

;; =============================================================================
;; FSM Spec — delegates to extension or returns nil
;; =============================================================================

(def forge-belt-spec
  "Delegates to extension if available."
  nil)

;; =============================================================================
;; Compilation & Execution API — delegates to extension or returns noop
;; =============================================================================

(defn compile-belt
  "Delegates to extension if available."
  []
  (delegate-or-noop :fb/compile nil []))

(m/=> compile-belt [:=> [:cat] [:maybe :map]])

(defn run-belt
  "Delegates to extension if available."
  ([compiled-fsm resources]
   (run-belt compiled-fsm resources {}))
  ([compiled-fsm resources opts]
   (delegate-or-noop :fb/run noop-result [compiled-fsm resources opts])))

(m/=> run-belt
  [:function
   [:=> [:cat [:maybe :map] wfs/Resources] BeltResult]
   [:=> [:cat [:maybe :map] wfs/Resources [:maybe :map]] BeltResult]])

(defn run-single-strike
  "Delegates to extension if available."
  [resources]
  (delegate-or-noop :fb/strike noop-result [resources]))

(m/=> run-single-strike [:=> [:cat wfs/Resources] BeltResult])

(defn run-continuous-belt
  "Delegates to extension if available."
  [resources]
  (delegate-or-noop :fb/cont noop-result [resources]))

(m/=> run-continuous-belt [:=> [:cat wfs/Resources] BeltResult])
