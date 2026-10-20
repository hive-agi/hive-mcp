(ns hive-mcp.server.routes.async-task.state-test
  "Contract facets for the pure async-task lifecycle decisions. Every input is
   drawn from the subsystem's own value-object schemas, so the properties
   range over every TaskState x HandleView combination the registry can hold."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.server.routes.async-task.schema :as s]
            [hive-mcp.server.routes.async-task.state :as st]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.util :as mu]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Oracles — each restates the docstring, never calls the subject
;;; ---------------------------------------------------------------------------

(defn- expected-reported
  "The handle wins over the record, except where the record is terminal or
   the handle is absent."
  [recorded {:keys [present? done? cancelled?]}]
  (cond
    (contains? s/terminal-states recorded) recorded
    (not present?)                         recorded
    cancelled?                             :task/cancelled
    done?                                  :task/done
    :else                                  recorded))

(defn- expected-cancel-variant
  [entry]
  (cond
    (nil? entry)                                   :cancel/unknown-task
    (not (get-in entry [:handle :present?]))       :cancel/not-started
    (or (contains? s/terminal-states (:state entry))
        (get-in entry [:handle :done?]))           :cancel/already-finished
    :else                                          :cancel/interrupted))

;;; ---------------------------------------------------------------------------
;;; Test-domain schemas
;;; ---------------------------------------------------------------------------

(def ^:private Millis
  ;; epoch-millis sized, so `now - started-at` stays inside a long
  [:int {:min 0 :max 4102444800000}])

(def ^:private ClockedEntry (mu/assoc s/TaskEntry :started-at Millis))

(def ^:private LiveHandle
  [:map {:closed true}
   [:present? [:= true]]
   [:done? [:= false]]
   [:cancelled? :boolean]])

(def ^:private CancelTarget
  ;; a third of the entries carry a live handle on a live state, so the
  ;; :interrupted arm is reached as often as the others
  [:or
   :nil
   s/TaskEntry
   (-> s/TaskEntry
       (mu/assoc :handle LiveHandle)
       (mu/assoc :state [:enum :task/queued :task/running]))])

;;; ---------------------------------------------------------------------------
;;; Trifectas
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema terminal-is-membership-in-terminal-states
  #'st/terminal?
  {:in [:cat [:or s/TaskState :keyword :nil]]
   :out :boolean
   :rel (fn [[state] out] (= out (contains? s/terminal-states state)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

(deftrifecta-from-schema reported-state-lets-the-handle-win
  #'st/reported-state
  {:in [:cat s/TaskState s/HandleView]
   :out s/TaskState
   :rel (fn [[recorded handle] out] (= out (expected-reported recorded handle)))
   :golden-path "test/golden/hive-mcp/server/routes/async-task/reported-state.edn"
   :n-cases 16
   :num-tests 300})

(deftrifecta-from-schema cancel-outcome-decides-before-acting
  #'st/cancel-outcome
  {:in [:cat s/TaskId CancelTarget]
   :out s/CancelOutcome
   :rel (fn [[task-id entry] out]
          (and (= task-id (:task-id out))
               (= (expected-cancel-variant entry) (:adt/variant out))
               (or (not= :cancel/already-finished (:adt/variant out))
                   (= (expected-reported (:state entry) (:handle entry)) (:state out)))))
   :classify (fn [_ out] (:adt/variant out))
   :classify-domain s/CancelReason
   :num-tests 400})

(deftrifecta-from-schema summarize-reports-elapsed-and-state
  #'st/summarize
  {:in [:cat s/TaskId ClockedEntry Millis]
   :out s/TaskSummary
   :rel (fn [[task-id entry now] out]
          (and (= task-id (:task-id out))
               (= (- now (:started-at entry)) (:elapsed-ms out))
               (= (expected-reported (:state entry) (:handle entry)) (:state out))
               (= (:timeout-ms entry) (:timeout-ms out))))
   :num-tests 200})

(deftrifecta-from-schema completion-promotes-only-running
  #'st/completion-state
  {:in [:cat s/TaskState]
   :out s/TaskState
   :rel (fn [[recorded] out]
          (= out (if (= :task/running recorded) :task/done recorded)))
   :num-tests 100})

(deftrifecta-from-schema collectable-iff-reported-terminal
  #'st/collectable?
  {:in [:cat s/TaskEntry]
   :out :boolean
   :rel (fn [[entry] out]
          (= out (contains? s/terminal-states
                            (expected-reported (:state entry) (:handle entry)))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

(deftrifecta-from-schema stops-the-work-only-when-interrupted
  #'st/stops-the-work?
  {:in [:cat s/CancelOutcome]
   :out :boolean
   :rel (fn [[outcome] out] (= out (= :cancel/interrupted (:adt/variant outcome))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

;;; ---------------------------------------------------------------------------
;;; Golden — every cancel-outcome arm, driven directly
;;; ---------------------------------------------------------------------------

(def ^:private running-handle {:present? true :done? false :cancelled? false})

(defn- entry [state handle]
  {:tool "memory" :caller-id "c" :state state :started-at 100
   :timeout-ms nil :handle handle})

(golden/deftest-golden cancel-outcome-arms-golden
  "test/golden/hive-mcp/server/routes/async-task/cancel-outcome-arms.edn"
  (into (sorted-map)
        {:unknown         (st/cancel-outcome "t" nil)
         :not-started     (st/cancel-outcome "t" (entry :task/queued
                                                        {:present? false :done? false :cancelled? false}))
         :finished-record (st/cancel-outcome "t" (entry :task/timed-out running-handle))
         :finished-handle (st/cancel-outcome "t" (entry :task/running
                                                        {:present? true :done? true :cancelled? true}))
         :interrupted     (st/cancel-outcome "t" (entry :task/running running-handle))}))

(deftest every-cancel-variant-validates-and-only-interrupted-stops
  (doseq [variant (m/children s/CancelReason)]
    (let [v (s/async-cancel-outcome variant {:task-id "t"})]
      (is (m/validate s/CancelOutcome v) (str variant))
      (is (= (= :cancel/interrupted variant) (st/stops-the-work? v))))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage async-task-contracts
  {:paths ["src/hive_mcp/server/routes/async_task"]})

(deftest async-task-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/server/routes/async_task"]})]
    (is (= 7 (count (:universe report))))
    (is (= 7 (count (:covered report))))))

(deftest every-async-task-contract-generates-both-sides
  (doseq [[fn-sym {:keys [schema]}] (get (m/function-schemas) 'hive-mcp.server.routes.async-task.state)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (testing (str fn-sym)
      (is (= 3 (count (mg/sample side {:size 3 :seed 7})))))))
