(ns hive-mcp.events.handlers.resilience-test
  "Tests for :resilience/dim-mismatch handler (ENGINE-L1.4)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.events.handlers.resilience :as res]
            [hive.events :as ev]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [t]
    (res/reset-registration!)
    (try (t) (finally (res/reset-registration!)))))

;; =============================================================================
;; handle-dim-mismatch — pure handler
;; =============================================================================

(deftest test-handle-dim-mismatch-emits-log-effect
  (testing "produces :log effect with warn level + event key"
    (let [event [:resilience/dim-mismatch
                 {:message "dim drift" :details {:expected 384 :got 768}
                  :ex-class "java.lang.IllegalStateException"}]
          result (res/handle-dim-mismatch {} event)]
      (is (contains? result :log) "must produce :log effect")
      (is (= :warn (get-in result [:log :level])))
      (is (= :resilience/dim-mismatch (get-in result [:log :event]))))))

(deftest test-handle-dim-mismatch-carries-structured-fields
  (testing ":log effect carries message, ex-class, details verbatim"
    (let [data {:message "embedder bumped from 384 → 768"
                :details {:collection "memories" :expected 384 :got 768}
                :ex-class "clojure.lang.ExceptionInfo"}
          result (res/handle-dim-mismatch {} [:resilience/dim-mismatch data])]
      (is (= (:message data) (get-in result [:log :message])))
      (is (= (:ex-class data) (get-in result [:log :ex-class])))
      (is (= (:details data) (get-in result [:log :details])))
      (is (= data (get-in result [:log :data]))
          "raw data preserved for downstream telemetry sinks"))))

(deftest test-handle-dim-mismatch-tolerates-nil-fields
  (testing "handler does not throw on missing optional keys"
    (let [result (res/handle-dim-mismatch {} [:resilience/dim-mismatch {}])]
      (is (contains? result :log))
      (is (nil? (get-in result [:log :message])))
      (is (nil? (get-in result [:log :ex-class])))
      (is (nil? (get-in result [:log :details]))))))

;; =============================================================================
;; register-handlers! — idempotent registration guard
;; =============================================================================

(deftest test-register-handlers-re-registers
  (testing "every call registers, so a reload can rewire the handler"
    (is (true? (res/register-handlers!)) "first registration returns true")
    (is (true? (res/register-handlers!)) "and so does every call after it"))

  (testing "the registry holds what the namespace defines NOW"
    ;; This is the assertion the old test inverted. It used to demand that a
    ;; second call no-op and return nil, which froze the defect: the gate is a
    ;; `defonce`'d atom, so after a hot reload it still read true, this function
    ;; did nothing, and the registry kept dispatching to the closure compiled
    ;; before the reload. `reg-event` is addressed by key and last-writer-wins,
    ;; so re-registering is free and is the whole repair.
    ;; Kanban 20260916134011-1246379c.
    (res/register-handlers!)
    (is (identical? @(ns-resolve (find-ns 'hive-mcp.events.handlers.resilience)
                                 'handle-dim-mismatch)
                    (:handler (ev/get-event :resilience/dim-mismatch)))
        "a gate here would leave the pre-reload closure registered")))
