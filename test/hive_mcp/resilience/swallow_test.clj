(ns hive-mcp.resilience.swallow-test
  "Contract facets for hive-mcp.resilience.swallow: the value passes through,
   a failure answers nil and is logged through the hive-spi port (observed with
   an installed capture logger, nothing redefined)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.resilience.swallow :as swallow]
            [hive-spi.log.ports :as log-ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- capture-logger [sink]
  (reify log-ports/ILogger
    (log-event [_ level message] (swap! sink conj [level message]) nil)
    (logger-levels [_] #{:debug :info :warn :error})))

(defn- with-capture [f]
  (let [sink (atom [])]
    (log-ports/set-logger! (capture-logger sink))
    (try
      (f sink)
      (finally (log-ports/clear-logger!)))))

(use-fixtures :each (fn [t] (try (t) (finally (log-ports/clear-logger!)))))

(deftest a-value-passes-through-unlogged
  (with-capture
    (fn [sink]
      (is (= 3 (swallow/attempt (+ 1 2))))
      (is (nil? (swallow/attempt nil)))
      (is (= [] @sink)))))

(deftest a-throw-answers-nil-and-is-logged-at-debug
  (with-capture
    (fn [sink]
      (is (nil? (swallow/attempt (throw (ex-info "boom" {})))))
      (is (= 1 (count @sink)))
      (let [[level msg] (first @sink)]
        (is (= :debug level))
        (is (str/includes? msg "boom"))
        (is (str/includes? msg "ExceptionInfo"))
        (is (str/includes? msg "throw"))))))

(deftest errors-are-swallowed-too
  (with-capture
    (fn [sink]
      (is (nil? (swallow/attempt (throw (AssertionError. "invariant")))))
      (is (str/includes? (second (first @sink)) "invariant")))))

(deftest an-interrupt-restores-the-flag
  (testing "InterruptedException is swallowed but the thread stays interrupted"
    (with-capture
      (fn [_]
        (is (nil? (swallow/attempt (throw (InterruptedException. "stop")))))
        (is (true? (Thread/interrupted)))))))

(deftest the-label-is-clipped
  (is (<= (count (swallow/clip (apply str (repeat 500 "x")))) 83))
  (is (= "short" (swallow/clip "short"))))

(deftest attempt-ex-swallows-exceptions-but-not-errors
  (with-capture
    (fn [sink]
      (is (= 3 (swallow/attempt-ex (+ 1 2))))
      (is (nil? (swallow/attempt-ex (throw (ex-info "bad" {})))))
      (is (str/includes? (second (first @sink)) "bad"))
      (is (thrown? AssertionError
                   (swallow/attempt-ex (throw (AssertionError. "fatal"))))))))

(deftest attempt-star-takes-a-thunk
  (is (= :v (swallow/attempt* "label" (fn [] :v))))
  (is (nil? (swallow/attempt* "label" (fn [] (throw (RuntimeException. "x")))))))
