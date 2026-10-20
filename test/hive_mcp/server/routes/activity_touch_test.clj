;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(ns hive-mcp.server.routes.activity-touch-test
  "REGISTRY-STALE-HIDES-LIVE: a ling's tool call must refresh its registry
   row's :slave/last-active-at, otherwise get-all-slaves hides a live
   claude/vterm ling after the 30 min staleness window."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.routes.middleware :as mw]))

(use-fixtures :each (fn [t] (mw/reset-activity-throttle!) (t) (mw/reset-activity-throttle!)))

(defn- run-call
  "Run one tool call through wrap-handler-context with a fake registry."
  [rows touches args]
  (binding [mw/*identity-get-slave* (fn [id] (get rows id))
            mw/*touch-slave!*       (fn [id updates] (swap! touches conj [id updates]))]
    ((mw/wrap-handler-context (fn [_] [{:type "text" :text "ran"}])) args)))

(deftest tool-call-from-a-ling-bumps-last-active-at
  (testing "caller `<slave-id>:<pid>` resolves to the slave row and touches it"
    (let [touches (atom [])
          before  (System/currentTimeMillis)
          rows    {"he-cider-boot" {:slave/id "he-cider-boot" :slave/alive? true
                                    :slave/last-active-at 1}}]
      (run-call rows touches {"command" "status" "_caller_id" "he-cider-boot:2534991"})
      (is (= 1 (count @touches)))
      (let [[id updates] (first @touches)]
        (is (= "he-cider-boot" id))
        (is (<= before (:slave/last-active-at updates)))))))

(deftest touch-is-throttled-per-slave
  (let [touches (atom [])
        rows    {"ling-a" {:slave/id "ling-a" :slave/alive? true}}]
    (dotimes [_ 5]
      (run-call rows touches {"_caller_id" "ling-a:1"}))
    (is (= 1 (count @touches)) "many calls within the throttle window write once")))

(deftest no-touch-without-a-registered-slave
  (let [touches (atom [])]
    (testing "coordinator / unknown callers write nothing"
      (run-call {} touches {"_caller_id" "coordinator:42"})
      (run-call {} touches {"_caller_id" "unknown:7"})
      (run-call {} touches {}))
    (is (empty? @touches))))

(deftest dead-rows-are-not-resurrected
  (let [touches (atom [])]
    (run-call {"ling-d" {:slave/id "ling-d" :slave/alive? false}} touches
              {"_caller_id" "ling-d:9"})
    (is (empty? @touches))))

(deftest a-failing-touch-never-fails-the-call
  (binding [mw/*identity-get-slave* (fn [_] {:slave/id "x" :slave/alive? true})
            mw/*touch-slave!*       (fn [_ _] (throw (ex-info "registry down" {})))]
    (is (= "ran" (-> ((mw/wrap-handler-context (fn [_] [{:type "text" :text "ran"}]))
                      {"_caller_id" "x:1"})
                     first :text)))))
