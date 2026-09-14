(ns hive-mcp.tools.consolidated.workflow.forge-milestone-boundary-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.workflow.forge-ops :as ops]))

(def ^:private apply-filter #'ops/apply-milestone-boundary-filter)
(def ^:private p {:tasks [{:id "a"}] :count 1 :blocked-count 0})

(deftest enabled-with-no-filter-registered-passes-the-survey-through
  (testing "flag on and nothing registered must not blank the survey"
    (is (= p (apply-filter p {:enabled? true :filter-fn nil})))))

(deftest disabled-flag-passes-the-survey-through-even-with-a-filter
  (testing "flag off short-circuits before the filter runs"
    (is (= p (apply-filter p {:enabled? false :filter-fn (fn [_] {:tasks [] :excluded-count 1})})))))

(deftest enabled-with-filter-excludes-incomplete-milestone-tasks
  (testing "filter excludes tasks and counts them as blocked"
    (is (= {:tasks [] :count 0 :blocked-count 1}
           (apply-filter p {:enabled? true
                            :filter-fn (fn [_] {:tasks [] :excluded-count 1 :reason "x"})})))))

(deftest enabled-with-filter-keeping-all-tasks-leaves-counts-unchanged
  (testing "no exclusions means identical counts"
    (is (= {:tasks [{:id "a"}] :count 1 :blocked-count 0}
           (apply-filter p {:enabled? true
                            :filter-fn (fn [_] {:tasks [{:id "a"}]})})))))

(deftest filter-throwing-falls-back-to-the-prioritized-survey
  (testing "a crashing filter degrades to the unfiltered survey"
    (is (= p (apply-filter p {:enabled? true
                              :filter-fn (fn [_] (throw (ex-info "boom" {})))})))))
