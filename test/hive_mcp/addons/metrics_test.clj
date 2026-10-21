(ns hive-mcp.addons.metrics-test
  "hive-mcp.addons.metrics against a recording stub recorder."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.metrics :as am]
            [hive-mcp.addons.runtime-ports :as rp]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- recording
  "A stub recorder and the atom of [k elapsed-ns outcome] rows it records."
  []
  (let [rows (atom [])]
    [(fn [k ns oc] (swap! rows conj [k ns oc])) rows]))

(defn- with-stub
  "Call (F rows) with a stub recorder installed; rows is the atom it fills."
  [f]
  (let [[rec rows] (recording)]
    (binding [am/*recorder* (constantly rec)]
      (f rows))))

(deftest outcome-classifies-answers
  (is (= :ok (am/outcome {:type "text" :text "x"})))
  (is (= :ok (am/outcome nil)))
  (is (= :error (am/outcome {:isError true})))
  (is (= :error (am/outcome {:success? false})))
  (is (= :ok (am/outcome {:success? true}))))

(deftest timed-call-records-ok-and-value
  (with-stub
    (fn [rows]
      (is (= 42 (am/timed-call [:addon/init "probe.a"] (constantly 42))))
      (let [[[k ns oc]] @rows]
        (is (= [:addon/init "probe.a"] k))
        (is (and (int? ns) (>= ns 0)))
        (is (= :ok oc))))))

(deftest timed-call-records-error-answer
  (with-stub
    (fn [rows]
      (am/timed-call [:addon/tool "a" "t"] (constantly {:isError true}))
      (is (= :error (last (first @rows)))))))

(deftest timed-call-rethrows-and-records-error
  (with-stub
    (fn [rows]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                            (am/timed-call [:addon/tool "a" "t"] #(throw (ex-info "boom" {})))))
      (is (= [[:addon/tool "a" "t"] :error]
             [(ffirst @rows) (last (first @rows))])))))

(deftest recorder-failure-never-fails-the-call
  (binding [am/*recorder* (constantly (fn [_ _ _] (throw (RuntimeException. "sink down"))))]
    (is (= :v (am/timed-call [:addon/init "x"] (constantly :v))))))

(deftest no-recorder-is-a-pass-through
  (binding [am/*recorder* (constantly nil)]
    (let [h (fn [_] :ran)]
      (is (identical? h (am/wrap-handler [:addon/tool "a" "t"] h)))
      (is (= :ran (am/timed-call [:addon/init "x"] #(h nil)))))))

(deftest wrap-handler-times-every-call
  (with-stub
    (fn [rows]
      (let [h (am/wrap-handler [:addon/tool "hive.carto" "code carto"] (fn [p] (:x p)))]
        (is (= 1 (h {:x 1})))
        (is (= 2 (h {:x 2})))
        (is (= 2 (count @rows)))
        (is (every? #(= [:addon/tool "hive.carto" "code carto"] (first %)) @rows))))))

(deftest wrap-handler-keeps-fn-metadata
  (with-stub
    (fn [_rows]
      (let [h (with-meta (fn [_] :x) {::tag 1})]
        (is (= {::tag 1} (meta (am/wrap-handler [:addon/tool "a" "t"] h))))))))

(deftest label-is-a-short-string
  (is (= "-" (am/label nil)))
  (is (= "hive.carto" (am/label "hive.carto")))
  (is (= "hive/carto" (am/label :hive/carto)))
  (is (= "carto" (am/label :carto))))

(deftest invoke-registered-tool-is-timed
  (with-stub
    (fn [rows]
      (let [catalog (constantly [{:name "memory" :handler (fn [a] {:echo a})}])]
        (is (= {:echo {:q 1}} (rp/invoke-registered-tool! catalog "memory" {:q 1})))
        (is (= [:addon/invoke "memory"] (ffirst @rows)))
        (is (= :ok (last (first @rows))))))))
