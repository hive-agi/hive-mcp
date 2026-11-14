(ns hive-mcp.tools.memory.plan-test
  "Tests for the `memory plan` subdomain.

   The index and the store are stubbed: these tests are about the verbs'
   contract (hydrate only plans, refuse illegal edges, never answer from index
   metadata alone), not about Milvus."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [hive-mcp.plan.plans :as plans]
            [hive-mcp.tools.memory.plan :as plan]
            [hive-mcp.vectordb.facade :as facade]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- body [response] (json/read-str (:text response) :key-fn keyword))

(def ^:private plan-a
  {:id "p1" :type "plan" :content "1. one\n2. two" :plan-status "draft"
   :project-id "proj" :tags ["x"]})
(def ^:private note
  {:id "n1" :type "note" :content "not a plan"})

(deftest plan-list-hydrates-only-plans
  (with-redefs [plans/query-plans (fn [& _] [{:id "p1"} {:id "n1"} {:id "gone"}])
                facade/get-entry-by-id (fn [id] ({"p1" plan-a "n1" note} id))]
    (let [out (body (plan/handle-plan-list {}))]
      (is (= 1 (:count out)))
      (is (= ["p1"] (mapv :id (:results out))))
      (is (string? (:preview (first (:results out)))))
      (is (nil? (:content (first (:results out))))))))

(deftest plan-get-carries-content-and-refuses-notes
  (with-redefs [facade/get-entry-by-id (fn [id] ({"p1" plan-a "n1" note} id))]
    (is (= "1. one\n2. two" (:content (body (plan/handle-plan-get {:id "p1"})))))
    (is (:isError (plan/handle-plan-get {:id "n1"})))))

(deftest plan-search-requires-query
  (is (:isError (plan/handle-plan-search {}))))

(deftest plan-list-refuses-unknown-status
  (is (:isError (plan/handle-plan-list {:status "bogus"}))))

(deftest plan-update-walks-the-machine-and-mirrors-the-index
  (let [writes (atom []) mirror (atom [])]
    (with-redefs [facade/get-entry-by-id (fn [_] (assoc plan-a :plan-status "draft"))
                  facade/update-entry! (fn [id updates] (swap! writes conj [id updates]))
                  plans/update-plan-status! (fn [id to] (swap! mirror conj [id to]))]
      (let [out (body (plan/handle-plan-update {:id "p1" :status "active"}))]
        (is (true? (:changed out)))
        (is (= [["p1" {:plan-status "active"}]] @writes))
        (is (= [["p1" "active"]] @mirror))))))

(deftest plan-update-noop-does-not-write
  (let [writes (atom 0)]
    (with-redefs [facade/get-entry-by-id (fn [_] (assoc plan-a :plan-status "active"))
                  facade/update-entry! (fn [& _] (swap! writes inc))]
      (let [out (body (plan/handle-plan-update {:id "p1" :status "active"}))]
        (is (false? (:changed out)))
        (is (zero? @writes))))))

(deftest plan-update-refuses-completed-to-active
  (with-redefs [facade/get-entry-by-id (fn [_] (assoc plan-a :plan-status "completed"))]
    (let [out (plan/handle-plan-update {:id "p1" :status "active"})]
      (is (:isError out))
      (is (re-find #"refused" (:text out))))))

(deftest plan-archive-supersedes
  (with-redefs [facade/get-entry-by-id (fn [_] (assoc plan-a :plan-status "active"))
                facade/update-entry! (fn [& _] {:id "p1"})
                plans/update-plan-status! (fn [& _] "p1")]
    (let [out (body (plan/handle-plan-archive {:id "p1"}))]
      (is (= "superseded" (:plan-status out))))))
