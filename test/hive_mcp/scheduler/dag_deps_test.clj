(ns hive-mcp.scheduler.dag-deps-test
  "Where DAG dependencies come from: the card's own `dep:<task-id>` tags and
   the KG, never 'no dependencies' because a read failed."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.scheduler.dag-waves :as dag]
            [hive-mcp.knowledge-graph.edges :as kg-edges]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private get-deps #'dag/get-task-dependencies)

(defn- edge [to] {:kg-edge/relation :depends-on :kg-edge/to to})

(deftest card-dependencies-read-dep-tags
  (is (= #{"a" "b"} (dag/card-dependencies {:tags ["kanban" "dep:a" "dep:b" "wave:1"]})))
  (is (= #{"a"} (dag/card-dependencies {:tags [:dep:a]})) "a keyword tag reads the same")
  (is (= #{} (dag/card-dependencies {:tags ["kanban"]})))
  (is (= #{} (dag/card-dependencies {:tags ["dep:"]})) "an empty id is no dependency")
  (is (= #{} (dag/card-dependencies nil))))

(deftest dependencies-are-the-union-of-card-and-kg
  (with-redefs [kg-edges/get-edges-from (fn [_] [(edge "from-kg")])]
    (is (= #{"from-card" "from-kg"}
           (get-deps "t" {:tags ["dep:from-card"]})))))

(deftest a-card-alone-answers-when-the-kg-is-unreachable
  (with-redefs [kg-edges/get-edges-from (fn [_] (throw (ex-info "kg timeout" {})))]
    (is (= #{"from-card"} (get-deps "t" {:tags ["dep:from-card"]})))))

(deftest an-unreadable-kg-with-no-card-deps-is-unknown-not-empty
  (with-redefs [kg-edges/get-edges-from (fn [_] (throw (ex-info "kg timeout" {})))]
    (is (not (set? (get-deps "t" {:tags ["kanban"]}))))))

(deftest find-ready-tasks-skips-a-task-whose-deps-are-unreadable
  (with-redefs [kg-edges/get-edges-from (fn [_] (throw (ex-info "kg timeout" {})))
                hive-mcp.scheduler.dag-waves/get-kanban-todos
                (fn [_] [{:id "t" :title "T" :tags ["kanban"]}])]
    (is (= [] (dag/find-ready-tasks "/tmp" #{} {} #{})))))

(deftest a-readable-kg-with-no-edges-is-an-empty-set
  (with-redefs [kg-edges/get-edges-from (fn [_] [])]
    (is (= #{} (get-deps "t" {:tags ["kanban"]})))))
