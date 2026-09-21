(ns hive-mcp.session.registry-scope-test
  "The registry half of WRAP-SESSION-HCR, against a real DataScript store.

   The regression under test: before session tagging, one wrap's clear retracted
   EVERY row in the store, so whichever session wrapped first destroyed the
   other's unharvested records."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.session.identity :as sid]
            [hive-mcp.swarm.datascript.coordination.session-registry :as reg]
            [hive-mcp.swarm.datascript.coordination.wrap-queue :as wq]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ref-a
  (sid/session-ref {:id "s-a" :kind :coordinator :project-id "hive" :agent-id "coord-a"}))

(def ref-b
  (sid/session-ref {:id "s-b" :kind :coordinator :project-id "hive" :agent-id "coord-b"}))

(def parent-of {"s-a1" "s-a" "s-b1" "s-b"})

(defn- clean [f]
  (reg/clear-completed-tasks!)
  (reg/clear-kanban-movements!)
  (f)
  (reg/clear-completed-tasks!)
  (reg/clear-kanban-movements!))

(use-fixtures :each clean)

(defn- seed-two-sessions! []
  (reg/register-completed-task! "t-a"  {:title "A's task"      :project-id "hive" :session-id "s-a"})
  (reg/register-completed-task! "t-a1" {:title "A's ling task" :project-id "hive" :session-id "s-a1"})
  (reg/register-completed-task! "t-b"  {:title "B's task"      :project-id "hive" :session-id "s-b"})
  (reg/register-completed-task! "t-old" {:title "untagged"     :project-id "hive"}))

(deftest scoped-read-sees-only-own-subtree-test
  (seed-two-sessions!)
  (testing "coordinator A reads its own row and its ling's, never B's"
    (let [ids (set (map :completed-task/id
                        (reg/get-completed-tasks-this-session
                         :session-ref ref-a :parent-of parent-of)))]
      (is (= #{"t-a" "t-a1"} ids))))
  (testing "coordinator B reads only its own"
    (let [ids (set (map :completed-task/id
                        (reg/get-completed-tasks-this-session
                         :session-ref ref-b :parent-of parent-of)))]
      (is (= #{"t-b"} ids))))
  (testing "an untagged row belongs to nobody"
    (is (not (contains? (set (map :completed-task/id
                                  (reg/get-completed-tasks-this-session
                                   :session-ref ref-a :parent-of parent-of)))
                        "t-old")))))

(deftest unscoped-read-is-unchanged-test
  (seed-two-sessions!)
  (testing "a caller that passes no session-ref still sees everything, so
            existing callers keep working"
    (is (= 4 (count (reg/get-completed-tasks-this-session))))))

(deftest clearing-is-scoped-to-what-was-harvested-test
  (seed-two-sessions!)
  (let [harvested (map :completed-task/id
                       (reg/get-completed-tasks-this-session
                        :session-ref ref-a :parent-of parent-of))
        cleared   (reg/clear-completed-tasks! harvested)
        left      (set (map :completed-task/id (reg/get-completed-tasks-this-session)))]
    (testing "A clears exactly its own two rows"
      (is (= 2 cleared)))
    (testing "B's unharvested row and the untagged row SURVIVE -- the regression"
      (is (= #{"t-b" "t-old"} left)))))

(deftest clearing-nothing-is-a-no-op-test
  (seed-two-sessions!)
  (is (= 0 (reg/clear-completed-tasks! [])))
  (is (= 0 (reg/clear-completed-tasks! [nil])))
  (is (= 4 (count (reg/get-completed-tasks-this-session)))))

(deftest kanban-movements-scope-the-same-way-test
  (reg/register-kanban-movement! {:task-id "k-a" :to "done" :project-id "hive" :session-id "s-a"})
  (reg/register-kanban-movement! {:task-id "k-b" :to "done" :project-id "hive" :session-id "s-b"})
  (let [mine (reg/get-kanban-movements-this-session :session-ref ref-a :parent-of parent-of)]
    (is (= ["k-a"] (mapv :kanban-movement/task-id mine)))
    (let [ids (map :kanban-movement/id mine)]
      (is (= 1 (reg/clear-kanban-movements! ids)))
      (is (= ["k-b"] (mapv :kanban-movement/task-id
                           (reg/get-kanban-movements-this-session)))))))

(deftest wrap-queue-selects-own-lings-test
  (testing "a coordinator picks up only the wraps that permeate into ITS session,
            which a project-id match cannot do when both share a project"
    (wq/add-wrap-notification! "w-a1" {:agent-id "ling-a1" :session-id "s-a1"
                                       :project-id "hive" :parent-session-id "s-a" :depth 1})
    (wq/add-wrap-notification! "w-b1" {:agent-id "ling-b1" :session-id "s-b1"
                                       :project-id "hive" :parent-session-id "s-b" :depth 1})
    (is (= ["w-a1"] (mapv :wrap-queue/id (wq/get-unprocessed-wraps-for-session "s-a"))))
    (is (= ["w-b1"] (mapv :wrap-queue/id (wq/get-unprocessed-wraps-for-session "s-b"))))
    (testing "the project-scoped query cannot tell them apart"
      (is (= 2 (count (wq/get-unprocessed-wraps-for-project "hive")))))
    (testing "a processed wrap is not offered twice"
      (wq/mark-wrap-processed! "w-a1")
      (is (empty? (wq/get-unprocessed-wraps-for-session "s-a"))))
    (is (nil? (wq/get-unprocessed-wraps-for-session nil)))))
