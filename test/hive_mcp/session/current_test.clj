(ns hive-mcp.session.current-test
  "The adapter half: env resolution, degradation with no swarm, and the
   parent-of lookup the ownership walks use."
  (:require [clojure.test :refer [deftest testing is]]
            [hive-mcp.session.current :as cur]
            [hive-mcp.session.identity :as sid]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def world
  {:slaves {"coord-a" {:slave/id "coord-a" :slave/depth 0
                       :slave/session-id "s-a" :slave/project-id "hive"}
            "ling-a1" {:slave/id "ling-a1" :slave/depth 1 :slave/parent-id "coord-a"
                       :slave/session-id "s-a1" :slave/project-id "hive"}
            "ling-a2" {:slave/id "ling-a2" :slave/depth 2 :slave/parent-id "ling-a1"
                       :slave/session-id "s-a2" :slave/project-id "hive"}}
   :coordinators {"coord-a" {:coordinator/id "coord-a" :coordinator/session-id "s-a"
                             :coordinator/project "hive" :coordinator/status :active}}})

(deftest host-session-id-precedence-test
  (testing "an explicit host id wins over the swarm slave id"
    (is (= "from-host" (cur/host-session-id {"HIVE_SESSION_ID" "from-host"
                                             "CLAUDE_SWARM_SLAVE_ID" "worker-1"}))))
  (testing "CLAUDE_SESSION_ID is accepted"
    (is (= "cc-1" (cur/host-session-id {"CLAUDE_SESSION_ID" "cc-1"}))))
  (testing "the slave id is the last resort"
    (is (= "worker-1" (cur/host-session-id {"CLAUDE_SWARM_SLAVE_ID" "worker-1"}))))
  (testing "blank and missing values are ignored"
    (is (nil? (cur/host-session-id {"HIVE_SESSION_ID" "" "CLAUDE_SESSION_ID" nil})))
    (is (nil? (cur/host-session-id {})))))

(deftest always-valid-ref-test
  (testing "with no swarm and no env, the session still gets a usable id --
            a bare editor session must not share an id with every other one"
    (let [r (cur/session-ref {:world {:slaves {} :coordinators {}}
                              :project-id "hive"})]
      (is (sid/valid? r))
      (is (= :adhoc (:session/kind r)))
      (is (seq (:session/id r))))))

(deftest process-id-is-stable-test
  (testing "two calls in the same process return the same id"
    (let [w {:slaves {} :coordinators {}}]
      (is (= (cur/session-id {:world w}) (cur/session-id {:world w}))))))

(deftest adhoc-takes-live-coordinator-as-parent-test
  (let [r (cur/session-ref {:world world :project-id "hive" :session-id "s-adhoc"})]
    (is (= :adhoc (:session/kind r)))
    (is (= "s-a" (:session/parent-id r)))))

(deftest parent-of-fn-test
  (let [parent-of (cur/parent-of-fn world)]
    (testing "a ling resolves to its ROOT coordinator's session at any depth"
      (is (= "s-a" (parent-of "s-a1")))
      (is (= "s-a" (parent-of "s-a2"))))
    (testing "a coordinator has no parent"
      (is (nil? (parent-of "s-a"))))
    (testing "an unknown session has no parent"
      (is (nil? (parent-of "s-nope"))))
    (testing "and it drives ownership: the coordinator owns its whole subtree,
              at any nesting depth"
      (let [ref-a (sid/session-ref {:id "s-a" :kind :coordinator :project-id "hive"})]
        (is (sid/owns? parent-of ref-a "s-a1"))
        (is (sid/owns? parent-of ref-a "s-a2"))
        (is (not (sid/owns? parent-of ref-a "s-other")))))))

(deftest world-snapshot-degrades-test
  (testing "world-snapshot never throws, even with no swarm store loaded"
    (let [w (cur/world-snapshot)]
      (is (map? w))
      (is (contains? w :slaves))
      (is (contains? w :coordinators)))))
