(ns hive-mcp.channel.audience-test
  "Audience routing + progress digest — pure, no fixtures, no live state."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.audience :as aud]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; coordinator-reader?
;; =============================================================================

(deftest coordinator-reader?-test
  (testing "the MCP lane's reader ids, bare and project-suffixed"
    (is (aud/coordinator-reader? "coordinator"))
    (is (aud/coordinator-reader? "coordinator-hive"))
    (is (aud/coordinator-reader? nil)))
  (testing "a ling is not a coordinator"
    (is (not (aud/coordinator-reader? "vt-billing-checkpoint")))
    (is (not (aud/coordinator-reader? "ling-7")))))

;; =============================================================================
;; addressed-to? — the anti-pollution contract
;; =============================================================================

(deftest siblings-never-see-each-other-test
  (testing "a shout from one ling does NOT reach a sibling under the same parent"
    (let [msg {:agent-id "vt-billing" :parent-id "coordinator" :project-id "hive"}]
      (is (not (aud/addressed-to? "vt-media-retention" msg)))
      (is (not (aud/addressed-to? "vt-pack-trifecta" msg))))))

(deftest spawner-receives-test
  (testing "the shout reaches exactly the agent named by :parent-id"
    (let [msg {:agent-id "child" :parent-id "ling-a"}]
      (is (aud/addressed-to? "ling-a" msg))
      (is (not (aud/addressed-to? "ling-b" msg)))))
  (testing "a coordinator-spawned ling reaches the coordinator lane despite the
            project suffix on the reader id"
    (let [msg {:agent-id "vt-billing" :parent-id "coordinator"}]
      (is (aud/addressed-to? "coordinator-hive" msg))
      (is (aud/addressed-to? "coordinator" msg)))))

(deftest grandchildren-stop-at-their-spawner-test
  (testing "a grandchild's shout reaches its own spawner, not the coordinator"
    (let [msg {:agent-id "grandchild" :parent-id "ling-a"}]
      (is (aud/addressed-to? "ling-a" msg))
      (is (not (aud/addressed-to? "coordinator-hive" msg))))))

(deftest root-shouts-reach-coordinators-test
  (testing "no :parent-id means root-level — coordinator readers only"
    (let [msg {:agent-id "wave-scheduler"}]
      (is (aud/addressed-to? "coordinator-hive" msg))
      (is (not (aud/addressed-to? "some-ling" msg))))))

(deftest broadcast-reaches-everyone-test
  (let [msg {:agent-id "coordinator" :broadcast? true}]
    (is (aud/addressed-to? "coordinator-hive" msg))
    (is (aud/addressed-to? "any-ling" msg))))

(deftest coordinator-sessions-are-distinct-readers-test
  (testing "the session token is read off the MCP lane's spellings"
    (is (= "1269206" (aud/coordinator-session "coordinator:1269206")))
    (is (= "1269206" (aud/coordinator-session "coordinator:1269206-hive-assay")))
    (is (= "a1b2c3d4" (aud/coordinator-session "coordinator:a1b2c3d4-hive")))
    (is (nil? (aud/coordinator-session "coordinator")))
    (is (nil? (aud/coordinator-session "coordinator-hive")))
    (is (nil? (aud/coordinator-session "ling-7")))
    (is (nil? (aud/coordinator-session nil))))
  (testing "a ling spawned by one window reaches that window under any project
            suffix, and no other window"
    (let [msg {:agent-id "wave-x-m0" :parent-id "coordinator:1269206"}]
      (is (aud/addressed-to? "coordinator:1269206-hive" msg))
      (is (aud/addressed-to? "coordinator:1269206-hive-mcp" msg))
      (is (aud/addressed-to? "coordinator:1269206" msg))
      (is (not (aud/addressed-to? "coordinator:1343228-hive" msg)))
      (is (not (aud/addressed-to? "coordinator:1343228" msg)))
      (is (not (aud/addressed-to? "ling-b" msg)))))
  (testing "a lane spelled without a session still matches every lane, so the
            legacy and Emacs paths keep receiving"
    (let [msg {:agent-id "ling-a" :parent-id "coordinator"}]
      (is (aud/addressed-to? "coordinator:1269206-hive" msg))
      (is (aud/addressed-to? "coordinator-hive" msg)))
    (is (aud/addressed-to? "coordinator-hive" {:agent-id "ling-a" :parent-id "coordinator:1269206"})))
  (testing "root-level shouts (no parent) still reach every coordinator lane"
    (is (aud/addressed-to? "coordinator:1269206-hive" {:agent-id "orphan"}))
    (is (aud/addressed-to? "coordinator:1343228-hive" {:agent-id "orphan"}))))

;; --- directed delivery to a project-scoped reader --------------------------

(defn- scoped-id
  "Stands in for hive-dsl.context.identity/make-piggyback-agent-id."
  [project]
  (fn [id] (str id "-" project)))

(deftest directed-message-reaches-a-project-scoped-reader-test
  (let [in-hive-mcp (scoped-id "hive-mcp")
        msg         {:agent-id "peer-a" :to "inbox-b"}]
    (testing "the bare reader matches, exactly as before"
      (is (aud/addressed-to? "inbox-b" msg)))
    (testing "the project-scoped reader matches its own :to"
      (is (aud/addressed-to? "inbox-b-hive-mcp" msg in-hive-mcp)))
    (testing "another ling in the same project does not"
      (is (not (aud/addressed-to? "other-ling-hive-mcp" msg in-hive-mcp))))
    (testing "a reader in a DIFFERENT project does not match a scope it is not in"
      (is (not (aud/addressed-to? "inbox-b-hive-mcp" msg (scoped-id "vtranslate")))))))

(deftest composing-the-scope-beats-stripping-it-test
  (testing "a ling whose own name ends in the project scope stays distinct"
    (let [in-hive-mcp (scoped-id "hive-mcp")]
      ;; ling literally named "worker-hive-mcp" reads as "worker-hive-mcp-hive-mcp"
      (is (aud/addressed-to? "worker-hive-mcp-hive-mcp"
                             {:agent-id "peer-a" :to "worker-hive-mcp"}
                             in-hive-mcp))
      ;; and a message for plain "worker" must NOT reach it
      (is (not (aud/addressed-to? "worker-hive-mcp-hive-mcp"
                                  {:agent-id "peer-a" :to "worker"}
                                  in-hive-mcp)))
      ;; while plain "worker" still gets its own
      (is (aud/addressed-to? "worker-hive-mcp"
                             {:agent-id "peer-a" :to "worker"}
                             in-hive-mcp)))))

(deftest scoped-reader-still-obeys-every-other-rule-test
  (let [in-hive-mcp (scoped-id "hive-mcp")]
    (testing "a scoped ling still receives a broadcast"
      (is (aud/addressed-to? "ling-a-hive-mcp"
                             {:agent-id "peer-b" :broadcast? true}
                             in-hive-mcp)))
    (testing "a directed message still excludes the coordinator"
      (is (not (aud/addressed-to? "coordinator:1-hive-mcp"
                                  {:agent-id "peer-a" :to "inbox-b"}
                                  in-hive-mcp))))
    (testing "the scope is consulted for the directed rule alone"
      (is (not (aud/addressed-to? "ling-parent-hive-mcp"
                                  {:agent-id "ling-child" :parent-id "ling-parent"}
                                  in-hive-mcp)))
      (is (aud/addressed-to? "ling-parent"
                             {:agent-id "ling-child" :parent-id "ling-parent"}
                             in-hive-mcp)))))

(deftest filter-messages-honours-the-scope-test
  (let [in-hive-mcp (scoped-id "hive-mcp")
        msgs [{:agent-id "peer-a" :to "inbox-b" :message "for b"}
              {:agent-id "peer-a" :to "someone-else" :message "not for b"}
              {:agent-id "peer-c" :broadcast? true :message "for everyone"}]]
    (is (= ["for b" "for everyone"]
           (mapv :message (aud/filter-messages "inbox-b-hive-mcp" msgs in-hive-mcp))))
    (is (= ["for everyone"]
           (mapv :message (aud/filter-messages "inbox-b-hive-mcp" msgs))))))

(deftest no-self-echo-for-lings-test
  (testing "a ling does not read back its own shout"
    (is (not (aud/addressed-to? "ling-a" {:agent-id "ling-a" :parent-id "coordinator"}))))
  (testing "but the coordinator still sees shouts it authored (wave scheduler)"
    (is (aud/addressed-to? "coordinator-hive" {:agent-id "coordinator"}))))

(deftest filter-messages-test
  (testing "filter-messages keeps order and drops what is not addressed"
    (let [msgs [{:agent-id "a" :parent-id "coordinator" :timestamp 1}
                {:agent-id "b" :parent-id "a" :timestamp 2}
                {:agent-id "c" :parent-id "coordinator" :timestamp 3}]]
      (is (= [1 3] (mapv :timestamp (aud/filter-messages "coordinator-hive" msgs))))
      (is (= [2] (mapv :timestamp (aud/filter-messages "a" msgs)))))))

;; =============================================================================
;; digest — the anti-micromanagement contract
;; =============================================================================

(defn- progress [a m] {:a a :e "progress" :m m})

(deftest digest-collapses-a-progress-burst-test
  (testing "21 per-turn rows from one agent collapse to a single rollup"
    (let [rows (mapv #(progress "vt-billing" (str "turn " %)) (range 1 22))
          out (aud/digest rows)]
      (is (= 1 (count out)))
      (is (= 21 (:n (first out))) "carries the burst count")
      (is (= "turn 21" (:m (first out))) "carries the LAST message, not the first"))))

(deftest digest-passes-lifecycle-through-verbatim-test
  (let [rows [(progress "a" "turn 1")
              (progress "a" "turn 2")
              {:a "a" :e "error" :m "boom"}]
        out (aud/digest rows)]
    (is (= ["progress" "error"] (mapv :e out)))
    (is (= "boom" (:m (last out))) "the error is not collapsed")))

(deftest digest-keeps-agents-separate-test
  (let [rows [(progress "a" "a1") (progress "b" "b1") (progress "a" "a2")]
        out (aud/digest rows)]
    (is (= 2 (count out)))
    (is (= #{"a" "b"} (set (map :a out))))
    (is (= "a2" (:m (first (filter #(= "a" (:a %)) out)))))))

(deftest digest-preserves-position-of-last-row-test
  (testing "the rollup sits where the agent's LAST progress row was, so it
            reads as current state relative to other agents' events"
    (let [rows [(progress "a" "a1")
                {:a "b" :e "completed" :m "done"}
                (progress "a" "a2")]
          out (aud/digest rows)]
      (is (= ["completed" "progress"] (mapv :e out))))))

(deftest digest-leaves-a-lone-progress-row-untouched-test
  (let [rows [(progress "a" "only")]]
    (is (= rows (aud/digest rows)) "no :n key when nothing was collapsed")))

(deftest digest-of-empty-is-empty-test
  (is (= [] (aud/digest [])))
  (is (= [] (aud/digest nil))))

(deftest digest-never-collapses-a-deliberate-row-test
  (testing "measured 2026-09-07: a wave member's own `hivemind shout` (progress,
            \"probe hello\") sat between two runtime `bb-ling turn N` progress
            rows and the digest kept only the LAST one — the reader saw the
            telemetry and never what the member said"
    (let [rows [(progress "a" "bb-ling turn 1")
                {:a "a" :e "progress" :m "probe hello" :deliberate? true}
                (progress "a" "bb-ling turn 2")]
          out  (aud/digest rows)]
      (is (= ["probe hello" "bb-ling turn 2"] (mapv :m out))
          "the deliberate row survives, the telemetry still rolls up")
      (is (= 2 (:n (second out))) "the rollup counts only the telemetry rows")
      (is (nil? (:n (first out))) "a pinned row is not a rollup"))))
