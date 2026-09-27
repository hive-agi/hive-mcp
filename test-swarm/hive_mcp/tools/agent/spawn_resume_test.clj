(ns hive-mcp.tools.agent.spawn-resume-test
  "resume / chat_run_id given to `agent spawn` reach the headless backend's
   ctx as kebab keys, through a stub backend (no concrete bb-ling)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.test.stub.headless-backend :as hb]
            [hive-test.isolation :as iso]
            [hive-mcp.isolation-methods]
            [hive-mcp.agent.ling.headless-registry :as headless-registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (iso/with-isolations :swarm-ds))

(defn- spawn-headless
  "Spawn through the handler against a chat-point capable stub backend."
  [b params]
  (hb/with-backend :hive-agent b
    (headless-registry/register-headless! :hive-agent b
                                          {:priority 1000 :capabilities #{:chat-points}})
    (let [resp (spawn/handle-spawn (merge {:type "ling"
                                           :cwd "/tmp/spawn-resume"
                                           :spawn_mode "headless"
                                           :model "venice:test-model"}
                                          params))]
      [resp (some-> (hb/calls-of b :spawn!) first first)])))

(deftest resume-at-a-turn-reaches-the-backend-ctx
  (let [[resp ctx] (spawn-headless (hb/->backend :hive-agent)
                                   {:name "resume-fork"
                                    :task "continue"
                                    :llm_retries 4
                                    :resume {:run_id "run-a" :at {:turn 3} :prompt "other way"}
                                    :chat_run_id "run-b"})]
    (is (not (:isError resp)) (:text resp))
    (is (= {:run-id "run-a" :at {:turn 3} :prompt "other way"} (:resume ctx)))
    (is (= "run-b" (:chat-run-id ctx)))
    (is (= 4 (:llm-retries ctx)))))

(deftest a-plain-spawn-carries-no-resume
  (let [[_ ctx] (spawn-headless (hb/->backend :hive-agent) {:name "resume-none" :task "x"})]
    (is (some? ctx))
    (is (not (contains? ctx :resume)))
    (is (not (contains? ctx :chat-run-id)))))

(deftest a-malformed-resume-is-an-error-and-spawns-nothing
  (let [b (hb/->backend :hive-agent)
        [resp ctx] (spawn-headless b {:name "resume-bad" :task "x" :resume {:at {:seq 1}}})]
    (is (:isError resp))
    (is (re-find #"run_id" (:text resp)))
    (is (nil? ctx))))

(deftest resume-on-a-backend-that-drops-it-is-refused
  (testing "a headless backend other than the chat-point one spawns nothing"
    (let [b    (hb/->backend :test-headless)
          resp (hb/with-backend :test-headless b
                 (spawn/handle-spawn {:type "ling" :name "resume-elsewhere" :task "x"
                                      :cwd "/tmp/spawn-resume" :spawn_mode "headless"
                                      :model "venice:test-model"
                                      :resume {:run_id "run-a"}}))
          ctx  (some-> (hb/calls-of b :spawn!) first first)]
      (is (:isError resp))
      (is (re-find #"resume" (:text resp)))
      (is (nil? ctx))))
  (testing "a terminal spawn refuses chat_run_id before launching anything"
    (let [resp (spawn/handle-spawn {:type "ling" :name "resume-vterm" :task "x"
                                    :cwd "/tmp/spawn-resume" :spawn_mode "claude"
                                    :model "venice:test-model"
                                    :chat_run_id "run-b"})]
      (is (:isError resp))
      (is (re-find #"chat_run_id" (:text resp))))))

(deftest a-mistyped-chat-run-id-is-a-clear-error
  (let [b (hb/->backend :hive-agent)
        [resp ctx] (spawn-headless b {:name "resume-num" :task "x" :chat_run_id 42})]
    (is (:isError resp))
    (is (re-find #"chat_run_id" (:text resp)))
    (is (nil? ctx))))
