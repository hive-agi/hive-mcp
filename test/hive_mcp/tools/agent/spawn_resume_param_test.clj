(ns hive-mcp.tools.agent.spawn-resume-param-test
  "The MCP `resume` / `chat_run_id` / `llm_retries` spawn params: advertised
   in the agent tool schema (resume derived from ResumeParam), validated once
   by SpawnLoopParams, and mapped to the kebab opts the bb-ling backend reads."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.tools.consolidated.agent :as agent]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest the-agent-tool-advertises-resume-and-chat-run-id
  (let [props (get-in agent/tool-def [:inputSchema :properties])
        at    (get-in props ["resume" :properties "at"])]
    (testing "resume is a closed object with run_id, at {seq|turn} and prompt"
      (is (= "object" (get-in props ["resume" :type])))
      (is (= ["run_id"] (get-in props ["resume" :required])))
      (is (false? (get-in props ["resume" :additionalProperties])))
      (is (= "string" (get-in props ["resume" :properties "run_id" :type])))
      (is (= #{"seq" "turn"} (set (mapcat (comp keys :properties) (:anyOf at)))))
      (is (= "string" (get-in props ["resume" :properties "prompt" :type])))
      (is (re-find #"tip" (get-in props ["resume" :description])))
      (is (re-find #"(?i)fork" (get-in props ["resume" :description]))))
    (testing "chat_run_id is a string, next to llm_retries, and points at chat_point.run_id"
      (is (= "string" (get-in props ["chat_run_id" :type])))
      (is (re-find #"chat_point\.run_id" (get-in props ["chat_run_id" :description])))
      (is (= "integer" (get-in props ["llm_retries" :type]))))))

(deftest normalize-resume-maps-json-to-kebab
  (testing "absent stays absent"
    (is (nil? (spawn/normalize-resume nil))))
  (testing "run_id alone resumes at the tip"
    (is (= {:run-id "r1"} (spawn/normalize-resume {:run_id "r1"})))
    (is (= {:run-id "r1"} (spawn/normalize-resume {"run_id" "r1"}))))
  (testing "at forks from a seq or a turn; string numbers are parsed"
    (is (= {:run-id "r1" :at {:seq 4}}
           (spawn/normalize-resume {:run_id "r1" :at {:seq 4}})))
    (is (= {:run-id "r1" :at {:turn 2} :prompt "go left"}
           (spawn/normalize-resume {:run_id "r1" :at {:turn "2"} :prompt "go left"})))
    (is (= {:run-id "r1"} (spawn/normalize-resume {:run_id "r1" :at {}})))
    (is (= {:run-id "r1"} (spawn/normalize-resume {:run_id "r1" :at nil}))))
  (testing "blank prompt is dropped"
    (is (= {:run-id "r1"} (spawn/normalize-resume {:run_id "r1" :prompt " "}))))
  (testing "malformed values are refused"
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume "r1")))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume {:at {:seq 1}})))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume {:run_id "r" :at 3})))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume {:run_id "r" :at {:seq 1 :turn 1}})))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume {:run_id "r" :at {:seq -1}})))))

(deftest a-misspelled-branch-point-is-refused-not-resumed
  (doseq [typo [{:run_id "r" :fork_at {:seq 3}}
                {"run_id" "r" "at_seq" 3}
                {:run_id "r" :at {:sequence 3}}]]
    (let [e (try (spawn/normalize-resume typo) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e) (pr-str typo))
      (is (re-find #"disallowed key" (ex-message e)) (pr-str typo)))))

(deftest loop-opts-validates-every-loop-param-once
  (testing "all three map to kebab opts; JSON strings coerce"
    (is (= {:llm-retries 4 :resume {:run-id "a"} :chat-run-id "b"}
           (spawn/loop-opts {:llm_retries "4" :resume {:run_id "a"} :chat_run_id "b"}))))
  (testing "absent or blank is omitted"
    (is (= {} (spawn/loop-opts {})))
    (is (= {} (spawn/loop-opts {:chat_run_id " " :resume nil :llm_retries nil}))))
  (testing "wrong types are an ex-info naming the param, not a ClassCastException"
    (doseq [bad [{:chat_run_id 5} {:llm_retries "x"} {:llm_retries -1}]]
      (let [e (try (spawn/loop-opts bad) nil (catch Exception e e))]
        (is (instance? clojure.lang.ExceptionInfo e) (pr-str bad))))))

(deftest chat-point-params-need-a-chat-point-backend
  (testing "no chat-point params, nothing to refuse"
    (is (nil? (spawn/chat-point-refusal :vterm {:llm-retries 3}))))
  (testing "a chat-point backend takes them"
    (is (nil? (spawn/chat-point-refusal :hive-agent {:resume {:run-id "r"} :chat-run-id "c"}))))
  (testing "any other resolved mode refuses, naming what it would drop"
    (let [msg (spawn/chat-point-refusal :claude {:resume {:run-id "r"} :chat-run-id "c"})]
      (is (re-find #"resume and chat_run_id" msg))
      (is (re-find #":claude" msg)))
    (is (re-find #"chat_run_id" (spawn/chat-point-refusal :agent-sdk {:chat-run-id "c"})))))
