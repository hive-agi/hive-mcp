(ns hive-mcp.tools.agent.spawn-resume-param-test
  "The MCP `resume` / `chat_run_id` spawn params: advertised in the agent
   tool schema and normalized from snake_case JSON to the kebab map the
   bb-ling backend reads."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.tools.consolidated.agent :as agent]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest the-agent-tool-advertises-resume-and-chat-run-id
  (let [props (get-in agent/tool-def [:inputSchema :properties])]
    (testing "resume is an object with run_id, at {seq|turn} and prompt"
      (is (= "object" (get-in props ["resume" :type])))
      (is (= ["run_id"] (get-in props ["resume" :required])))
      (is (= "string" (get-in props ["resume" :properties "run_id" :type])))
      (is (= "integer" (get-in props ["resume" :properties "at" :properties "seq" :type])))
      (is (= "integer" (get-in props ["resume" :properties "at" :properties "turn" :type])))
      (is (= "string" (get-in props ["resume" :properties "prompt" :type])))
      (is (re-find #"tip" (get-in props ["resume" :description])))
      (is (re-find #"(?i)fork" (get-in props ["resume" :description]))))
    (testing "chat_run_id is a string, next to llm_retries"
      (is (= "string" (get-in props ["chat_run_id" :type])))
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
    (is (= {:run-id "r1"} (spawn/normalize-resume {:run_id "r1" :at {}}))))
  (testing "blank prompt is dropped"
    (is (= {:run-id "r1"} (spawn/normalize-resume {:run_id "r1" :prompt " "}))))
  (testing "malformed values are refused"
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume "r1")))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume {:at {:seq 1}})))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume {:run_id "r" :at 3})))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume {:run_id "r" :at {:seq 1 :turn 1}})))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/normalize-resume {:run_id "r" :at {:seq -1}})))))
