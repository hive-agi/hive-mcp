(ns hive-mcp.agent.ling.resume-ctx-test
  "A spawn's chat-point :resume and :chat-run-id reach the headless
   backend's ctx unchanged, the same way :llm-retries does. The host does
   not interpret them: the bb-ling backend resolves the run and cut point."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.ling.lifecycle :as lifecycle]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest resume-and-chat-run-id-ride-from-ling-opts-to-strategy-ctx
  (testing "resume at the tip, fork at a point, and an explicit run id all survive"
    (doseq [resume [{:run-id "run-1"}
                    {:run-id "run-1" :at {:seq 7}}
                    {:run-id "run-1" :at {:turn 2} :prompt "try the other branch"}]]
      (let [l   (ling/->ling "resume-ling" {:cwd "/w" :spawn-mode :hive-agent
                                            :resume resume :chat-run-id "run-2"
                                            :llm-retries 5})
            ctx (lifecycle/ling-ctx l)]
        (is (= resume (:resume l)))
        (is (= resume (:resume ctx)))
        (is (= "run-2" (:chat-run-id ctx)))
        (is (= 5 (:llm-retries ctx))))))
  (testing "a spawn that says nothing leaves both keys out"
    (let [ctx (lifecycle/ling-ctx (ling/->ling "plain" {:cwd "/w" :spawn-mode :hive-agent}))]
      (is (not (contains? ctx :resume)))
      (is (not (contains? ctx :chat-run-id))))))
