(ns hive-mcp.agent.headless-env-test
  "What a headless child ling may inherit from the host's environment."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.headless :as headless]))

(deftest build-child-env-withholds-api-key-billing-vars-test
  (testing "no withheld var reaches a child, not even through env-extra"
    (let [env (headless/build-child-env
               "test-ling"
               {:env-extra (zipmap headless/withheld-env-vars (repeat "leak"))})]
      (doseq [k headless/withheld-env-vars]
        (is (not (contains? env k)) k))))

  (testing "the withheld set covers the Anthropic, OpenAI and Kimi key and endpoint vars"
    (is (every? (set headless/withheld-env-vars)
                ["ANTHROPIC_API_KEY" "ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_BASE_URL"
                 "OPENAI_API_KEY" "MOONSHOT_API_KEY" "KIMI_API_KEY"])))

  (testing "other env-extra entries still pass through"
    (is (= "v" (get (headless/build-child-env "test-ling" {:env-extra {"SOME_VAR" "v"}})
                    "SOME_VAR")))))
