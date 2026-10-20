(ns hive-mcp.agent.headless-trifecta-test
  "Facets for headless ling process management. `build-child-env` is pure over
   the environment and gets a trifecta; the process lifecycle is driven against
   a real, harmless child (`sleep`, not a claude CLI) so the contracts are
   exercised on live Process handles."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.agent.identity :as agent-identity]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (headless/kill-all-headless!)
    (f)
    (headless/kill-all-headless!)))

(def ^:private EnvExtra
  "env-extra with keyword and string keys, including the reserved names."
  [:maybe [:map-of [:or :keyword :string
                    [:enum "HIVE_AGENT_CREDENTIAL" "CLAUDE_SWARM_SLAVE_ID" "ANTHROPIC_API_KEY"]]
           :string]])

(deftrifecta-from-schema build-child-env-trifecta
  hive-mcp.agent.headless/build-child-env
  {:in [:cat [:string {:min 1}]
        [:map
         [:cwd {:optional true} [:string {:min 1}]]
         [:model {:optional true} [:string {:min 1}]]
         [:env-extra {:optional true} EnvExtra]
         [:credential {:optional true} [:string {:min 1}]]]]
   :out [:map-of :string :string]
   :rel (fn [[ling-id {:keys [credential cwd model]}] env]
          (and (= ling-id (get env "CLAUDE_SWARM_SLAVE_ID"))
               (every? #(not (contains? env %)) headless/withheld-env-vars)
               (or (nil? cwd) (= cwd (get env "BB_MCP_PROJECT_DIR")))
               (or (nil? credential)
                   (= credential (get env agent-identity/credential-env)))
               (or (nil? model) (= "claude" model)
                   (= model (get env "OPENROUTER_MODEL")))))
   :num-tests 200})

(deftest child-env-never-leaks-withheld-names
  (let [env (headless/build-child-env "ling-1" {:env-extra {"ANTHROPIC_API_KEY" "x"
                                                            :OPENAI_API_KEY "y"
                                                            "KEEP" "z"
                                                            "CLAUDE_SWARM_SLAVE_ID" "imposter"}
                                                :credential "tok"})]
    (is (= "ling-1" (get env "CLAUDE_SWARM_SLAVE_ID")))
    (is (= "tok" (get env "HIVE_AGENT_CREDENTIAL")))
    (is (= "z" (get env "KEEP")))
    (is (not-any? #(contains? env %) headless/withheld-env-vars))))

(defn- spawn-sleeper! [id]
  (headless/spawn-headless! id {:cwd (System/getProperty "java.io.tmpdir")
                                :claude-cmd "sleep"
                                :task "30"
                                :buffer-capacity 7}))

(deftest process-lifecycle
  (let [res (spawn-sleeper! "hl-trifecta-1")]
    (testing "spawn result and registry"
      (is (= "hl-trifecta-1" (:ling-id res)))
      (is (pos? (:pid res)))
      (is (true? (headless/headless? "hl-trifecta-1")))
      (is (false? (headless/headless? "nobody")))
      (is (= 1 (headless/headless-count)))
      (is (identical? (:process res) (headless/get-process "hl-trifecta-1")))
      (is (identical? (:stdout-buf res) (headless/get-stdout-buffer "hl-trifecta-1")))
      (is (identical? (:stderr-buf res) (headless/get-stderr-buffer "hl-trifecta-1"))))
    (testing "status"
      (let [st (headless/headless-status "hl-trifecta-1")]
        (is (true? (:alive? st)))
        (is (nil? (:exit-code st)))
        (is (= 7 (get-in st [:stdout :capacity]))))
      (is (nil? (headless/headless-status "nobody")))
      (is (= [(:ling-id (headless/headless-status "hl-trifecta-1"))]
             (mapv :ling-id (headless/list-headless))))
      (is (= {:active 1 :total 1 :dead 0} (headless/active-process-count))))
    (testing "buffers"
      (is (= [] (headless/get-stdout "hl-trifecta-1")))
      (is (= [] (headless/get-stderr "hl-trifecta-1" {})))
      (is (= [] (headless/get-stdout-since "hl-trifecta-1" 0)))
      (is (nil? (headless/get-stdout "nobody"))))
    (testing "duplicate id is refused"
      (is (thrown? clojure.lang.ExceptionInfo (spawn-sleeper! "hl-trifecta-1"))))
    (testing "kill"
      (let [k (headless/kill-headless! "hl-trifecta-1" {:force? true})]
        (is (true? (:killed? k)))
        (is (= "hl-trifecta-1" (:ling-id k))))
      (is (= 0 (headless/headless-count)))
      (is (thrown? clojure.lang.ExceptionInfo (headless/kill-headless! "hl-trifecta-1"))))))

(deftest kill-all-and-watchdog
  (spawn-sleeper! "hl-trifecta-2")
  (spawn-sleeper! "hl-trifecta-3")
  (is (= {:killed 2 :errors 0} (headless/kill-all-headless!)))
  (is (= {:killed 0 :errors 0} (headless/kill-all-headless!)))
  (let [w (headless/watchdog-status)]
    (is (boolean? (:running? w)))
    (is (vector? (:last-cleaned w)))))

(deftest dispatch-to-unknown-ling-throws
  (is (thrown? clojure.lang.ExceptionInfo (headless/dispatch-via-stdin! "nobody" "hi"))))

(deftest deprecated-watchdog-shims-answer-nil
  (is (nil? (headless/start-watchdog!)))
  (is (nil? (headless/stop-watchdog!))))

(golden/deftest-golden-fn headless-child-env-golden
  "test/golden/agent/headless-child-env.edn"
  (fn []
    (let [env (headless/build-child-env "ling-g" {:cwd "/work" :model "deepseek/x"
                                                  :credential "tok"
                                                  :env-extra {"A" "1" :B "2"
                                                              "ANTHROPIC_API_KEY" "no"}})]
      {:static (select-keys env ["A" "B" "BB_MCP_PROJECT_DIR" "OPENROUTER_MODEL"
                                 "CLAUDE_SWARM_SLAVE_ID" "HIVE_AGENT_CREDENTIAL"])
       :withheld-present? (some? (some #(contains? env %) headless/withheld-env-vars))
       :claude-model-no-override? (not (contains? (headless/build-child-env "l" {:model "claude"})
                                                  "OPENROUTER_MODEL"))})))

(deftest-contract-coverage headless-contract-coverage
  {:paths ["src/hive_mcp/agent/headless.clj"]})
