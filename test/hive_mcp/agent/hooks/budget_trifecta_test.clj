(ns hive-mcp.agent.hooks.budget-trifecta-test
  "Property + golden + mutation facets for the budget guardrail, derived from
   the malli contracts, plus unit tests for the stateful ledger."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.hooks.budget :as b]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (b/reset-all-budgets!) (try (f) (finally (b/reset-all-budgets!)))))

(def Model
  [:maybe [:enum "claude" "claude-haiku" "claude-opus" "x:free" "deepseek-v3" "kimi-k2" "gpt-x"]])

(def Tokens [:int {:min 0 :max 1000000}])

(defn- cost [in out model]
  (b/estimate-cost-usd in out model))

(deftrifecta-from-schema estimate-cost-usd-trifecta
  hive-mcp.agent.hooks.budget/estimate-cost-usd
  {:in [:cat Tokens Tokens Model]
   :out [:double {:min 0.0}]
   :rel (fn [[in out model] c]
          (and (>= c 0.0)
               ;; monotone in tokens
               (<= c (+ (cost (inc in) (inc out) model) 1e-9))
               ;; the free tier is free
               (or (not= model "x:free") (zero? c))))
   :golden-path "test/golden/agent/budget-estimate-cost.edn"
   :num-tests 200})

(deftrifecta-from-schema estimate-tool-call-cost-trifecta
  hive-mcp.agent.hooks.budget/estimate-tool-call-cost
  {:in [:cat [:enum "read_file" "grep" "bash" "file_write" "other" nil] [:string {:max 40}] Model]
   :out [:double {:min 0.0}]
   :rel (fn [[tool input model] c] (>= c 0.0))
   :golden-path "test/golden/agent/budget-estimate-tool-cost.edn"})

(deftest pricing-by-model-family
  (is (= 3.0 (cost 1000000 0 nil)) "nil model prices as claude sonnet")
  (is (= 0.25 (cost 1000000 0 "claude-haiku")))
  (is (= 75.0 (cost 0 1000000 "claude-opus")))
  (is (= 0.0 (cost 1000000 1000000 "m:free")))
  (is (= 0.14 (cost 1000000 0 "deepseek-v3")))
  (is (= 0.2 (cost 1000000 0 "kimi-k2")))
  (is (= 3.0 (cost 1000000 0 "gpt-x")) "unknown models fall to :default"))

(golden/deftest-golden-fn budget-ledger-golden
  "test/golden/agent/budget-ledger.edn"
  (fn []
    (b/reset-all-budgets!)
    (b/register-budget! "a" 1.0 {:model "claude-haiku"})
    (b/register-budget! "b" 0.0001)
    (let [allow (b/check-budget "a" "bash" "ls")
          deny  (b/check-budget "b" "bash" (apply str (repeat 400 "x")))
          _     (b/record-usage! "a" 0.5)
          st    (-> (b/get-budget-status "a") (dissoc :registered-at))]
      {:allow allow
       :deny-action (:action deny)
       :deny-interrupt (:interrupt? deny)
       :deny-message-prefix (subs (:message deny) 0 16)
       :status st
       :unknown (b/check-budget "nobody" "bash" "ls")
       :total (-> (b/get-total-spend) (update :agents (fn [as] (mapv #(update % :spent-usd double) as))))
       :guard-no-agent ((b/budget-guardrail-handler) "bash" "ls" {})
       :guard-agent ((b/budget-guardrail-handler) "bash" "ls" {:agent-id "a"})})))

(deftest ledger-lifecycle
  (testing "register / record / reset / deregister"
    (let [e (b/register-budget! "x" 2 {:model "claude-opus"})]
      (is (= 2.0 (:max-budget-usd e)))
      (is (= "claude-opus" (:model e))))
    (is (= 0.5 (:spent-usd (b/record-usage! "x" 0.5 {:tool-name "bash"}))))
    (is (true? (:exceeded? (b/record-usage! "x" 5))))
    (is (nil? (b/record-usage! "ghost" 1)))
    (b/reset-agent-spend! "x")
    (is (= 0.0 (:spent-usd (b/get-budget-status "x"))))
    (is (false? (:exceeded? (b/get-budget-status "x"))))
    (is (= "x" (:agent-id (first (b/get-all-budget-statuses)))))
    (is (some? (b/deregister-budget! "x")))
    (is (nil? (b/deregister-budget! "x")))
    (is (= [] (b/get-all-budget-statuses)))))

(deftest record-tool-usage-estimates-cost
  (b/register-budget! "t" 10.0)
  (is (pos? (:spent-usd (b/record-tool-usage! "t" "bash" "ls"))))
  (is (nil? (b/record-tool-usage! "ghost" "bash" "ls"))))

(deftest check-budget-denies-and-counts
  (b/register-budget! "d" 0.000001)
  (let [r (b/check-budget "d" "bash" "ls")]
    (is (= :deny (:action r)))
    (is (true? (:interrupt? r)))
    (is (re-find #"^Budget exceeded" (:message r))))
  (let [s (b/get-budget-status "d")]
    (is (= 1 (:denied-calls s)))
    (is (true? (:exceeded? s)))))

(deftest register-budget-precondition
  (is (thrown? Throwable (b/register-budget! "bad" 0)))
  (is (thrown? Throwable (b/register-budget! "bad" -1))))

(deftest-contract-coverage hooks-contract-coverage
  {:paths ["src/hive_mcp/agent/hooks"]
   :exempt {}})
