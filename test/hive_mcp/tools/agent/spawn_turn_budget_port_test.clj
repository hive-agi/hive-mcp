(ns hive-mcp.tools.agent.spawn-turn-budget-port-test
  "The turn_budget port of `agent spawn`: hive-mcp core owns no turn-budget
   namespace (the census gate freezes the hive-agent extraction), so the
   lease is normalized through a port that resolves hive-agent's normalizer.
   These tests stub the port with a plain fn; no hive-agent on the classpath."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- stub-normalize
  "Port stub: tags what it was handed, so a test sees the raw value reach it."
  [v]
  {:normalized v})

(defn run-turn-budget-opt
  "Subject adapter: turn-budget-opt through the stub port."
  [v]
  (spawn/turn-budget-opt stub-normalize v))

(deftrifecta turn-budget-opt-through-port
  hive-mcp.tools.agent.spawn-turn-budget-port-test/run-turn-budget-opt
  {:golden-path "test/golden/tools/agent/spawn-turn-budget-opt.edn"
   :cases       {:absent  nil
                 :object  {"judge" "hivemind" "initial" 60}
                 :string  "{\"initial\":60}"}
   :gen         (gen/one-of [(gen/return nil)
                             (gen/map gen/keyword gen/small-integer)
                             gen/string-alphanumeric])
   :pred        #(or (nil? %) (contains? % :normalized))
   :num-tests   100
   :mutations   [["drops-the-lease" (constantly nil)]
                 ["bypasses-the-port" identity]
                 ["normalizes-nil-too" (fn [v] {:normalized v})]]
   :assert      (fn []
                  (is (nil? (run-turn-budget-opt nil)) "absent -> backend defaults")
                  (is (= {:normalized {"initial" 60}} (run-turn-budget-opt {"initial" 60}))
                      "a present value goes through the port"))})

(deftest absent-port-refuses-a-lease-loudly
  (testing "hive-agent missing: a turn_budget is refused, never dropped"
    (let [e (try (spawn/turn-budget-opt nil {"initial" 60}) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (= "turn_budget" (:param (ex-data e))))))
  (testing "hive-agent missing and no turn_budget: nothing to refuse"
    (is (nil? (spawn/turn-budget-opt nil nil)))))

(deftest loop-opts-threads-the-port
  (is (= {:turn-budget {:normalized {"initial" 60}} :llm-retries 4}
         (spawn/loop-opts {:turn_budget {"initial" 60} :llm_retries 4} stub-normalize)))
  (is (= {} (spawn/loop-opts {} stub-normalize))))
