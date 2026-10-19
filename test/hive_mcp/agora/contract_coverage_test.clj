(ns hive-mcp.agora.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/agora carries an `m/=>` contract
   or a REASONED exemption, and no exemption is stale.

   signal, schema and consensus are compat shims over hive-agent, so their
   trifectas live in the swarm suite (test-swarm/hive_mcp/agora), where the
   addon is on the classpath. What runs here without the addon: the gate, the
   schemas generating, the shims failing loudly when hive-agent is absent, and
   a trifecta over dialogue's one pure fn."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agora.consensus :as consensus]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.schema :as schema]
            [hive-mcp.agora.signal :as signal]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage agora-contracts-gate
  {:paths ["src/hive_mcp/agora"]
   :exempt {}})

(hst/deftrifecta-from-schema format-signal-contract
  hive-mcp.agora.dialogue/format-signal
  {:in        [:cat [:enum :propose :approve :no-change "counter"] [:enum "LGTM" "Here's my change" nil]]
   :out       [:string {:min 11}]
   :rel       (fn [[sig msg] out] (= out (str "[SIGNAL: " (name sig) "] " msg)))
   :golden-path "test/golden/agora/format-signal.edn"})

(deftest agora-schemas-generate-test
  (doseq [s [signal/SignalType signal/Signal signal/SignalError signal/SignalOrError
             signal/SignalInput signal/Contribution signal/Classifiable
             schema/DialogueId schema/DialogueStatus schema/TurnSignal schema/Conn schema/TxReport
             schema/Epoch schema/Uuid schema/Dialogue schema/DialogueRow schema/Turn
             schema/SignalCounts schema/CreateDialogueOpts schema/TurnOpts
             consensus/ConsensusStatus consensus/ProgressScore consensus/MediatorAdvice
             consensus/ConsensusResult
             dialogue/DialogueView dialogue/TurnView dialogue/Detection dialogue/DispatchOpts
             dialogue/DispatchResult]]
    (is (seq (mg/sample s {:size 4})) (pr-str (m/form s)))))

(deftest every-shim-contract-generates-its-arguments-test
  (testing "each contracted arity's argument list generates"
    (doseq [nsym '[hive-mcp.agora.signal hive-mcp.agora.schema hive-mcp.agora.consensus]
            [fsym {:keys [schema]}] (get (m/function-schemas) nsym)
            arity (let [s (m/schema schema)]
                    (if (= :function (m/type s)) (m/children s) [s]))]
      (is (some? (mg/generate (first (m/children arity)) {:size 3}))
          (str nsym "/" fsym)))))

(deftest shims-fail-loudly-without-the-addon-test
  (if (delegate/available? "hive-agent.swarm.agora.signal")
    (is true "hive-agent is on the classpath; the swarm suite exercises the shims")
    (doseq [call [#(signal/parse-signal :approve)
                  #(schema/get-dialogue "d")
                  #(consensus/check-consensus "d")]]
      (let [e (try (call) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (true? (:swarm/addon-missing (ex-data e))))
        (is (= "hive-agent" (:addon (ex-data e))))))))
