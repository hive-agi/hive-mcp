(ns hive-mcp.agora.consensus-trifecta-test
  "Trifectas for the `hive-mcp.agora.consensus` compat shim, driven by its
   contracts. Dialogue-reading fns run over a fixed set of scenario dialogues
   built once per namespace, so their inputs are an enum of scenario ids and
   their outputs are deterministic. Runs in the swarm suite: the shim
   delegates to hive-agent."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agora.consensus :as c]
            [hive-mcp.agora.schema :as schema]
            [hive-mcp.agora.signal :as signal]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private w "writer-1")
(def ^:private cr "critic-2")

(def ^:private scenario-turns
  "Scenario id -> the [sender signal] turns it records, in order."
  {"scn-empty"    []
   "scn-agree"    [[w :propose] [cr :approve] [w :no-change]]
   "scn-self"     [[w :propose] [w :approve]]
   "scn-open"     [[w :propose] [cr :counter]]
   "scn-deadlock" [[w :propose] [cr :counter] [w :counter] [cr :counter]]
   "scn-stuck"    (into [[w :propose]] (take 10 (cycle [[cr :defer] [w :defer]])))
   "scn-timeout"  (vec (take 50 (cycle [[w :propose] [cr :counter]])))})

(defn- build-scenarios! []
  (schema/reset-conn!)
  (doseq [[id turns] scenario-turns]
    (schema/create-dialogue! {:id id :name id :participants [w cr]})
    (doseq [[from sig] turns]
      (schema/add-turn! id {:sender from :receiver (if (= from w) cr w)
                            :message (str (name sig) " msg") :signal sig}))))

(use-fixtures :once (fn [f] (build-scenarios!) (f)))

(def ^:private ScenarioId
  "Every scenario, plus an id no dialogue has."
  (into [:enum "scn-missing"] (keys scenario-turns)))

(def ^:private expected-status
  {"scn-empty" :insufficient "scn-agree" :consensus "scn-self" :continue
   "scn-open" :continue "scn-deadlock" :continue "scn-stuck" :stuck
   "scn-timeout" :timeout "scn-missing" :insufficient})

(def ^:private expected-mediator
  {"scn-deadlock" :deadlock "scn-timeout" :deadlock "scn-stuck" :stuck})

(hst/deftrifecta-from-schema consensus-result-contract
  hive-mcp.agora.consensus/consensus-result
  {:in        ScenarioId
   :out       c/ConsensusResult
   :rel       (fn [id out]
                (and (= (expected-status id) (:status out))
                     (= (= "scn-agree" id) (:nash-equilibrium? out))
                     (= (count (scenario-turns id)) (:turn-count out))
                     (= (if (scenario-turns id) 2 0) (:participants out))
                     (= (expected-mediator id) (:mediator-reason out))
                     (= (boolean (expected-mediator id)) (:mediator-needed? out))))
   :n-cases   16
   :golden-path "test/golden/agora/consensus-result.edn"})

(hst/deftrifecta-from-schema check-consensus-contract
  hive-mcp.agora.consensus/check-consensus
  {:in        ScenarioId
   :out       c/ConsensusStatus
   :rel       (fn [id out] (= (expected-status id) out))
   :n-cases   16
   :golden-path "test/golden/agora/check-consensus.edn"})

(hst/deftrifecta-from-schema should-recruit-mediator-contract
  hive-mcp.agora.consensus/should-recruit-mediator?
  {:in        ScenarioId
   :out       c/MediatorAdvice
   :rel       (fn [id out]
                (and (= (expected-mediator id) (:reason out))
                     (= ({:deadlock :high :stuck :medium} (expected-mediator id)) (:urgency out))))
   :n-cases   16})

(hst/deftrifecta-from-schema calculate-progress-score-contract
  hive-mcp.agora.consensus/calculate-progress-score
  {:in        ScenarioId
   :out       c/ProgressScore
   ;; the score is the sum of its three factors
   :rel       (fn [_ {:keys [score factors]}]
                (< (Math/abs (- score (reduce + (vals factors)))) 1e-9))
   :n-cases   16
   :golden-path "test/golden/agora/progress-score.edn"})

(hst/deftrifecta-from-schema approval-ratio-contract
  hive-mcp.agora.consensus/approval-ratio
  {:in        ScenarioId
   :out       c/Ratio
   :rel       (fn [id out]
                (let [n (c/count-participants id)]
                  (= out (if (zero? n) 0.0 (/ (c/count-approvals id) (double n))))))
   :n-cases   16})

(hst/deftrifecta-from-schema turn-count-contract
  hive-mcp.agora.consensus/turn-count
  {:in        ScenarioId
   :out       c/Count
   :rel       (fn [id out] (= (count (scenario-turns id)) out))
   :n-cases   16
   :golden-path "test/golden/agora/turn-count.edn"})

(def ^:private SlaveId
  "Ids in each naming pattern `extract-short-name` recognises, and nil."
  [:enum "swarm-agora-docs-skeptic-1769195802" "ling-agora-critic-456"
   "agora-preset-docs-writer-12" "writer-123" "plain" "a-b-c" nil])

(hst/deftrifecta-from-schema extract-short-name-contract
  hive-mcp.agora.consensus/extract-short-name
  {:in        SlaveId
   :out       [:maybe :string]
   :rel       (fn [in out] (if (nil? in) (nil? out) (and (string? out) (<= (count out) (count in)))))
   :n-cases   10
   :golden-path "test/golden/agora/extract-short-name.edn"})

(hst/deftrifecta-from-schema normalize-participant-match-contract
  hive-mcp.agora.consensus/normalize-participant-match
  {:in        [:cat SlaveId SlaveId]
   :out       [:maybe :boolean]
   :rel       (fn [[a b] out]
                (if (= a b)
                  (true? out)
                  (= (boolean out)
                     (boolean (and (c/extract-short-name a)
                                   (= (c/extract-short-name a) (c/extract-short-name b)))))))
   :num-tests 200})

(hst/deftrifecta-from-schema parse-signal-contract
  hive-mcp.agora.consensus/parse-signal
  {:in        [:enum "[SIGNAL: approve] ok" "[SIGNAL: defer]" "[SIGNAL: shrug] x" "text" "" nil]
   :out       signal/SignalType
   :rel       (fn [in out]
                (= out (case in
                         "[SIGNAL: approve] ok" :approve
                         "[SIGNAL: defer]"      :defer
                         :propose)))
   :n-cases   10
   :golden-path "test/golden/agora/consensus-parse-signal.edn"})

(hst/deftrifecta-from-schema signal-equilibrium-contribution-contract
  hive-mcp.agora.consensus/signal->equilibrium-contribution
  {:in        [:enum :approve :no-change :lgtm :propose :counter :defer :zzz nil]
   :out       signal/Contribution
   :rel       (fn [in out] (= out (signal/equilibrium-contribution in)))
   :n-cases   10})

(deftest scenario-predicates-test
  (testing "nash equilibrium needs every participant aligned on another's proposal"
    (is (true? (c/nash-equilibrium? "scn-agree")))
    (is (false? (c/nash-equilibrium? "scn-self")))
    (is (false? (c/nash-equilibrium? "scn-missing"))))
  (testing "alignment is false with no proposal and nil when it has no outside approval"
    (is (true? (c/approvals-aligned? "scn-agree")))
    (is (false? (c/approvals-aligned? "scn-empty")))
    (is (nil? (c/approvals-aligned? "scn-self"))))
  (testing "deadlock and stuck"
    (is (true? (c/deadlocked? "scn-deadlock")))
    (is (false? (c/deadlocked? "scn-agree")))
    (is (true? (c/stuck? "scn-stuck")))
    (is (false? (c/stuck? "scn-open"))))
  (testing "threshold consensus"
    (is (true? (c/threshold-consensus? "scn-agree")))
    (is (false? (c/threshold-consensus? "scn-open"))))
  (testing "counts"
    (is (= 2 (c/count-approvals "scn-agree")))
    (is (= 2 (c/count-participants "scn-agree")))
    (is (= 0 (c/count-participants "scn-missing")))
    (is (= 10 (c/turns-since-last-proposal "scn-stuck")))
    (is (= 0 (c/turns-since-last-proposal "scn-open")))))

(deftest scenario-reads-test
  (is (= [w cr] (c/get-participants "scn-agree")))
  (is (= [] (c/get-participants "scn-missing")))
  (is (= 0.8 (:threshold (c/get-dialogue-config "scn-agree"))))
  (is (= c/default-config (c/get-dialogue-config "scn-missing")))
  (is (= :no-change (:signal (c/last-turn-for "scn-agree" "writer-1"))))
  (is (= :approve (:signal (c/last-turn-for "scn-agree" "critic"))) "fuzzy short-name match")
  (is (nil? (c/last-turn-for "scn-empty" w)))
  (is (= 4 (:turn-number (c/get-active-proposal "scn-deadlock"))))
  (is (nil? (c/get-active-proposal "scn-empty")))
  (is (m/validate [:maybe schema/Turn] (c/get-active-proposal "scn-open"))))

(deftest mediator-advice-arms-discriminate-test
  (let [arm (fn [id] (:reason (c/should-recruit-mediator? id)))]
    (is (= :deadlock (arm "scn-deadlock")))
    (is (= :stuck (arm "scn-stuck")))
    (is (nil? (arm "scn-agree")))))

(deftest consensus-schemas-generate-test
  (doseq [s [c/ConsensusStatus c/Count c/Ratio c/ProgressScore c/MediatorAdvice c/ConsensusResult]]
    (is (seq (mg/sample s {:size 4})) (pr-str s))))
