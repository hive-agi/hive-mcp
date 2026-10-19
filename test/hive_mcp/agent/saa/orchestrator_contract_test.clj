(ns hive-mcp.agent.saa.orchestrator-contract-test
  "`hive-mcp.agent.saa.orchestrator` against its contracts: the coverage gate
   for src/hive_mcp/agent/saa, a trifecta over the factory, and the state
   readers driven through a real SAA cycle with the agent session injected as
   a stub that implements `IAgentSession`."
  (:require [clojure.core.async :as async :refer [<!! chan close! go put!]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.saa.orchestrator :as saa]
            [hive-mcp.protocols.agent-bridge :as bridge]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage saa-contracts-gate
  {:paths ["src/hive_mcp/agent/saa"]
   :exempt {}})

(use-fixtures :each (fn [f] (saa/clear-all-states!) (try (f) (finally (saa/clear-all-states!)))))

(defn- stub-session
  "An IAgentSession whose query! streams two messages and closes."
  [id]
  (reify bridge/IAgentSession
    (session-id [_] id)
    (query! [_ _prompt _opts]
      (let [ch (chan 10)]
        (go (put! ch {:type :message :content "observed"})
            (put! ch {:type :complete :content "done"})
            (close! ch))
        ch))
    (interrupt! [_] {:success? true :errors []})
    (receive-messages [_] (let [ch (chan)] (close! ch) ch))
    (receive-response [_] (let [ch (chan)] (close! ch) ch))))

(defn- drain! [ch] (loop [acc []] (if-let [m (<!! ch)] (recur (conj acc m)) acc)))

(hst/deftrifecta-from-schema saa-orchestrator-factory-contract
  hive-mcp.agent.saa.orchestrator/->saa-orchestrator
  {:in        [:maybe [:map
                       [:shout? {:optional true} :boolean]
                       [:max-act-turns {:optional true} [:int {:min 1 :max 500}]]
                       [:score-threshold {:optional true} [:double {:min 0.0 :max 1.0}]]]]
   :out       saa/OrchestratorValue
   ;; caller overrides win; every unnamed budget keeps its default
   :rel       (fn [in out]
                (let [cfg (:config out)]
                  (and (every? (fn [[k v]] (= v (get cfg k))) in)
                       (= (get in :shout? true) (:shout? cfg))
                       (= 50 (:max-silence-turns cfg))
                       (= 20 (:max-abstract-turns cfg))
                       (some? (:phase-provider cfg)))))})

(deftest state-readers-follow-a-full-cycle-test
  (let [orch (saa/->saa-orchestrator {:shout? false})
        msgs (drain! (bridge/run-full-saa! orch (stub-session "saa-a") "fix the bug" {}))]
    (testing "the cycle ends complete and the readers say so"
      (is (= :saa-complete (:type (last msgs))))
      (is (= :complete (saa/agent-saa-phase "saa-a")))
      (is (m/validate [:maybe saa/SaaState] (saa/agent-saa-state "saa-a")))
      (is (= ["observed" "done"] (:observations (saa/agent-saa-state "saa-a")))))
    (testing "a completed agent is not active"
      (is (empty? (saa/list-active-saa))))
    (testing "clear-completed removes exactly the finished agents"
      (is (= {:cleared 1} (saa/clear-completed-states!)))
      (is (nil? (saa/agent-saa-state "saa-a")))
      (is (= {:cleared 0} (saa/clear-completed-states!))))))

(deftest a-phase-without-init-leaves-a-partial-state-test
  (testing "run-act! on its own builds its state from nothing: no task, no start
            time, and a first history entry with no phase"
    (let [orch (saa/->saa-orchestrator {:shout? false})]
      (drain! (bridge/run-act! orch (stub-session "saa-b") "plan" {}))
      (let [state (saa/agent-saa-state "saa-b")]
        (is (m/validate [:maybe saa/SaaState] state))
        (is (= :complete (saa/agent-saa-phase "saa-b")))
        (is (not (contains? state :started-at)))
        (is (nil? (:phase (first (:phase-history state)))))))))

(deftest active-agents-are-listed-test
  (let [orch (saa/->saa-orchestrator {:shout? false})
        gate (chan)
        session (reify bridge/IAgentSession
                  (session-id [_] "saa-c")
                  (query! [_ _ _] gate)
                  (interrupt! [_] {:success? true :errors []})
                  (receive-messages [_] (let [ch (chan)] (close! ch) ch))
                  (receive-response [_] (let [ch (chan)] (close! ch) ch)))
        out (bridge/run-silence! orch session "task" {})]
    (testing "an agent blocked in silence is listed as active"
      (let [active (saa/list-active-saa)]
        (is (= [:silence] (mapv :phase active)))
        (is (m/validate (-> (m/function-schemas)
                            (get-in ['hive-mcp.agent.saa.orchestrator 'list-active-saa :schema])
                            m/children second)
                        active))))
    (close! gate)
    (drain! out)
    (is (nil? (saa/clear-all-states!)))
    (is (nil? (saa/agent-saa-state "saa-c")))))

(deftest saa-schemas-generate-test
  (doseq [s [saa/SaaPhase saa/AgentId saa/SaaState saa/OrchestratorValue]]
    (is (seq (mg/sample s {:size 4})) (pr-str s))))
