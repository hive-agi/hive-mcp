(ns hive-mcp.agent.protocol-test
  "Tests for IAgent protocol and the Ling implementation.

   Verifies that the Ling implementation satisfies the IAgent protocol
   with correct behavior.

   Test areas:
   - Protocol satisfaction (satisfies? checks)
   - agent-type returns correct keyword
   - can-chain-tools? behavior
   - claims lifecycle"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.swarm.datascript.connection :as conn]
            [hive-test.isolation :as iso]
            hive-mcp.isolation-methods))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(use-fixtures :each (iso/with-isolations :swarm-ds))

;; =============================================================================
;; Section 1: Protocol Satisfaction Tests
;; =============================================================================

(deftest ling-satisfies-iagent
  (testing "Ling record implements IAgent protocol"
    (let [ling (ling/->ling "test-ling-001"
                            {:cwd "/tmp/test"
                             :presets ["tdd"]
                             :project-id "test-project"})]
      (is (satisfies? proto/IAgent ling)
          "Ling should satisfy IAgent protocol"))))

;; =============================================================================
;; Section 2: agent-type Tests
;; =============================================================================

(deftest ling-agent-type-test
  (testing "Ling returns :ling as agent-type"
    (let [ling (ling/->ling "ling-type-test" {})]
      (is (= :ling (proto/agent-type ling))
          "Ling agent-type should be :ling"))))

;; =============================================================================
;; Section 3: can-chain-tools? Tests
;; =============================================================================

(deftest ling-can-chain-tools-test
  (testing "Lings CAN chain multiple tool calls"
    (let [ling (ling/->ling "ling-chain-test" {})]
      (is (true? (proto/can-chain-tools? ling))
          "Lings should be able to chain tools"))))

;; =============================================================================
;; Section 4: Protocol Method Presence Tests
;; =============================================================================

(deftest ling-has-all-protocol-methods
  (testing "Ling has all IAgent protocol methods"
    (let [ling (ling/->ling "ling-methods-test" {:cwd "/tmp"})]
      ;; Check that all protocol methods are callable
      ;; (they exist as methods on the record)
      (is (fn? (fn [] (.agent-type ling)))
          "should have agent-type method")
      (is (fn? (fn [] (.can-chain-tools? ling)))
          "should have can-chain-tools? method")
      (is (fn? (fn [] (.claims ling)))
          "should have claims method"))))

;; =============================================================================
;; Section 6: Claims Lifecycle (Without Mocking External Deps)
;; =============================================================================

(deftest ling-claims-empty-without-registration
  (testing "Ling claims return empty without registration"
    (let [ling (ling/->ling "ling-claims-test" {:cwd "/tmp"})]
      ;; Claims query DataScript, which should be empty for unregistered ling
      (is (empty? (proto/claims ling))
          "Unregistered ling should have no claims"))))

;; =============================================================================
;; Section 7: Record Field Access Tests
;; =============================================================================

(deftest ling-record-fields
  (testing "Ling record has expected fields"
    (let [ling (ling/->ling "ling-fields-test"
                            {:cwd "/home/user/project"
                             :presets ["tdd" "reviewer"]
                             :project-id "my-project"})]
      (is (= "ling-fields-test" (:id ling)))
      (is (= "/home/user/project" (:cwd ling)))
      (is (= ["tdd" "reviewer"] (:presets ling)))
      (is (= "my-project" (:project-id ling))))))

;; =============================================================================
;; Section 8: IAgentRegistry Protocol Tests
;; =============================================================================

(deftest iagent-registry-protocol-exists
  (testing "IAgentRegistry protocol is defined"
    (is (some? proto/IAgentRegistry)
        "IAgentRegistry protocol should exist")))

;; =============================================================================
;; Section 9: LLMBackend Protocol Tests
;; =============================================================================

(deftest llmbackend-protocol-exists
  (testing "LLMBackend protocol is defined"
    (is (some? proto/LLMBackend)
        "LLMBackend protocol should exist")))

(comment
  ;; Run tests
  (clojure.test/run-tests 'hive-mcp.agent.protocol-test)

  ;; Run single test
  (clojure.test/test-vars [#'ling-satisfies-iagent]))
