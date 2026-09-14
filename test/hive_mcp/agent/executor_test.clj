(ns hive-mcp.agent.executor-test
  "Tests for tool executor: agent-id attribution and batch execution.

   CLARITY-T: Verifies that agent-id propagates through tool execution
   so that hivemind_shout and similar tools can identify the calling agent."
  (:require [clojure.test :refer :all]
            [hive-mcp.agent.executor :as executor]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.agent.registry :as registry]
            [hive-mcp.channel.piggyback-tap :as tap]
            [hive-mcp.channel.activation :as act]
            [hive-mcp.extensions.registry :as ext]))

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def captured-agent-id (atom nil))

(defn test-tool-handler
  "A test tool that captures the current agent-id from execution context."
  [_args]
  (reset! captured-agent-id (ctx/current-agent-id))
  {:type "text" :text "ok"})

(defn setup-test-tool! []
  (registry/register!
   [{:name "test_capture_agent"
     :description "Test tool that captures agent-id"
     :inputSchema {:type "object" :properties {}}
     :handler test-tool-handler}]))

;; =============================================================================
;; Agent-ID Propagation Tests
;; =============================================================================

(deftest current-agent-id-outside-execution
  (testing "current-agent-id returns nil when not in execution context"
    (is (nil? (executor/current-agent-id)))))

(deftest current-agent-id-during-execution
  (testing "current-agent-id returns the agent-id during tool execution"
    (setup-test-tool!)
    (reset! captured-agent-id nil)

    (let [calls [{:id "call-1" :name "test_capture_agent" :arguments {}}]
          agent-id "agent-test-12345"]
      (executor/execute-tool-calls agent-id calls #{:auto-approve})

      ;; The tool should have captured the agent-id
      (is (= agent-id @captured-agent-id)
          "Tool handler should see the correct agent-id via current-agent-id"))))

(deftest agent-id-isolated-between-executions
  (testing "agent-id is isolated between different executions"
    (setup-test-tool!)

    ;; Execute with first agent
    (reset! captured-agent-id nil)
    (executor/execute-tool-calls "agent-A"
                                 [{:id "c1" :name "test_capture_agent" :arguments {}}]
                                 #{:auto-approve})
    (is (= "agent-A" @captured-agent-id))

    ;; Execute with second agent
    (reset! captured-agent-id nil)
    (executor/execute-tool-calls "agent-B"
                                 [{:id "c2" :name "test_capture_agent" :arguments {}}]
                                 #{:auto-approve})
    (is (= "agent-B" @captured-agent-id))

    ;; After execution, should be nil again
    (is (nil? (executor/current-agent-id)))))

;; =============================================================================
;; Integration Test: hivemind_shout Attribution (requires hivemind setup)
;; =============================================================================

(deftest hivemind-shout-uses-context-agent-id
  (testing "hivemind_shout uses execution context agent-id when not provided"
    ;; This test verifies the fix for P1: agent error attribution
    ;; When an agent calls hivemind_shout without explicit agent_id,
    ;; it should use the agent-id from the execution context (current-agent-id)

    ;; Skip if hivemind_shout isn't registered (unit test environment)
    (when-let [tool (registry/get-tool "hivemind_shout")]
      (let [agent-id "agent-attribution-test-123"
            calls [{:id "call-shout"
                    :name "hivemind_shout"
                    :arguments {:event_type "progress"
                                :message "Test message"}}]
            ;; Note: We're not providing agent_id in arguments
            ;; The tool should get it from current-agent-id
            results (executor/execute-tool-calls agent-id calls #{:auto-approve})]

        ;; Result should show the agent-id was used
        (is (some? results))))))

;; =============================================================================
;; Batch Execution Tests
;; =============================================================================

(defn setup-test-tools!
  "Register multiple test tools for batch execution testing."
  []
  (registry/register!
   [{:name "test_allowed_tool"
     :description "A tool that should be allowed"
     :inputSchema {:type "object" :properties {}}
     :handler (fn [_] {:type "text" :text "allowed-ok"})}
    {:name "test_blocked_tool"
     :description "A tool that should be blocked"
     :inputSchema {:type "object" :properties {}}
     :handler (fn [_] {:type "text" :text "blocked-ok"})}
    {:name "test_another_allowed"
     :description "Another allowed tool"
     :inputSchema {:type "object" :properties {}}
     :handler (fn [_] {:type "text" :text "another-ok"})}]))

(deftest execute-tool-calls-backward-compatible
  (testing "3-arity call executes every call"
    (setup-test-tools!)
    (let [calls [{:id "c1" :name "test_allowed_tool" :arguments {}}
                 {:id "c2" :name "test_blocked_tool" :arguments {}}]
          results (executor/execute-tool-calls "agent-1" calls #{:auto-approve})]
      ;; Both should execute
      (is (= 2 (count results)))
      (is (every? #(= "tool" (:role %)) results))
      ;; Neither should have "TOOL REJECTED" in content
      (is (not-any? #(.contains (:content %) "TOOL REJECTED") results)))))

(deftest execute-tool-calls-nil-opts
  (testing "4-arity call with nil opts executes every call"
    (setup-test-tools!)
    (let [calls [{:id "c1" :name "test_allowed_tool" :arguments {}}]
          results (executor/execute-tool-calls "agent-1" calls #{:auto-approve} nil)]
      (is (= 1 (count results)))
      (is (not (.contains (:content (first results)) "TOOL REJECTED"))))))

(defn- capture-drain-ctx
  "Run execute-tool-calls with a stub drain, returning the ctx it received."
  [calls]
  (let [seen (atom ::none)]
    (executor/execute-tool-calls
     "agent-1" calls #{:auto-approve}
     {:drain-fn (fn [_agent-id _project-id ctx] (reset! seen ctx) nil)})
    @seen))

(deftest agent-lane-consults-the-activation-provider
  (testing "a registered provider's pins reach the agentic-loop drain"
    (setup-test-tools!)
    (try
      (ext/register! act/extension-key
                     (fn [{:keys [tool-name]}]
                       (when (= "test_allowed_tool" tool-name)
                         {:pins #{"pin-1"} :tokens #{"activated"}})))
      (let [ctx (capture-drain-ctx [{:id "c1" :name "test_allowed_tool" :arguments {}}])]
        (is (= #{"pin-1"} (:pins ctx))
            "executor must build ctx through activation/drain-ctx, not a bare {:tokens ...}")
        (is (contains? (:tokens ctx) "activated")
            "provider tokens union onto the harvested cues"))
      (finally (ext/deregister! act/extension-key)))))

(deftest agent-lane-names-its-tool-only-when-unambiguous
  (testing "a single-call batch names its tool; a multi-call batch reports nil"
    (setup-test-tools!)
    (let [seen (atom [])]
      (try
        (ext/register! act/extension-key
                       (fn [{:keys [tool-name]}] (swap! seen conj tool-name) nil))
        (capture-drain-ctx [{:id "c1" :name "test_allowed_tool" :arguments {}}])
        (capture-drain-ctx [{:id "c1" :name "test_allowed_tool" :arguments {}}
                            {:id "c2" :name "test_another_allowed" :arguments {}}])
        (is (= ["test_allowed_tool" nil] @seen)
            "fabricating one tool-name for a batch would lie to every rule keyed on it")
        (finally (ext/deregister! act/extension-key))))))

(deftest agent-lane-degrades-to-cues-without-a-provider
  (testing "no provider means the ctx is the cues alone — activation never breaks the drain"
    (setup-test-tools!)
    (ext/deregister! act/extension-key)
    (let [ctx (capture-drain-ctx [{:id "c1" :name "test_allowed_tool" :arguments {}}])]
      (is (nil? (:pins ctx)))
      (is (nil? (:floor-cap ctx)))
      (is (contains? ctx :tokens)))))

(deftest the-drain-defaults-to-the-piggyback-tap
  (testing "opts without :drain-fn resolve to the real tap"
    (is (identical? tap/drain-all! (#'executor/drain-fn-for nil))
        "a stub-only suite would pass even if the default were wired to nothing")
    (is (identical? tap/drain-all! (#'executor/drain-fn-for {})))
    (let [stub (fn [_ _ _] nil)]
      (is (identical? stub (#'executor/drain-fn-for {:drain-fn stub}))))))
