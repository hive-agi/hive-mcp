(ns hive-mcp.tools.magit-pinning-test
  "Pinning tests for Magit MCP handlers.

   Tests cover the following handlers:
   - handle-magit-status: Get comprehensive git repository status
   - handle-magit-branches: Get branch information
   - handle-magit-log: Get recent commit log

   All tests use with-redefs to mock emacsclient calls and verify
   proper MCP response format {:type \"text\" :text ...}."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.tools.magit :as tools]
            [hive-mcp.test.stub.swarm-host :as sh]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn mock-emacsclient-success
  "A vessel answer that succeeds with RESULT for any op."
  [result]
  (fn [_op]
    {:success true :result result :timed-out false}))

(defn mock-emacsclient-failure
  "A vessel answer that fails with ERROR for any op."
  [error]
  (fn [_op]
    {:success false :error error :timed-out false}))

(defmacro with-mock-emacsclient
  "Execute body with a stub vessel whose :dispatch answers with MOCK-FN.

   Magit handlers reach the editor only through the closed `:vessel :dispatch`
   capability; the SPI registry is the seam, never a concrete client."
  [mock-fn & body]
  `(sh/with-swarm-host [_# (fn [op# _t#] (~mock-fn op#))]
     ~@body))

(defn- only-op
  "The single op map HOST received."
  [host]
  (let [[[op] :as calls] (sh/calls host)]
    (is (= 1 (count calls)) "exactly one dispatch")
    op))

;; =============================================================================
;; handle-magit-status Tests
;; =============================================================================

(deftest handle-magit-status-success-test
  (testing "Returns proper MCP response format on success"
    (let [mock-result "{\"branch\":\"main\",\"staged\":[],\"unstaged\":[],\"untracked\":[]}"]
      (with-mock-emacsclient (mock-emacsclient-success mock-result)
        (let [result (tools/handle-magit-status {})]
          (is (= "text" (:type result))
              "Response type should be 'text'")
          (is (string? (:text result))
              "Response text should be a string")
          (is (= mock-result (:text result))
              "Response text should match mock result")
          (is (nil? (:isError result))
              "Success response should not have isError"))))))

(deftest handle-magit-status-error-test
  (testing "Returns proper MCP error response on failure"
    (with-mock-emacsclient (mock-emacsclient-failure "Magit addon not available")
      (let [result (tools/handle-magit-status {})]
        (is (= "text" (:type result))
            "Error response type should be 'text'")
        (is (string? (:text result))
            "Error response text should be a string")
        (is (str/includes? (:text result) "Error:")
            "Error response should contain 'Error:' prefix")
        (is (str/includes? (:text result) "Magit addon not available")
            "Error response should contain the error message")
        (is (true? (:isError result))
            "Error response should have isError true")))))

(deftest handle-magit-status-with-directory-test
  (testing "Accepts directory parameter (even if ignored in current impl)"
    (with-mock-emacsclient (mock-emacsclient-success "{\"branch\":\"develop\"}")
      (let [result (tools/handle-magit-status {:directory "/path/to/repo"})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))))))

;; =============================================================================
;; handle-magit-branches Tests
;; =============================================================================

(deftest handle-magit-branches-success-test
  (testing "Returns proper MCP response format on success"
    (let [mock-result "{\"current\":\"main\",\"upstream\":\"origin/main\",\"local\":[\"main\",\"develop\"],\"remote\":[\"origin/main\",\"origin/develop\"]}"]
      (with-mock-emacsclient (mock-emacsclient-success mock-result)
        (let [result (tools/handle-magit-branches {})]
          (is (= "text" (:type result))
              "Response type should be 'text'")
          (is (string? (:text result))
              "Response text should be a string")
          (is (= mock-result (:text result))
              "Response text should match mock result")
          (is (nil? (:isError result))
              "Success response should not have isError"))))))

(deftest handle-magit-branches-error-test
  (testing "Returns proper MCP error response on failure"
    (with-mock-emacsclient (mock-emacsclient-failure "Not a git repository")
      (let [result (tools/handle-magit-branches {})]
        (is (= "text" (:type result))
            "Error response type should be 'text'")
        (is (string? (:text result))
            "Error response text should be a string")
        (is (str/includes? (:text result) "Error:")
            "Error response should contain 'Error:' prefix")
        (is (str/includes? (:text result) "Not a git repository")
            "Error response should contain the error message")
        (is (true? (:isError result))
            "Error response should have isError true")))))

(deftest handle-magit-branches-with-directory-test
  (testing "Accepts directory parameter"
    (with-mock-emacsclient (mock-emacsclient-success "{\"current\":\"feature/test\"}")
      (let [result (tools/handle-magit-branches {:directory "/custom/path"})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))))))

;; =============================================================================
;; handle-magit-log Tests
;; =============================================================================

(deftest handle-magit-log-success-test
  (testing "Returns proper MCP response format on success"
    (let [mock-result "[{\"hash\":\"abc123\",\"author\":\"dev\",\"date\":\"2024-01-15\",\"subject\":\"Initial commit\"}]"]
      (with-mock-emacsclient (mock-emacsclient-success mock-result)
        (let [result (tools/handle-magit-log {})]
          (is (= "text" (:type result))
              "Response type should be 'text'")
          (is (string? (:text result))
              "Response text should be a string")
          (is (= mock-result (:text result))
              "Response text should match mock result")
          (is (nil? (:isError result))
              "Success response should not have isError"))))))

(deftest handle-magit-log-error-test
  (testing "Returns proper MCP error response on failure"
    (with-mock-emacsclient (mock-emacsclient-failure "Git log failed")
      (let [result (tools/handle-magit-log {})]
        (is (= "text" (:type result))
            "Error response type should be 'text'")
        (is (string? (:text result))
            "Error response text should be a string")
        (is (str/includes? (:text result) "Error:")
            "Error response should contain 'Error:' prefix")
        (is (str/includes? (:text result) "Git log failed")
            "Error response should contain the error message")
        (is (true? (:isError result))
            "Error response should have isError true")))))

(deftest handle-magit-log-with-count-test
  (testing "Accepts count parameter"
    (sh/with-swarm-host [host (fn [_ _] {:success true :result "[]"})]
      (let [result (tools/handle-magit-log {:count 5})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))
        (is (= 5 (:count (only-op host))) "the op carries the count")))))

(deftest handle-magit-log-default-count-test
  (testing "Uses default count of 10 when not specified"
    (sh/with-swarm-host [host (fn [_ _] {:success true :result "[]"})]
      (let [result (tools/handle-magit-log {})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))
        (is (= 10 (:count (only-op host))) "default count of 10")))))

(deftest handle-magit-log-with-directory-test
  (testing "Accepts directory parameter"
    (with-mock-emacsclient (mock-emacsclient-success "[]")
      (let [result (tools/handle-magit-log {:count 3 :directory "/some/repo"})]
        (is (= "text" (:type result)))
        (is (nil? (:isError result)))))))

;; =============================================================================
;; Response Format Consistency Tests
;; =============================================================================

(deftest all-handlers-return-consistent-format-test
  (testing "All magit handlers return consistent MCP response format"
    (with-mock-emacsclient (mock-emacsclient-success "{}")
      ;; Test status
      (let [status-result (tools/handle-magit-status {})]
        (is (contains? status-result :type))
        (is (contains? status-result :text))
        (is (= "text" (:type status-result))))

      ;; Test branches
      (let [branches-result (tools/handle-magit-branches {})]
        (is (contains? branches-result :type))
        (is (contains? branches-result :text))
        (is (= "text" (:type branches-result))))

      ;; Test log
      (let [log-result (tools/handle-magit-log {})]
        (is (contains? log-result :type))
        (is (contains? log-result :text))
        (is (= "text" (:type log-result)))))))

(deftest all-handlers-error-format-consistent-test
  (testing "All magit handlers return consistent error format"
    (with-mock-emacsclient (mock-emacsclient-failure "Test error")
      ;; Test status error
      (let [status-result (tools/handle-magit-status {})]
        (is (= "text" (:type status-result)))
        (is (true? (:isError status-result)))
        (is (str/starts-with? (:text status-result) "Error:")))

      ;; Test branches error
      (let [branches-result (tools/handle-magit-branches {})]
        (is (= "text" (:type branches-result)))
        (is (true? (:isError branches-result)))
        (is (str/starts-with? (:text branches-result) "Error:")))

      ;; Test log error
      (let [log-result (tools/handle-magit-log {})]
        (is (= "text" (:type log-result)))
        (is (true? (:isError log-result)))
        (is (str/starts-with? (:text log-result) "Error:"))))))

;; =============================================================================
;; Elisp Generation Verification Tests
;; =============================================================================

(deftest elisp-calls-correct-functions-test
  (testing "handle-magit-status dispatches the closed :magit/status op"
    (sh/with-swarm-host [host (fn [_ _] {:success true :result "{}"})]
      (tools/handle-magit-status {:directory "/r"})
      (is (= {:op :magit/status :directory "/r"} (only-op host)))))

  (testing "handle-magit-branches dispatches the closed :magit/branches op"
    (sh/with-swarm-host [host (fn [_ _] {:success true :result "{}"})]
      (tools/handle-magit-branches {:directory "/r"})
      (is (= {:op :magit/branches :directory "/r"} (only-op host))))))

;; =============================================================================
;; Push Remote Targeting
;; =============================================================================

(deftest handle-magit-push-carries-remote-test
  (testing "An explicit remote reaches :magit/push as :remote"
    (sh/with-swarm-host [host (fn [_ _] {:success true :result "{}"})]
      (tools/handle-magit-push {:remote "github" :directory "/some/repo"})
      (is (= {:op :magit/push :set-upstream false :remote "github" :directory "/some/repo"}
             (only-op host))
          "The remote the caller named must reach api-push")))

  (testing "set_upstream and remote travel together"
    (sh/with-swarm-host [host (fn [_ _] {:success true :result "{}"})]
      (tools/handle-magit-push {:remote "github" :set_upstream true})
      (let [op (only-op host)]
        (is (true? (:set-upstream op)))
        (is (= "github" (:remote op))))))

  (testing "No remote and no upstream: the translator emits bare nil options"
    (sh/with-swarm-host [host (fn [_ _] {:success true :result "{}"})]
      (tools/handle-magit-push {})
      (let [op (only-op host)]
        (is (false? (:set-upstream op)))
        (is (nil? (:remote op))))))

  (testing "A blank remote is absent, not a remote named the empty string"
    (sh/with-swarm-host [host (fn [_ _] {:success true :result "{}"})]
      (tools/handle-magit-push {:remote "   "})
      (is (nil? (:remote (only-op host)))))))
