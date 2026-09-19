(ns hive-mcp.guards-test
  "Tests for process-role guards.

   Covers:
   - Configuration helpers
   - Child ling detection and role/depth env handling"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-spi.swarm.guards :as guards]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn reset-guards [f]
  ;; Reset to defaults
  (guards/enable-guards!)
  (guards/set-enforcement-mode! :warn)
  (f))

(use-fixtures :each reset-guards)

;;; =============================================================================
;;; Configuration Tests
;;; =============================================================================

(deftest test-set-enforcement-mode
  (testing "set-enforcement-mode changes mode"
    (guards/set-enforcement-mode! :block)
    (is (= :block guards/*enforcement-mode*))
    (guards/set-enforcement-mode! :warn)
    (is (= :warn guards/*enforcement-mode*))))

(deftest test-enable-disable-guards
  (testing "enable/disable guards changes flag"
    (guards/disable-guards!)
    (is (not guards/*guard-enabled?*))
    (guards/enable-guards!)
    (is guards/*guard-enabled?*)))

(deftest test-guard-status
  (testing "guard-status returns configuration"
    (guards/set-enforcement-mode! :block)
    (let [status (guards/guard-status)]
      (is (:enabled? status))
      (is (= :block (:mode status))))))

;;; =============================================================================
;;; Child Ling Detection Tests
;;; =============================================================================

(deftest test-child-ling-detection
  (testing "child-ling? returns true when HIVE_MCP_ROLE is child-ling"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_MCP_ROLE" "child-ling" nil))]
      (is (true? (guards/child-ling?)))))

  (testing "child-ling? returns false when HIVE_MCP_ROLE is unset"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (is (false? (guards/child-ling?)))))

  (testing "child-ling? returns false when HIVE_MCP_ROLE is something else"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_MCP_ROLE" "coordinator" nil))]
      (is (false? (guards/child-ling?))))))

(deftest test-coordinator-detection
  (testing "coordinator? returns true when not child-ling"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (is (true? (guards/coordinator?)))))

  (testing "coordinator? returns false when child-ling"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_MCP_ROLE" "child-ling" nil))]
      (is (false? (guards/coordinator?))))))

(deftest test-get-role
  (testing "get-role returns child-ling when env is set"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_MCP_ROLE" "child-ling" nil))]
      (is (= "child-ling" (guards/get-role)))))

  (testing "get-role returns coordinator when env is unset"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (is (= "coordinator" (guards/get-role))))))

(deftest test-ling-depth
  (testing "ling-depth returns 0 when env is unset"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (is (= 0 (guards/ling-depth)))))

  (testing "ling-depth returns parsed integer from env"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_LING_DEPTH" "2" nil))]
      (is (= 2 (guards/ling-depth)))))

  (testing "ling-depth returns 0 for non-numeric string"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_LING_DEPTH" "not-a-number" nil))]
      (is (= 0 (guards/ling-depth)))))

  (testing "ling-depth returns 0 for empty string"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_LING_DEPTH" "" nil))]
      (is (= 0 (guards/ling-depth))))))

(deftest test-child-ling-env
  (testing "child-ling-env returns correct env map from coordinator (depth 0)"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (let [env (guards/child-ling-env)]
        (is (= "child-ling" (get env "HIVE_MCP_ROLE")))
        (is (= "1" (get env "HIVE_LING_DEPTH"))))))

  (testing "child-ling-env increments depth from existing value"
    (with-redefs [guards/get-env-var (fn [k] (case k "HIVE_LING_DEPTH" "3" nil))]
      (let [env (guards/child-ling-env)]
        (is (= "child-ling" (get env "HIVE_MCP_ROLE")))
        (is (= "4" (get env "HIVE_LING_DEPTH")))))))

(deftest test-guard-status-includes-child-ling-info
  (testing "guard-status includes child-ling fields"
    (with-redefs [guards/get-env-var (fn [_] nil)]
      (let [status (guards/guard-status)]
        (is (contains? status :child-ling?))
        (is (contains? status :role))
        (is (contains? status :ling-depth))
        (is (false? (:child-ling? status)))
        (is (= "coordinator" (:role status)))
        (is (= 0 (:ling-depth status)))))))
