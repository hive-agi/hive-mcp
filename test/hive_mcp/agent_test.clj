(ns hive-mcp.agent-test
  "Pinning tests for agent tool permissions.

   Ensures the tier-3 (human approval) tool classification stays stable."
  (:require [clojure.test :refer :all]
            [hive-mcp.server.permissions :as permissions]))

;; =============================================================================
;; Test Data
;; =============================================================================

(def expected-tier-3-tools
  "Tier-3 tools that require human approval (from permissions module)."
  #{"bash" "magit_commit" "magit_push" "eval_elisp" "cider_eval_explicit"
    "preset_delete" "swarm_kill" "mcp_memory_cleanup_expired"})

;; =============================================================================
;; Pinning Tests - Permissions
;; =============================================================================

(deftest dangerous-tool-predicate-exists
  (testing "permissions/dangerous-tool? is defined"
    (is (fn? permissions/dangerous-tool?)
        "permissions/dangerous-tool? must be defined")))

(deftest tier-3-tools-match-expected
  (testing "permissions/dangerous-tool? returns true for tier-3 tools"
    (doseq [tool expected-tier-3-tools]
      (is (permissions/dangerous-tool? tool)
          (str tool " should be marked as dangerous"))))
  (testing "permissions/dangerous-tool? returns false for safe tools"
    (doseq [tool ["read_file" "grep" "glob_files"]]
      (is (not (permissions/dangerous-tool? tool))
          (str tool " should not be marked as dangerous")))))

(comment
  ;; Run tests in REPL
  (require '[clojure.test :refer [run-tests]])
  (run-tests 'hive-mcp.agent-test))
