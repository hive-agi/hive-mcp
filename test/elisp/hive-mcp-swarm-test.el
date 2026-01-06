;;; hive-mcp-swarm-test.el --- ERT tests for hive-mcp-swarm -*- lexical-binding: t -*-

;; Copyright (C) 2025 BuddhiLW

;; Author: BuddhiLW
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Regression tests for pure functions in hive-mcp-swarm.
;; These tests focus on deterministic, side-effect-free functions.

;;; Code:

(require 'ert)

;; Load the module under test
(require 'hive-mcp-swarm)

;;;; Test: hive-mcp-swarm--generate-slave-id

(ert-deftest hive-mcp-swarm-test-generate-slave-id-format ()
  "Test that generate-slave-id produces correct format: swarm-NAME-TIMESTAMP."
  (let ((id (hive-mcp-swarm--generate-slave-id "tester")))
    ;; Should match pattern: swarm-<name>-<digits>
    (should (string-match-p "^swarm-tester-[0-9]+$" id))))

(ert-deftest hive-mcp-swarm-test-generate-slave-id-preserves-name ()
  "Test that the name is preserved in the generated ID."
  (let ((id1 (hive-mcp-swarm--generate-slave-id "agent-alpha"))
        (id2 (hive-mcp-swarm--generate-slave-id "worker")))
    (should (string-match-p "^swarm-agent-alpha-" id1))
    (should (string-match-p "^swarm-worker-" id2))))

(ert-deftest hive-mcp-swarm-test-generate-slave-id-has-timestamp ()
  "Test that the ID contains a Unix timestamp."
  (let* ((before (floor (float-time)))
         (id (hive-mcp-swarm--generate-slave-id "test"))
         (after (floor (float-time))))
    ;; Extract timestamp from id
    (string-match "swarm-test-\\([0-9]+\\)" id)
    (let ((timestamp (string-to-number (match-string 1 id))))
      ;; Timestamp should be within the time window
      (should (>= timestamp before))
      (should (<= timestamp after)))))

;;;; Test: hive-mcp-swarm--depth-label

(ert-deftest hive-mcp-swarm-test-depth-label-master ()
  "Test that depth 0 returns 'master'."
  (should (equal "master" (hive-mcp-swarm--depth-label 0))))

(ert-deftest hive-mcp-swarm-test-depth-label-child ()
  "Test that depth 1 returns 'child'."
  (should (equal "child" (hive-mcp-swarm--depth-label 1))))

(ert-deftest hive-mcp-swarm-test-depth-label-grandchild ()
  "Test that depth 2 returns 'grandchild'."
  (should (equal "grandchild" (hive-mcp-swarm--depth-label 2))))

(ert-deftest hive-mcp-swarm-test-depth-label-great-grandchild ()
  "Test that depth 3 returns 'great-grandchild'."
  (should (equal "great-grandchild" (hive-mcp-swarm--depth-label 3))))

(ert-deftest hive-mcp-swarm-test-depth-label-deep ()
  "Test that depth 4+ returns 'depth-N' format."
  (should (equal "depth-4" (hive-mcp-swarm--depth-label 4)))
  (should (equal "depth-10" (hive-mcp-swarm--depth-label 10)))
  (should (equal "depth-99" (hive-mcp-swarm--depth-label 99))))

;;;; Test: hive-mcp-swarm--slave-matches-filter

(ert-deftest hive-mcp-swarm-test-slave-matches-filter-nil-filter ()
  "Test that nil filter matches any slave."
  (let ((slave '(:slave-id "swarm-test-1" :role "tester" :status idle)))
    ;; With nil filter, should match
    (should (hive-mcp-swarm--slave-matches-filter slave nil))))

(ert-deftest hive-mcp-swarm-test-slave-matches-filter-role-match ()
  "Test that matching role filter returns t."
  (let ((slave '(:slave-id "swarm-test-1" :role "tester" :status idle)))
    (should (hive-mcp-swarm--slave-matches-filter slave '(:role "tester")))))

(ert-deftest hive-mcp-swarm-test-slave-matches-filter-role-mismatch ()
  "Test that non-matching role filter returns nil."
  (let ((slave '(:slave-id "swarm-test-1" :role "tester" :status idle)))
    (should-not (hive-mcp-swarm--slave-matches-filter slave '(:role "reviewer")))))

(ert-deftest hive-mcp-swarm-test-slave-matches-filter-status-match ()
  "Test that matching status filter returns t."
  (let ((slave '(:slave-id "swarm-test-1" :role "tester" :status idle)))
    (should (hive-mcp-swarm--slave-matches-filter slave '(:status idle)))))

(ert-deftest hive-mcp-swarm-test-slave-matches-filter-status-mismatch ()
  "Test that non-matching status filter returns nil."
  (let ((slave '(:slave-id "swarm-test-1" :role "tester" :status idle)))
    (should-not (hive-mcp-swarm--slave-matches-filter slave '(:status busy)))))

(ert-deftest hive-mcp-swarm-test-slave-matches-filter-combined-match ()
  "Test that both role AND status must match."
  (let ((slave '(:slave-id "swarm-test-1" :role "tester" :status idle)))
    ;; Both match
    (should (hive-mcp-swarm--slave-matches-filter
             slave '(:role "tester" :status idle)))
    ;; Role matches, status doesn't
    (should-not (hive-mcp-swarm--slave-matches-filter
                 slave '(:role "tester" :status busy)))
    ;; Status matches, role doesn't
    (should-not (hive-mcp-swarm--slave-matches-filter
                 slave '(:role "reviewer" :status idle)))))

(ert-deftest hive-mcp-swarm-test-slave-matches-filter-nil-role-in-slave ()
  "Test filter behavior when slave has nil role."
  (let ((slave '(:slave-id "swarm-test-1" :role nil :status idle)))
    ;; Nil role should not match explicit role filter
    (should-not (hive-mcp-swarm--slave-matches-filter slave '(:role "tester")))
    ;; But should match status-only filter
    (should (hive-mcp-swarm--slave-matches-filter slave '(:status idle)))))

(provide 'hive-mcp-swarm-test)
;;; hive-mcp-swarm-test.el ends here
