(ns emacs-mcp.bug-regression-test
  "TDD tests pinning down bugs found in testing session 2025-12-31.
   These tests should FAIL until the bugs are fixed."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [emacs-mcp.tools :as tools]
            [emacs-mcp.org-clj.parser :as parser]
            [emacs-mcp.org-clj.render :as render]
            [emacs-mcp.prompt-capture :as pc]))

;; =============================================================================
;; BUG #1: parser/parse-file does not exist (HIGH)
;; Expected: A convenience function to parse a file directly
;; Actual: Only parse-document exists, requiring manual slurp
;; =============================================================================

(deftest test-parse-file-exists
  (testing "BUG #1: parser namespace should have parse-file function"
    (is (ifn? parser/parse-file)
        "parse-file function should exist in parser namespace")))

(deftest test-parse-file-works
  (testing "BUG #1: parse-file should parse org file directly from path"
    (let [test-file "/home/lages/dotfiles/gitthings/emacs-mcp/kanban.org"
          result (parser/parse-file test-file)]
      (is (map? result) "parse-file should return a map")
      (is (contains? result :headlines) "Result should contain :headlines")
      (is (pos? (count (:headlines result))) "Should have parsed headlines"))))

;; =============================================================================
;; BUG #2: CIDER eval returns feature name instead of result (HIGH)
;; Expected: {:text "6"} for (+ 1 2 3)
;; Actual: {:text "emacs-mcp-cider"}
;; =============================================================================

(deftest test-cider-eval-returns-result
  (testing "BUG #2: cider-eval-silent should return evaluation result, not feature name"
    (let [result (tools/handle-cider-eval-silent {:code "(+ 1 2 3)"})]
      (is (map? result) "Should return a map")
      (is (contains? result :text) "Should have :text key")
      ;; This test pins down the bug - previously returned "emacs-mcp-cider"
      (is (not= "emacs-mcp-cider" (:text result))
          "Should NOT return the feature name (original bug)")
      ;; In test environment CIDER may not be connected
      ;; Valid outcomes: either "6" (success) or error message about CIDER
      (let [text (str (:text result))]
        (is (or (str/includes? text "6")
                (str/includes? text "CIDER not connected")
                (str/includes? text "not loaded"))
            "Should return result or proper error, not feature name")))))

;; =============================================================================
;; BUG #3: list-prompts returns nil for some entry fields (MEDIUM)
;; Expected: All entries have :id, :created, :source populated
;; Actual: Second entry returns nil for these fields despite org file being correct
;; =============================================================================

(deftest test-prompt-list-returns-complete-entries
  (testing "BUG #3: list-prompts should return complete entries with all fields"
    (let [result (pc/list-prompts {})
          entries (:entries result)]
      (is (seq entries) "Should have at least one entry")
      (doseq [entry entries]
        (is (some? (:id entry))
            (str "Entry should have :id, got nil for: " (pr-str (select-keys entry [:prompt]))))
        (is (some? (:created entry))
            (str "Entry should have :created, got nil for: " (pr-str (select-keys entry [:prompt]))))
        (is (some? (:source entry))
            (str "Entry should have :source, got nil for: " (pr-str (select-keys entry [:prompt]))))))))

;; =============================================================================
;; BUG #4: Render stats vs column count mismatch (LOW)
;; Expected: Stats line and column headers should show same counts
;; Actual: Stats shows "4 todo" but column header shows "(54)"
;; =============================================================================

(deftest test-render-stats-match-columns
  (testing "BUG #4: Kanban render stats should match column counts"
    (let [test-file "/home/lages/dotfiles/gitthings/emacs-mcp/kanban.org"
          output (render/render-to-terminal test-file)]
      ;; Extract stats line
      (let [stats-match (re-find #"(\d+) todo" output)
            column-match (re-find #"TODO \((\d+)\)" output)]
        (when (and stats-match column-match)
          (let [stats-count (parse-long (second stats-match))
                column-count (parse-long (second column-match))]
            (is (= stats-count column-count)
                (format "Stats todo count (%d) should match column header count (%d)"
                        stats-count column-count))))))))

;; =============================================================================
;; BUG #5: handle-mcp-kanban-status times out (HIGH)
;; Expected: Should return within 5 seconds
;; Actual: Times out after 10+ seconds
;; =============================================================================

(deftest test-kanban-status-performance
  (testing "BUG #5: kanban-status should complete within reasonable time"
    (let [start (System/currentTimeMillis)
          ;; Use a timeout wrapper
          result (deref
                  (future (tools/handle-mcp-kanban-status {}))
                  5000 ; 5 second timeout
                  {:timeout true})]
      (is (not (:timeout result))
          "kanban-status should complete within 5 seconds")
      (when-not (:timeout result)
        (let [elapsed (- (System/currentTimeMillis) start)]
          (is (< elapsed 5000)
              (format "Should complete in <5s, took %dms" elapsed)))))))

;; =============================================================================
;; BUG #6: Swarm returns double-encoded JSON (LOW)
;; Expected: Clean parsed data structure
;; Actual: Returns escaped JSON strings within JSON
;; =============================================================================

(deftest test-swarm-status-clean-json
  (testing "BUG #6: swarm-status should return clean JSON, not double-encoded"
    (let [result (tools/handle-swarm-status {})]
      (is (map? result) "Should return a map")
      (when-let [text (get-in result [:content 0 :text])]
        ;; Check for double-encoding signs
        (is (not (str/includes? text "\\\\\\\""))
            "Should not have triple-escaped quotes (double-encoding sign)")
        (is (not (str/includes? text "\\\"{"))
            "Should not have escaped JSON object start")))))

;; =============================================================================
;; Helper to run all bug tests
;; =============================================================================

(defn run-bug-tests []
  (clojure.test/run-tests 'emacs-mcp.bug-regression-test))
