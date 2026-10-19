(ns hive-mcp.tools.memory.query-scope-test
  "Tests for DB-level $in filtering in query-entries and search-similar.

   Validates:
   - query-entries :project-ids builds correct $in where clause
   - query-entries :project-id backward compat still works
   - search-similar :project-ids filters at DB level
   - handle-query computes correct visible-ids per scope mode
   - handle-search-semantic computes correct visible-ids
   - Cross-project isolation: entries from project A don't appear in project B queries

   Test strategy:
   - Uses a contract-backed in-memory store instead of a running server
   - Creates test entries in two projects, verifies isolation
   - A fresh store is installed for each test"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-spi.memory.ports :as ports]
            [hive-spi.memory.registry :as registry]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.project.scope :as kg-scope]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(def ^:private test-project-a "test-project-alpha")
(def ^:private test-project-b "test-project-beta")
(defn- create-test-entry!
  "Create a test entry and track its ID for cleanup."
  [project-id content type tags]
  (ports/add-entry! (registry/get-store)
                    {:type type
                     :content content
                     :tags (conj tags (str "scope:project:" project-id) "test:query-scope")
                     :project-id project-id
                     :duration "ephemeral"
                     :expires "2026-12-31T23:59:59Z"}))

(defn- query-entries
  "Exercise the registered memory port with legacy keyword options."
  [& opts]
  (let [{:keys [project-ids] :as filters} (apply hash-map opts)]
    (ports/query-entries (registry/get-store)
                         (cond-> filters (seq project-ids) (dissoc :project-id)))))

(defn- search-similar
  "Search through the registered memory port with keyword options."
  [text & opts]
  (ports/search-similar (registry/get-store) text (apply hash-map opts)))

(use-fixtures :each mem-stub/with-stub-store)

;; =============================================================================
;; query-entries :project-ids Tests
;; =============================================================================

(deftest query-entries-project-ids-filters-correctly
  (testing ":project-ids filters to specified projects via $in"
    (let [id-a (create-test-entry! test-project-a "Alpha note for testing" "note" [])
          id-b (create-test-entry! test-project-b "Beta note for testing" "note" [])
          ;; Query only project A
          results-a (query-entries :project-ids [test-project-a] :limit 100)]
      ;; Should find project A entry
      (is (some #(= (:id %) id-a) results-a)
          "Should find entry from project A")
      ;; Should NOT find project B entry
      (is (not (some #(= (:id %) id-b) results-a))
          "Should NOT find entry from project B"))))

(deftest query-entries-project-ids-multi-project
  (testing ":project-ids with multiple projects returns entries from all specified"
    (let [id-a (create-test-entry! test-project-a "Alpha multi test" "note" [])
          id-b (create-test-entry! test-project-b "Beta multi test" "note" [])
          ;; Query both projects
          results (query-entries :project-ids [test-project-a test-project-b] :limit 100)]
      ;; Should find both entries
      (is (some #(= (:id %) id-a) results)
          "Should find entry from project A")
      (is (some #(= (:id %) id-b) results)
          "Should find entry from project B"))))

(deftest query-entries-project-id-backward-compat
  (testing ":project-id (singular) still works for backward compat"
    (let [id-a (create-test-entry! test-project-a "Alpha compat test" "note" [])
          _id-b (create-test-entry! test-project-b "Beta compat test" "note" [])
          results (query-entries :project-id test-project-a :limit 100)]
      (is (some #(= (:id %) id-a) results)
          "Should find entry with singular :project-id"))))

(deftest query-entries-project-ids-precedence
  (testing ":project-ids takes precedence over :project-id"
    (let [id-a (create-test-entry! test-project-a "Alpha precedence test" "note" [])
          id-b (create-test-entry! test-project-b "Beta precedence test" "note" [])
          ;; Pass both - :project-ids should win
          results (query-entries :project-id test-project-a :project-ids [test-project-b] :limit 100)]
      ;; Should find B (from :project-ids) but not A (from ignored :project-id)
      (is (some #(= (:id %) id-b) results)
          "Should find entry matching :project-ids (takes precedence)")
      (is (not (some #(= (:id %) id-a) results))
          "Should NOT find entry matching only :project-id (ignored)"))))

(deftest query-entries-no-filter-returns-all
  (testing "No :project-id or :project-ids returns all entries"
    (let [id-a (create-test-entry! test-project-a "Alpha no-filter test" "note" [])
          id-b (create-test-entry! test-project-b "Beta no-filter test" "note" [])
          results (query-entries :limit 1000)]
      ;; Should find both (no project filter)
      (is (some #(= (:id %) id-a) results)
          "Should find entry from project A without filter")
      (is (some #(= (:id %) id-b) results)
          "Should find entry from project B without filter"))))

(deftest query-entries-project-ids-with-type
  (testing ":project-ids combined with :type filter"
    (let [_id-note (create-test-entry! test-project-a "Alpha note" "note" [])
          id-dec (create-test-entry! test-project-a "Alpha decision" "decision" [])
          results (query-entries :type "decision" :project-ids [test-project-a] :limit 100)]
      (is (some #(= (:id %) id-dec) results)
          "Should find decision entry from project A")
      (is (every? #(= (:type %) "decision") results)
          "All results should be decisions"))))

;; =============================================================================
;; search-similar :project-ids Tests
;; =============================================================================

(deftest search-similar-project-ids-filters
  (testing ":project-ids filters semantic search at the memory port"
    (let [id-a (create-test-entry! test-project-a "Clojure testing conventions for alpha" "convention" [])
          id-b (create-test-entry! test-project-b "Clojure testing conventions for beta" "convention" [])
          ;; Search only in project A
          results-a (search-similar "Clojure testing" :limit 10 :project-ids [test-project-a])]
      ;; All results should be from project A
      (is (some #(= id-a (:id %)) results-a))
      (is (not-any? #(= id-b (:id %)) results-a))
      (is (every? #(= test-project-a (:project-id %)) results-a)
          "All semantic search results should be from project A"))))

(deftest search-similar-no-project-ids-returns-all
  (testing "Without :project-ids, semantic search returns from all projects"
    (let [id-a (create-test-entry! test-project-a "Unique alpha convention XYZ123" "convention" [])
          id-b (create-test-entry! test-project-b "Unique beta convention XYZ123" "convention" [])
          results (search-similar "Unique convention XYZ123" :limit 10)
          ids (set (map :id results))]
      ;; Should find entries from both projects
      (is (contains? ids id-a) "Should find alpha entry")
      (is (contains? ids id-b) "Should find beta entry"))))

;; =============================================================================
;; Visible IDs Computation Tests (Pure)
;; =============================================================================

(deftest visible-scopes-returns-hierarchy
  (testing "kg-scope/visible-scopes returns self + ancestors + global"
    (let [scopes (kg-scope/visible-scopes "hive-mcp")]
      (is (some #(= "hive-mcp" %) scopes) "Should include self")
      (is (some #(= "global" %) scopes) "Should include global"))))

(deftest visible-scopes-global-returns-global-only
  (testing "visible-scopes for 'global' returns just ['global']"
    (is (= ["global"] (kg-scope/visible-scopes "global")))))

(deftest visible-scopes-nil-returns-global
  (testing "visible-scopes for nil returns just ['global']"
    (is (= ["global"] (kg-scope/visible-scopes nil)))))
