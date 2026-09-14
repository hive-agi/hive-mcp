(ns hive-mcp.tools.memory-kanban.after-threshold-test
  "Regression: `created_after` / `updated_after` must compare timestamps as
   instants, not lexicographically (card 20260821214046-0016d5f0).

   A stored timestamp like `2026-08-21T16:55:25.040-03:00[America/Fortaleza]`
   (a ZonedDateTime string with zone id suffix) is 19:55Z, but a naive string
   comparison against threshold `2026-08-21T19:18:00Z` compares `16` vs `19`
   and falsely *excludes* the entry.

   This suite tests the pure predicate directly — no store, no with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.kanban.filters :as kf]))

;; Fixture helpers — plain entry maps, exactly what entry-after-ts? inspects.
(def ^:private zoned-entry
  "A card stored in Brazil (UTC-3), its :created carries a Java ZonedDateTime
   string with [America/Fortaleza] zone id suffix."
  {:id     "test-card-1"
   :type   "note"
   :tags   ["kanban" "todo"]
   :content {:title  "Brazil card"
             :status "todo"
             :created "2026-08-21T16:55:25.040-03:00[America/Fortaleza]"
             :updated "2026-08-21T16:55:25.040-03:00[America/Fortaleza]"}})

(def ^:private offset-entry
  "A card with a plain offset timestamp (no zone id suffix)."
  {:id     "test-card-2"
   :type   "note"
   :tags   ["kanban" "todo"]
   :content {:title  "Offset card"
             :status "todo"
             :created "2026-08-21T16:55:25-03:00"
             :updated "2026-08-21T16:55:25-03:00"}})

(def ^:private utc-zone-entry
  "A card whose :created is an ISO instant string in Z."
  {:id     "test-card-3"
   :type   "note"
   :tags   ["kanban" "todo"]
   :content {:title  "UTC card"
             :status "todo"
             :created "2026-08-21T19:55:25Z"
             :updated "2026-08-21T19:55:25Z"}})

;; ZonedDateTime string with [Zone/Id] — threshold in Z, entry is LATER
(deftest zoned-entry-included-when-threshold-earlier-in-z
  (testing "zoned 16:55-03:00 (19:55Z) > threshold 19:18:00Z -> INCLUDED"
    (is (true? (kf/entry-after-ts? zoned-entry :created "2026-08-21T19:18:00Z"))
        "19:55Z is after 19:18Z, must be included")))

(deftest zoned-entry-excluded-when-threshold-later-in-z
  (testing "zoned 16:55-03:00 (19:55Z) < threshold 20:00:00Z -> EXCLUDED"
    (is (false? (kf/entry-after-ts? zoned-entry :created "2026-08-21T20:00:00Z"))
        "19:55Z is NOT after 20:00Z, must be excluded")))

(deftest zoned-entry-included-with-local-threshold
  (testing "zoned 16:55-03:00 > threshold 16:18:50-03:00 -> INCLUDED"
    (is (true? (kf/entry-after-ts? zoned-entry :created "2026-08-21T16:18:50-03:00"))
        "Same offset comparison must also work")))

;; Offset-only string (no [Zone/Id]) — cross-offset pair
(deftest offset-entry-included-when-threshold-in-z
  (testing "offset 16:55:25-03:00 (19:55Z) > threshold 19:18:00Z -> INCLUDED"
    (is (true? (kf/entry-after-ts? offset-entry :created "2026-08-21T19:18:00Z"))
        "Offset-only string cross-offset must work")))

(deftest offset-entry-excluded-when-threshold-later-in-z
  (testing "offset 16:55:25-03:00 (19:55Z) < threshold 20:00:00Z -> EXCLUDED"
    (is (false? (kf/entry-after-ts? offset-entry :created "2026-08-21T20:00:00Z"))
        "Offset-only string correctly excluded")))

;; UTC/Z entry — simplest case
(deftest utc-entry-included-when-threshold-earlier
  (testing "UTC 19:55:25Z > threshold 19:18:00Z -> INCLUDED"
    (is (true? (kf/entry-after-ts? utc-zone-entry :created "2026-08-21T19:18:00Z")))))

(deftest utc-entry-excluded-when-threshold-later
  (testing "UTC 19:55:25Z < threshold 20:00:00Z -> EXCLUDED"
    (is (false? (kf/entry-after-ts? utc-zone-entry :created "2026-08-21T20:00:00Z")))))

(deftest utc-entry-included-with-offset-threshold
  (testing "UTC 19:55:25Z > threshold 16:18:50-03:00 -> INCLUDED"
    (is (true? (kf/entry-after-ts? utc-zone-entry :created "2026-08-21T16:18:50-03:00")))))

;; updated_after — same cross-offset pair on :updated
(deftest updated-after-zoned-cross-offset
  (testing "updated: zoned 16:55-03:00 (19:55Z) > threshold 19:18:00Z -> INCLUDED"
    (is (true? (kf/entry-after-ts? zoned-entry :updated "2026-08-21T19:18:00Z"))
        ":updated cross-offset must work")))

(deftest updated-after-zoned-excluded
  (testing "updated: zoned 16:55-03:00 (19:55Z) < threshold 20:00:00Z -> EXCLUDED"
    (is (false? (kf/entry-after-ts? zoned-entry :updated "2026-08-21T20:00:00Z"))
        ":updated cross-offset exclusion")))

;; Nil threshold — always included (current contract)
(deftest nil-threshold-matches-all
  (testing "nil threshold for :created -> INCLUDED"
    (is (true? (kf/entry-after-ts? zoned-entry :created nil)))))

(deftest nil-updated-threshold-matches-all
  (testing "nil threshold for :updated -> INCLUDED"
    (is (true? (kf/entry-after-ts? zoned-entry :updated nil)))))
