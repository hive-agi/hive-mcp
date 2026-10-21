(ns hive-mcp.tools.catchup.hierarchy-single-query-test
  "Pins the single-query hierarchy fetch: ONE backend query for the whole
   hierarchy (not one per descendant), the metadata projection kept, and
   the per-project cap applied in memory, newest first.

   The store is a reified IMemoryStore stub that records every query."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.catchup.hierarchy :as hier]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- recording-store
  "Stub store returning the `rows` whose project is queried, recording
   each opts map."
  [calls rows]
  (reify mem-proto/IMemoryStore
    (query-entries [_ opts]
      (swap! calls conj opts)
      (vec (filter #(some #{(:project-id %)} (:project-ids opts)) rows)))))

(defn- row [pid i]
  {:id (str pid "-" i) :project-id pid
   :created (format "2026-10-%02dT00:00:00Z" (inc i))})

(deftest one-query-for-the-whole-hierarchy-test
  (let [calls (atom [])
        pids  (mapv #(str "p" %) (range 40))
        store (recording-store calls (mapv #(row % 0) pids))
        out   (hier/chunked-hierarchy-fetch store pids 50)]
    (is (= 1 (count @calls)) "one backend query, not one per descendant")
    (let [opts (first @calls)]
      (is (= pids (:project-ids opts)))
      (is (= hier/metadata-projection (:output-fields opts)))
      (is (= (hier/single-query-limit 40) (:limit opts))))
    (is (= 40 (count out)))))

(deftest empty-hierarchy-issues-no-query-test
  (let [calls (atom [])]
    (is (= [] (hier/chunked-hierarchy-fetch (recording-store calls []) [] 50)))
    (is (empty? @calls))))

(deftest per-project-cap-keeps-newest-test
  (testing "a noisy project is capped; a quiet one keeps everything"
    (let [noisy (map #(row "noisy" %) (range 20))
          rows  (concat noisy (map #(row "quiet" %) (range 3)))
          out   (hier/cap-per-project 5 (reverse rows))
          by    (group-by :project-id out)]
      (is (= 5 (count (by "noisy"))))
      (is (= 3 (count (by "quiet"))))
      (is (= (set (map :id (take-last 5 noisy)))
             (set (map :id (by "noisy"))))
          "the newest five survive")
      (is (= out (sort-by :created #(compare %2 %1) out)) "newest first"))))

(deftest single-query-limit-is-bounded-test
  (is (= 50 (hier/single-query-limit 0)))
  (is (= 500 (hier/single-query-limit 10)))
  (is (= 16384 (hier/single-query-limit 1000))))
