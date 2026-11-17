(ns hive-mcp.tools.memory.crud.query-duration-test
  "Duration filter of memory query.

   Regression: friction 20261102195114-54a37a0b. The store answers :duration
   as a keyword (:permanent) while the request carries a string
   (\"permanent\"), so `memory query type=axiom duration=permanent` answered []
   although every axiom is permanent. An unknown duration is refused instead
   of answering an empty list."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.memory.crud.query :as q]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private entries
  [{:id "kw"  :created "2026-07-13T08:00:00" :duration :permanent :tags []}
   {:id "str" :created "2026-07-12T08:00:00" :duration "permanent" :tags []}
   {:id "med" :created "2026-07-11T08:00:00" :duration :medium :tags []}])

(defn- run [duration]
  (mapv :id (#'q/apply-post-filters entries {:tags [] :duration duration :limit-val 10})))

(deftest duration-filter-matches-keyword-and-string-durations
  (testing "a string request matches rows whose stored duration is a keyword"
    (is (= ["kw" "str"] (run "permanent")))
    (is (= ["med"] (run "medium"))))
  (testing "no duration keeps every row"
    (is (= ["kw" "str" "med"] (run nil)))))

(deftest unknown-duration-is-refused-not-answered-empty
  (let [resp (q/handle-query {:type "axiom" :duration "forever" :directory "/tmp"})]
    (is (true? (:isError resp)))
    (is (re-find #"duration" (pr-str resp)))))
