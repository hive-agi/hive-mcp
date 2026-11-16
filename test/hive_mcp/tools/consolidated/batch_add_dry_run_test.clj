(ns hive-mcp.tools.consolidated.batch-add-dry-run-test
  "A single-command memory batch never performs writes when asked for a dry run."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.memory :as c-mem]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- counting-runner
  "A batch runner that records every op it is handed and answers success."
  [seen]
  (fn [ops & _]
    (swap! seen into ops)
    {:results (mapv (fn [op] {:id (:id op) :success true :result {}}) ops)
     :summary {:total (count ops) :success (count ops) :failed 0}}))

(defn- batch-add [seen]
  (#'c-mem/make-single-command-batch :add (counting-runner seen)))

(def ^:private ops [{:content "x" :type "note"}])

(deftest batch-add-refuses-dry-run-without-writing
  (doseq [k [:dry-run :dry_run :dry-run?]
          v [true "true"]]
    (testing (str k " " v)
      (let [seen (atom [])
            res  ((batch-add seen) {:operations ops k v})]
        (is (:isError res) "a dry run is refused")
        (is (re-find #"no dry-run mode" (:text res)))
        (is (empty? @seen) "no op reached the runner")))))

(deftest batch-add-dry-run-false-still-writes
  (let [seen (atom [])
        res  ((batch-add seen) {:operations ops :dry-run false})]
    (is (not (:isError res)))
    (is (= 1 (count @seen)))))

(deftest dry-run-refusal-is-pure
  (is (nil? (c-mem/dry-run-refusal "add" {})))
  (is (nil? (c-mem/dry-run-refusal "add" {:dry_run "false"})))
  (is (string? (c-mem/dry-run-refusal "add" {:dry_run true}))))
