(ns hive-mcp.agent.ling.inbox-seq-test
  "Per-ling send sequence: each dispatch names the last accepted one as the
   message it must not overtake."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling.inbox-seq :as seqs]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [t] (seqs/forget! "l1") (seqs/forget! "l2") (t)
                      (seqs/forget! "l1") (seqs/forget! "l2")))

(deftest seqs-count-up-per-ling-in-one-epoch
  (let [a (seqs/stamp! "l1")
        b (seqs/stamp! "l1")
        c (seqs/stamp! "l2")]
    (is (= [1 2] [(:seq a) (:seq b)]))
    (is (= 1 (:seq c)) "each ling has its own sequence")
    (is (= seqs/epoch (:epoch a) (:epoch c)))))

(deftest a-stamp-names-the-last-accepted-send
  (let [s1 (seqs/stamp! "l1")]
    (is (nil? (:after s1)) "the first message waits for nothing")
    (seqs/accepted! "l1" s1)
    (let [s2 (seqs/stamp! "l1")]
      (is (= 1 (:after s2)))
      (testing "a send that failed is never accepted, so nothing waits for it"
        (let [s3 (seqs/stamp! "l1")]
          (is (= 1 (:after s3)) "s2 was not accepted (yet)")))
      (seqs/accepted! "l1" s2)
      (is (= 2 (:after (seqs/stamp! "l1")))))))

(deftest acceptance-never-moves-backwards-or-across-epochs
  (let [s1 (seqs/stamp! "l1")
        s2 (seqs/stamp! "l1")]
    (seqs/accepted! "l1" s2)
    (seqs/accepted! "l1" s1)
    (is (= 2 (:after (seqs/stamp! "l1"))) "a late acceptance of s1 does not lower it")
    (seqs/accepted! "l1" {:epoch "another-host" :seq 99})
    (is (= 2 (:after (seqs/stamp! "l1"))))))

(deftest forget-restarts-the-ling
  (seqs/accepted! "l1" (seqs/stamp! "l1"))
  (seqs/forget! "l1")
  (is (= {:seq 1 :after nil} (select-keys (seqs/stamp! "l1") [:seq :after]))))
