(ns hive-mcp.protocols.memory-read-test
  "A store error Result (hive-milvus :milvus/read-incomplete) reaches hive-mcp
   callers of query-entries / search-similar / get-entries as an empty vector
   marked incomplete, never as a map they would walk as entries."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.protocols.memory-read :as mread]
            [hive-spi.memory.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def incomplete-answer
  {:error   :milvus/read-incomplete
   :failed  [{:collection "hive_mcp_memory_2560d" :message "milvus query-scalar timed out after 5000ms"}]
   :partial [{:id "a"} {:id "b"}]})

(defrecord AnswerStore [answer]
  ports/IMemoryStore
  (connect! [_ _] {:success? true})
  (disconnect! [_] nil)
  (connected? [_] true)
  (health-check [_] {:healthy? true})
  (add-entry! [_ e] (:id e))
  (get-entry [_ _] nil)
  (update-entry! [_ _ _] nil)
  (delete-entry! [_ _] true)
  (query-entries [_ _] answer)
  (search-similar [_ _ _] answer)
  (supports-semantic-search? [_] true)
  (cleanup-expired! [_] {:count 0 :deleted-ids []})
  (entries-expiring-soon [_ _ _] [])
  (find-duplicate [_ _ _ _] nil)
  (store-status [_] {:backend "answer-stub"})
  (reset-store! [_] true)
  ports/IMemoryStoreBatch
  (get-entries [_ _] answer))

(deftest error-result-degrades-at-every-read-port
  (let [store (->AnswerStore incomplete-answer)]
    (doseq [[op answer] [[:query-entries  (proto/query-entries store {:limit 5})]
                         [:search-similar (proto/search-similar store "q" {:limit 5})]
                         [:get-entries    (proto/get-entries store ["a" "b"])]]]
      (testing (name op)
        (is (vector? answer))
        (is (= (:partial incomplete-answer) answer) "the partial rows are kept")
        (is (= 2 (count answer)) "count no longer walks the error map's keys")
        (is (= {:op op
                :error :milvus/read-incomplete
                :failed (:failed incomplete-answer)
                :partial-count 2
                :partial (:partial incomplete-answer)}
               (mread/incomplete answer)))))))

(deftest failed-read-with-no-partial-rows-is-empty-and-marked
  (let [answer (mread/read-answer :query-entries (assoc incomplete-answer :partial []))]
    (is (= [] answer))
    (is (= :milvus/read-incomplete (:error (mread/incomplete answer))))
    (is (not (contains? (mread/incomplete answer) :partial)))))

(deftest complete-reads-pass-through-unchanged
  (let [rows  [{:id "a" :content "x"} {:id "b" :content "y"}]
        store (->AnswerStore rows)]
    (is (= rows (proto/query-entries store {})))
    (is (= rows (proto/search-similar store "q" {})))
    (is (= rows (proto/get-entries store ["a" "b"])))
    (is (nil? (mread/incomplete (proto/query-entries store {}))))
    (testing "degraded search metadata survives"
      (let [d (ports/degraded-search-result {:reason :no-embedder})]
        (is (identical? d (proto/search-similar (->AnswerStore d) "q" {})))))
    (testing "nil (no ids asked) stays nil"
      (is (nil? (mread/read-answer :get-entries nil))))))

(deftest projected-batch-read-survives-an-error-result
  (is (= [{:id "a"} {:id "b"}]
         (proto/get-entries-projected (->AnswerStore incomplete-answer)
                                      ["a"] {:output-fields ["id"]}))))

(deftrifecta read-answer-never-answers-a-map
  mread/read-answer
  {:cases {:incomplete [:query-entries incomplete-answer]
           :entries    [:search-similar [{:id "x"}]]
           :empty      [:get-entries []]
           :other-map  [:query-entries {:unexpected true}]}
   :gen (gen/tuple (gen/elements [:query-entries :search-similar :get-entries])
                   (gen/one-of [(gen/vector (gen/hash-map :id gen/string-alphanumeric))
                                (gen/hash-map :error gen/keyword
                                              :partial (gen/vector (gen/return {:id "p"})))]))
   :apply? true
   :pred (fn [answer] (and (sequential? answer) (every? map? answer)))
   :num-tests 50
   :mutations [["pass-error-through" (fn [_ answer] answer)]
               ["drop-the-mark"      (fn [_ answer] (if (map? answer) (vec (:partial answer)) answer))]
               ["swallow-entries"    (fn [_ _] [])]]
   :assert (fn []
             (let [d (mread/read-answer :query-entries incomplete-answer)]
               (is (= (:partial incomplete-answer) d))
               (is (= :milvus/read-incomplete (:error (mread/incomplete d)))))
             (is (= [{:id "x"}] (mread/read-answer :search-similar [{:id "x"}]))))})
