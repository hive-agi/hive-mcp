(ns hive-mcp.tools.memory.crud.query-read-outcome-test
  "memory query and memory expiring report what the store read answered.

   Measured on the live server 2026-11-02: `memory query type=axiom
   scope=all limit=100` returned three rows with null id, type and created.
   Those three rows are the three keys of the hive-milvus error Result
   `{:error :milvus/read-incomplete :failed [..] :partial [..]}`, which the
   handler iterated as if it were a sequence of entries. With a duration
   filter the same answer became [], an empty result that read as absence.

   Every test here runs against the atom-backed stub store, wrapped where a
   test needs the store to answer what hive-milvus answers on a failed or
   partial read. Nothing touches a production store."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-spi.memory.ports :as ports]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.memory.crud.query :as q]
            [hive-mcp.tools.memory.lifecycle :as lifecycle]
            [hive-mcp.tools.memory.read-outcome :as ro]
            [hive-mcp.tools.memory.scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each stub/with-stub-store)

(defn- axiom [i & {:as more}]
  (merge {:id (format "2026010100%04d-ax" i)
          :type "axiom"
          :content (str "axiom " i)
          :tags ["axiom" "scope:global"]
          :project-id "global"
          :duration "permanent"
          :created (format "2026-01-01T00:%02d:%02d" (quot i 60) (mod i 60))}
         more))

(defrecord AnswerStore [inner answer]
  ports/IMemoryStore
  (connect! [_ c] (ports/connect! inner c))
  (disconnect! [_] (ports/disconnect! inner))
  (connected? [_] true)
  (health-check [_] (ports/health-check inner))
  (add-entry! [_ e] (ports/add-entry! inner e))
  (get-entry [_ id] (ports/get-entry inner id))
  (update-entry! [_ id u] (ports/update-entry! inner id u))
  (delete-entry! [_ id] (ports/delete-entry! inner id))
  (query-entries [_ opts] (answer :query (ports/query-entries inner opts) opts))
  (search-similar [_ t o] (ports/search-similar inner t o))
  (supports-semantic-search? [_] true)
  (cleanup-expired! [_] (ports/cleanup-expired! inner))
  (entries-expiring-soon [_ d o] (answer :expiring (ports/entries-expiring-soon inner d o) o))
  (find-duplicate [_ t h o] (ports/find-duplicate inner t h o))
  (store-status [_] (ports/store-status inner))
  (reset-store! [_] (ports/reset-store! inner)))

(defn- install-answering!
  "Install a stub seeded with ENTRIES whose reads are rewritten by ANSWER
   (fn [op rows opts] -> what the store answers)."
  [entries answer]
  (stub/install! (->AnswerStore (stub/->stub entries) answer)))

(defn- incomplete
  "What hive-milvus `read-outcome` answers when a collection fails."
  [rows]
  {:error :milvus/read-incomplete
   :failed [{:collection "hive_mcp_memory_2560d" :message "DEADLINE_EXCEEDED"}]
   :partial (vec rows)})

(defn- body [resp]
  (json/read-str (:text resp) :key-fn keyword))

(defn- rows-of [resp]
  (let [b (body resp)] (if (map? b) (:results b) b)))

(deftest classify-folds-every-answer-shape
  (is (= :complete (:status (ro/classify [{:id "a"}]))))
  (is (= :incomplete (:status (ro/classify (incomplete [{:id "a"}])))))
  (is (= [{:id "a"}] (:rows (ro/classify (incomplete [{:id "a"}])))))
  (is (= :failed (:status (ro/classify (incomplete [])))))
  (is (= :failed (:status (ro/classify {:success? false :errors ["UNAVAILABLE"]}))))
  (is (= [] (:rows (ro/classify {:success? false :errors ["UNAVAILABLE"]})))))

(deftest error-result-never-becomes-null-id-rows
  (install-answering! [(axiom 1)] (fn [op rows _] (if (= op :query) (incomplete []) rows)))
  (let [resp (q/handle-query {:type "axiom" :scope "all" :limit 100 :directory "/tmp"})
        rows (rows-of resp)]
    (testing "no row without an id is returned"
      (is (every? :id rows)))
    (testing "the empty answer carries a warning naming the failure"
      (is (empty? rows))
      (is (re-find #"(?i)failed" (pr-str (:warnings (body resp))))))))

(deftest partial-read-returns-partial-rows-with-a-warning
  (install-answering! [(axiom 1) (axiom 2)] (fn [op rows _] (if (= op :query) (incomplete (take 1 rows)) rows)))
  (let [resp (q/handle-query {:type "axiom" :scope "all" :limit 100 :directory "/tmp"})
        rows (rows-of resp)]
    (is (= 1 (count rows)))
    (is (every? :id rows))
    (is (re-find #"(?i)incomplete" (pr-str (:warnings (body resp)))))))

(deftest duration-filter-on-failed-read-is-not-absence
  (install-answering! [(axiom 1)] (fn [op rows _] (if (= op :query) (incomplete []) rows)))
  (let [resp (q/handle-query {:type "axiom" :scope "all" :duration "long" :directory "/tmp"})]
    (is (empty? (rows-of resp)))
    (is (seq (:warnings (body resp))))))

(deftest duration-filter-sees-past-the-store-window
  (testing "a duration the store does not filter must not be starved by the fetch window"
    (let [perm (mapv #(axiom %) (range 1 200))
          long-ones [(axiom 500 :duration "long" :created "2025-01-01T00:00:00")]]
      (install-answering! (into perm long-ones) (fn [_ rows _] rows))
      (let [resp (q/handle-query {:type "axiom" :scope "all" :duration "long" :limit 5 :directory "/tmp"})
            rows (rows-of resp)]
        (is (or (= ["2026010100500-ax"] (mapv :id rows))
                (seq (:warnings (body resp))))
            "either the long entry is found or the answer says the window truncated it")))))

(deftest clean-read-keeps-the-plain-vector-shape
  (install-answering! [(axiom 1)] (fn [_ rows _] rows))
  (let [resp (q/handle-query {:type "axiom" :scope "all" :directory "/tmp"})]
    (is (vector? (body resp)))
    (is (= 1 (count (body resp))))))

(deftest expiring-failed-read-warns
  (install-answering! [] (fn [op rows _] (if (= op :expiring) {:success? false :errors ["UNAVAILABLE"]} rows)))
  (let [resp (lifecycle/handle-expiring-soon {:days 120 :directory "/tmp"})]
    (is (not (:isError resp)))
    (is (seq (:warnings (body resp))))))

(defn- expires-in-days [d]
  (str (.plusDays (java.time.ZonedDateTime/now) d)))

(deftest expiring-finds-an-entry-90-days-ahead-in-a-child-project
  (testing "an entry of project hive is found from a directory resolving to hive"
    (install-answering! [{:id "20261102194055-2dba76dd" :type "decision" :duration "long"
                          :project-id "hive" :tags ["decision" "scope:project:hive"]
                          :created "2026-11-02T19:40:55" :expires (expires-in-days 90)}]
                        (fn [_ rows _] rows))
    (with-redefs [hive-mcp.tools.memory.scope/get-current-project-id (fn [& _] "hive")]
      (let [rows (rows-of (lifecycle/handle-expiring-soon {:days 120 :limit 50 :directory "/tmp/hive"}))]
        (is (= ["20261102194055-2dba76dd"] (mapv :id rows)))))))

(deftest expiring-limit-applies-after-the-soonest-first-sort
  (install-answering! [{:id "late" :type "decision" :duration "long" :project-id "global"
                        :tags ["scope:global"] :expires (expires-in-days 60)}
                       {:id "soon" :type "decision" :duration "long" :project-id "global"
                        :tags ["scope:global"] :expires (expires-in-days 2)}]
                      (fn [_ rows _] (reverse rows)))
  (let [rows (rows-of (lifecycle/handle-expiring-soon {:days 120 :limit 1 :scope "all" :directory "/tmp"}))]
    (is (= ["soon"] (mapv :id rows)))))

(deftest expiring-type-filter-is-applied
  (install-answering! [{:id "ax" :type "axiom" :duration "long" :project-id "global"
                        :tags ["scope:global"] :expires (expires-in-days 5)}
                       {:id "dc" :type "decision" :duration "long" :project-id "global"
                        :tags ["scope:global"] :expires (expires-in-days 5)}]
                      (fn [_ rows _] rows))
  (let [rows (rows-of (lifecycle/handle-expiring-soon {:days 30 :type "axiom" :scope "all" :directory "/tmp"}))]
    (is (= ["ax"] (mapv :id rows)))))

(deftest expiring-refuses-unknown-params
  (let [resp (lifecycle/handle-expiring-soon {:days 30 :fields ["id"] :directory "/tmp"})]
    (is (:isError resp))
    (is (re-find #"fields" (:text resp)))))
