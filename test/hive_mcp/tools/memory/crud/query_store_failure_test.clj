(ns hive-mcp.tools.memory.crud.query-store-failure-test
  "A store that answers a failure instead of rows (timed-out or partial
   Milvus read, resilience failure value) makes memory query an error
   response, never an empty result. Regression: kanban
   20261029114433-1cb667ed — guard refresh! saw 0 candidates (:empty-load)
   because a slow read surfaced as [] through handle-query."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.memory.crud.query :as q]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn classify
  "Adapt store-read-failure to trifecta's unary port. Returns {:in :out}."
  [answer]
  {:in answer :out (q/store-read-failure answer)})

(deftrifecta store-read-failure-separates-rows-from-failures
  hive-mcp.tools.memory.crud.query-store-failure-test/classify
  {:golden-path "test/golden/hive-mcp/tools/memory/query-store-failure.edn"
   :cases {:rows       [{:id "a"}]
           :empty-rows []
           :nil-answer nil
           :milvus-incomplete {:error :milvus/read-incomplete :failed ["c"] :partial []}
           :resilience {:success? false :errors ["timeout"] :reconnecting? true}}
   :gen (gen/one-of
         [(gen/vector (gen/fmap (fn [id] {:id id}) gen/string-alphanumeric) 0 4)
          (gen/fmap (fn [k] {:error k}) gen/keyword)
          (gen/return {:success? false :errors ["x"]})])
   :pred (fn [{answer :in out :out}]
           (if (or (nil? answer) (sequential? answer))
             (nil? out)
             (= answer out)))
   :num-tests 100
   :mutations [["always-nil" (fn [a] {:in a :out nil})]
               ["identity"   (fn [a] {:in a :out a})]]})

(defn- answering-store
  "IMemoryStore stub whose query-entries answers `answer`."
  [answer]
  (reify mem-proto/IMemoryStore
    (connect! [_ _] nil) (disconnect! [_] nil) (connected? [_] true)
    (health-check [_] {})
    (add-entry! [_ e] (:id e))
    (get-entry [_ _] nil)
    (update-entry! [_ _ _] nil)
    (delete-entry! [_ _] nil)
    (query-entries [_ _] answer)
    (search-similar [_ _ _] []) (supports-semantic-search? [_] false)
    (cleanup-expired! [_] nil) (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil) (store-status [_] {})
    (reset-store! [_] nil)))

(defn- query-against
  [answer]
  (let [snapshot (mem-proto/registered-stores)]
    (try
      (mem-proto/register-store! :default (answering-store answer))
      (q/handle-query {:tags ["guard-rule"] :scope "all" :verbosity "full"
                       :limit 10 :directory "/tmp"})
      (finally
        (mem-proto/unregister-store! :default)
        (when-let [old (:default snapshot)]
          (mem-proto/register-store! :default old))))))

(deftest failed-read-is-an-error-not-an-empty-result
  (testing "a timed-out Milvus read answers an error response naming it"
    (let [resp (query-against {:error :milvus/read-incomplete :failed ["hive-mcp-memory"] :partial []})]
      (is (true? (:isError resp)))
      (is (str/includes? (str (:text resp)) "read-incomplete"))))
  (testing "the resilience failure value is an error response too"
    (is (true? (:isError (query-against {:success? false :errors ["deadline"] :reconnecting? true})))))
  (testing "an empty row vector stays an empty, successful answer"
    (let [resp (query-against [])]
      (is (not (:isError resp)))
      (is (= [] (json/read-str (:text resp)))))))
