(ns hive-mcp.tools.memory.crud.retrieve-read-failure-test
  "A store that could not read is a failed read: never an entry, never not-found."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.tools.memory.crud.retrieve :as retrieve]
            [hive-mcp.tools.memory.read-outcome :as read-outcome]
            [hive-mcp.protocols.memory :as mem-proto]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private outage
  "What hive-milvus get-entry answers once a transient cause outlives its retry."
  {:success? false :errors ["connection refused"] :reconnecting? true})

(def ^:private writes (atom []))

(defrecord MapStore [entries]
  mem-proto/IMemoryStore
  (connect! [_ _] nil)
  (disconnect! [_] nil)
  (connected? [_] true)
  (health-check [_] {:healthy? true})
  (add-entry! [_ _] nil)
  (get-entry [_ id] (get entries id))
  (update-entry! [_ id updates] (swap! writes conj [id updates]) (get entries id))
  (delete-entry! [_ _] nil)
  (query-entries [_ _] [])
  (search-similar [_ _ _] [])
  (supports-semantic-search? [_] false)
  (cleanup-expired! [_] nil)
  (entries-expiring-soon [_ _ _] nil)
  (find-duplicate [_ _ _ _] nil)
  (store-status [_] {:configured? true})
  (reset-store! [_] nil))

(defn- with-default-store
  "Run F with STORE in the default slot and an empty kanban slot, then restore."
  [store f]
  (let [prior (mem-proto/registered-stores)]
    (try
      (mem-proto/register-store! :default store)
      (mem-proto/register-store! :kanban (->MapStore {}))
      (f)
      (finally
        (doseq [slot [:default :kanban]]
          (if-let [p (get prior slot)]
            (mem-proto/register-store! slot p)
            (mem-proto/unregister-store! slot)))))))

(def ^:private entry {:id "here" :type "note" :content "real" :tags ["t"]})

(def ^:private store (->MapStore {"here" entry "down" outage}))

(defn- failed-read? [response]
  (and (:isError response)
       (re-find #"FAILED" (str (:text response)))
       (re-find #"connection refused" (str (:text response)))
       (not (re-find #"not found" (str (:text response))))))

(deftest entry-read-failure-reads-only-a-failed-answer
  (is (= "connection refused" (read-outcome/entry-read-failure outage)))
  (is (= ":milvus/read-incomplete"
         (read-outcome/entry-read-failure {:error :milvus/read-incomplete :failed []})))
  (is (nil? (read-outcome/entry-read-failure entry)) "an entry is no failure")
  (is (nil? (read-outcome/entry-read-failure nil)) "absence is no failure"))

(deftest get-full-reports-the-outage
  (with-default-store store
    (fn []
      (testing "an outage is an error naming its cause"
        (is (failed-read? (retrieve/handle-get-full {:id "down"}))))
      (testing "absence stays not-found"
        (let [answer (retrieve/handle-get-full {:id "ghost"})]
          (is (re-find #"Entry not found" (:text answer)))))
      (testing "an entry is still answered"
        (is (re-find #"real" (:text (retrieve/handle-get-full {:id "here"}))))))))

(deftest get-metadata-reports-the-outage
  (with-default-store store
    (fn []
      (is (failed-read? (retrieve/handle-get-metadata {:id "down"})))
      (is (re-find #"Entry not found: ghost"
                   (:text (retrieve/handle-get-metadata {:id "ghost"})))))))

(deftest batch-get-separates-unreadable-from-missing
  (with-default-store store
    (fn []
      (let [answer (json/read-str (:text (retrieve/handle-batch-get {:ids ["here" "down" "ghost"]}))
                                  :key-fn keyword)]
        (is (= 1 (:count answer)))
        (is (= ["here"] (mapv :id (:entries answer))))
        (is (= ["ghost"] (:missing answer)))
        (is (= [{:id "down" :cause "connection refused"}] (:unreadable answer)))))))

(deftest update-tags-writes-nothing-during-an-outage
  (reset! writes [])
  (with-default-store store
    (fn []
      (is (failed-read? (retrieve/handle-update-tags {:id "down" :tags ["x"]})))
      (is (= [] @writes)))))
