(ns hive-mcp.tools.memory.migration.paging-test
  "migrate-scoped over a tag-filter that selects thousands of entries.

   The tag-filter path used to read every candidate in ONE {:limit 10000}
   query and move them all before answering, so a ~2.5k-chunk document never
   answered inside the tool timeout. These tests hold the paged shape: a store
   with IMemoryStorePage is walked by cursor in bounded pages, every page is
   moved before the next is read, and a call whose budget runs out answers a
   partial count instead of hanging."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.memory.migration.core :as migration]
            [hive-spi.memory.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- seed [n]
  (into (sorted-map)
        (map (fn [i]
               (let [id (format "chunk-%05d" i)]
                 [id {:id id :project-id "dbs"
                      :tags ["ingestion-chunk" "source:e2fb" "scope:project:dbs"]}])))
        (range n)))

(defrecord PagedStore [db calls]
  ports/IMemoryStore
  (get-entry [_ id] (get @db id))
  (update-entry! [_ id updates] (swap! db update id merge updates))
  (query-entries [_ opts]
    (swap! calls update :query-entries (fnil conj []) opts)
    (throw (ex-info "a paged store must not be asked for one big scan" {})))

  ports/IMemoryStoreBatch
  (get-entries [_ ids] (keep #(get @db %) ids))

  ports/IMemoryStoreMetadataWrite
  (update-metadata! [_ id updates] (swap! db update id merge updates))

  ports/IMemoryStorePage
  (query-page [_ {:keys [project-id tags limit cursor] :as opts}]
    (swap! calls update :query-page (fnil conj []) (dissoc opts :cursor))
    (let [match (->> (vals @db)
                     (filter #(= project-id (:project-id %)))
                     (filter #(every? (set (:tags %)) tags))
                     (filter #(or (nil? cursor) (pos? (compare (:id %) cursor))))
                     (take limit)
                     vec)]
      {:rows        (mapv #(select-keys % [:id :tags]) match)
       :next-cursor (when (= limit (count match)) (:id (peek match)))})))

(defn- run [store args]
  (with-redefs [mem-proto/store-set? (fn [] true)
                mem-proto/get-store  (fn [] store)]
    (let [res (migration/handle-migrate-scoped args)]
      (assoc (json/read-str (:text res) :key-fn keyword) :isError (:isError res)))))

(def ^:private args
  {:old-project-id "dbs" :new-project-id "topic:dbs" :tag-filter "source:e2fb"})

(deftest a-5k-tag-filter-is-walked-in-bounded-pages
  (let [db    (atom (seed 5000))
        calls (atom {})
        res   (run (->PagedStore db calls) args)]
    (testing "every entry moved, counted in the answer"
      (is (= 5000 (:migrated res)))
      (is (true? (:complete res)))
      (is (every? #(= "topic:dbs" (:project-id %)) (vals @db)))
      (is (every? #(some #{"scope:project:topic:dbs"} (:tags %)) (vals @db))
          "the destination scope tag is ensured on every moved entry"))
    (testing "no call held more than a page, and the old 10000-row scan is gone"
      (is (nil? (:query-entries @calls)))
      (is (every? #(<= (:limit %) 500) (:query-page @calls)))
      (is (every? #(= ["id" "tags"] (:output-fields %)) (:query-page @calls))))))

(deftest a-dry-run-pages-and-moves-nothing
  (let [db  (atom (seed 1200))
        res (run (->PagedStore db (atom {})) (assoc args :dry-run true))]
    (is (= 1200 (:migrated res)))
    (is (true? (:dry-run res)))
    (is (every? #(= "dbs" (:project-id %)) (vals @db)))))

(deftest the-deadline-is-checked-between-pages
  (let [store (->PagedStore (atom (seed 3)) (atom {}))
        clock (atom 0)
        res   (#'migration/migrate-in-pages!
               store [["chunk-00000"] ["chunk-00001"] ["chunk-00002"]]
               {:dry-run true :deadline-ms 15 :now-ms #(swap! clock + 10)})]
    (is (false? (:complete? res)))
    (is (= 1 (:pages res)))
    (is (= ["chunk-00000"] (:found res)))))

(deftest a-spent-budget-answers-a-partial-count-and-resumes
  (let [store (->PagedStore (atom (seed 1200)) (atom {}))
        res   (run store (assoc args :deadline-ms 0))]
    (is (not (:isError res)) "running out of time is a partial answer, not an error")
    (is (false? (:complete res)))
    (is (= 0 (:migrated res)))
    (is (re-find #"partial count" (:reason res)))
    (testing "calling again finishes the move"
      (let [res2 (run store args)]
        (is (= 1200 (:migrated res2)))
        (is (true? (:complete res2)))))))
