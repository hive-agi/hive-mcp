(ns hive-mcp.tools.memory.plan-index-test
  "A stored plan must land in the dedicated `hive-mcp-plans` index.

   The main store stays authoritative; the plans collection is derived state
   that `plan search`/`plan query` read. These tests pin the two write hooks
   (add, edit) that keep the index in step, and pin that the index can never
   take the write down with it: a plan whose embedding provider is down is
   still stored, still edited, and the failure is only logged.

   Nothing here touches Milvus or an embedding provider — the memory store is
   the port stub and `plans/index-plan!` is redefined to record its argument."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.plan.plans :as plans]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.memory.crud.edit :as edit]
            [hive-mcp.tools.memory.crud.write :as write]
            [hive-spi.memory.ports :as ports]
            [hive-test.isolation :as iso]
            [hive-mcp.isolation-methods]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; The port stub and a fresh ephemeral KG store, both restored after each test.
(use-fixtures :each mem-stub/with-stub-store (iso/with-isolations :kg-conn))

;; A schema-valid EDN plan: the write gate parses and checks dependencies, so
;; the fixtures cannot be arbitrary prose.
(def ^:private plan-content
  (pr-str {:id "plan-index"
           :title "Index hook plan"
           :steps [{:id "s1" :title "Do X"}
                   {:id "s2" :title "Do Y" :depends-on ["s1"]}]}))

(defn- capture-index!
  "Redefine `plans/index-plan!` for the body of F to record each call in CALLS
   and answer a fixed id. `THROWING` redefines it to throw instead."
  [calls throwing f]
  (with-redefs [plans/index-plan!
                (if throwing
                  (fn [_] (throw (ex-info "embedding provider down" {:type :no-provider})))
                  (fn [entry] (swap! calls conj entry) (:id entry)))]
    (f)))

(defn- add-plan!
  [& [content]]
  (write/handle-add {:type "plan"
                     :content (or content plan-content)
                     :tags ["t"]
                     :duration "long"
                     :directory "/tmp/test"}))

(defn- seed-plan!
  [id]
  (ports/add-entry! (mem-proto/get-store)
                    {:id id :type "plan" :content plan-content
                     :tags ["t"] :project-id "hive-mcp"
                     :duration "long" :plan-status "draft"}))

;; ============================================================
;; add
;; ============================================================

(deftest add-indexes-a-stored-plan-under-its-own-id
  (testing "the created entry id, not a fresh one, keys the plans index"
    (let [calls (atom [])
          _     (capture-index! calls false #(add-plan!))
          entry (first @calls)]
      (is (= 1 (count @calls)) "one plan write, one index call")
      (is (string? (:id entry)))
      (is (= "plan" (:type entry)))
      (is (= "draft" (:plan-status entry))
          "a new plan starts in draft, whatever the content says")
      (is (= "test" (:project-id entry)) "/tmp/test derives the project")
      (is (contains? entry :content-hash))
      (is (contains? entry :expires)))))

(deftest add-indexes-under-the-store-assigned-id
  (testing "the indexed id is the id the store assigned and answered with"
    (let [calls (atom [])
          resp  (capture-index! calls false #(add-plan!))
          id    (:id (json/read-str (:text resp) :key-fn keyword))]
      (is (string? id))
      (is (= id (:id (first @calls)))))))

(deftest add-counts-steps-of-a-list-shaped-plan
  (testing "a numbered plan indexes its step count, so the index can rank by size"
    (let [calls (atom [])]
      (capture-index! calls false
                      #(add-plan! "# Plan\n\n## Step 1\n\nfirst\n\n## Step 2\n\nsecond\n"))
      (is (= 2 (:steps-count (first @calls)))))))

(deftest add-indexes-a-steps-count-only-when-there-are-steps
  (testing "an EDN plan has no line-shaped steps; the count is still numeric"
    (let [calls (atom [])]
      (capture-index! calls false #(add-plan!))
      (is (= 0 (:steps-count (first @calls)))))))

(deftest add-does-not-index-a-non-plan
  (testing "notes stay out of the plans collection"
    (let [calls (atom [])
          resp  (capture-index! calls false
                                #(write/handle-add {:type "note" :content "some note"
                                                    :tags [] :directory "/tmp/test"}))]
      (is (not (:isError resp)))
      (is (empty? @calls)))))

(deftest add-keeps-the-entry-when-indexing-fails
  (testing "index failure is logged and swallowed; the write still answers"
    (let [calls (atom [])
          resp  (capture-index! calls true #(add-plan!))]
      (is (not (:isError resp)) (str "add must survive index failure: " (:text resp)))
      (is (string? (:id (json/read-str (:text resp) :key-fn keyword)))
          "the caller still gets the receipt of the stored entry"))))

;; ============================================================
;; edit
;; ============================================================

(deftest edit-reindexes-an-edited-plan
  (testing "a content edit re-indexes by the same id (upsert, not a new row)"
    (seed-plan! "p-1")
    (let [calls (atom [])
          resp  (capture-index! calls false
                                #(edit/handle-edit {:id "p-1" :content "1. only step"}))]
      (is (not (:isError resp)))
      (is (= 1 (count @calls)))
      (is (= "p-1" (:id (first @calls))))
      (is (= "1. only step" (:content (first @calls)))
          "the index receives the edited content, not the stale one"))))

(deftest edit-reindexes-a-metadata-only-plan-edit
  (testing "tags live in indexed metadata too, so a tag edit re-indexes"
    (seed-plan! "p-2")
    (let [calls (atom [])]
      (capture-index! calls false
                      #(edit/handle-edit {:id "p-2" :tags ["t" "auth"]}))
      (is (= 1 (count @calls)))
      (is (= ["t" "auth"] (:tags (first @calls)))))))

(deftest edit-preserves-plan-status-when-reindexing
  (testing "an edit may not reset an active plan to draft"
    (ports/add-entry! (mem-proto/get-store)
                      {:id "p-3" :type "plan" :content plan-content
                       :tags ["t"] :plan-status "active"})
    (let [calls (atom [])]
      (capture-index! calls false
                      #(edit/handle-edit {:id "p-3" :content "1. changed"}))
      (is (= "active" (:plan-status (first @calls)))))))

(deftest edit-does-not-reindex-a-non-plan
  (testing "editing a note leaves the plans collection alone"
    (ports/add-entry! (mem-proto/get-store)
                      {:id "n-1" :type "note" :content "before" :tags []})
    (let [calls (atom [])
          resp  (capture-index! calls false
                                #(edit/handle-edit {:id "n-1" :content "after"}))]
      (is (not (:isError resp)))
      (is (empty? @calls)))))

(deftest edit-keeps-the-edit-when-reindexing-fails
  (testing "index failure does not turn a landed edit into an error"
    (seed-plan! "p-4")
    (let [calls (atom [])
          resp  (capture-index! calls true
                                #(edit/handle-edit {:id "p-4" :content "1. new"}))]
      (is (not (:isError resp)) (str "edit must survive index failure: " (:text resp)))
      (is (true? (:edit_applied (json/read-str (:text resp) :key-fn keyword)))))))
