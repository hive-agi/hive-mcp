(ns hive-mcp.tools.memory.crud.edit-readback-test
  "A memory edit's reported result must match what the store holds.

   Under contention the milvus store answers `{:error ..}` (RPC deref or
   embedding-gate timeout) for updates whose upsert can still land. The async
   `memory review` path then answered \"Memory store update failed\" for writes
   that were saved (card 20261026130012-4368e3e8). These tests run several
   reviews in parallel against a store that lands-but-errors, errors-then-lands
   on retry, or never lands, and check each answer is truthful."
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.memory.type-registry :as type-registry]
            [hive-mcp.tools.memory.review :as review]
            [hive-mcp.tools.memory.crud.edit :as edit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- parked-entry
  "An entry as the write gate parks a requested axiom."
  [id]
  (let [{:keys [type gate requested]} (type-registry/resolve-write-type "axiom")]
    {:id id
     :type type
     :content (str "hypothesis " id)
     :tags (type-registry/queued-tags ["t"] gate requested)}))

(defn- behaviour
  "How the fake store treats the Nth update call on `id`:
     :ok          lands and answers the entry
     :land-error  lands, but answers a failure map (timed-out RPC)
     :drop-error  does not land, answers a failure map
     :throw-land  lands, then the call throws"
  [id n]
  (case (subs id 0 1)
    "a" :ok
    "b" :land-error
    "c" (if (= 1 n) :drop-error :ok)          ; recovers on the retry
    "d" :drop-error                           ; never lands
    "e" :throw-land))

(defn- with-fake-store
  [ids f]
  (let [db    (atom (into {} (map (juxt identity parked-entry)) ids))
        calls (atom {})
        store (reify Object (toString [_] "fake-store"))]
    (with-redefs [mem-proto/store-set?       (constantly true)
                  mem-proto/get-store        (constantly store)
                  mem-proto/registered-stores (constantly {:default store})
                  mem-proto/get-entry        (fn [_ id] (get @db id))
                  mem-proto/update-entry!
                  (fn [_ id updates]
                    (Thread/sleep (long (rand-int 15)))
                    (let [n (get (swap! calls update id (fnil inc 0)) id)
                          land! #(get (swap! db update id merge updates) id)]
                      (case (behaviour id n)
                        :ok         (land!)
                        :land-error (do (land!) {:error :boundary/milvus-write-failed
                                                 :id id :reconnecting? true})
                        :drop-error {:error :embedder/gate-timeout :id id}
                        :throw-land (do (land!) (throw (ex-info "milvus add timed out after 5000ms"
                                                                {:milvus/timeout true}))))))]
      (f db calls))))

(defn- saved? [db id]
  (= "axiom" (:type (get @db id))))

(deftest parallel-reviews-report-what-the-store-holds
  (let [ids (vec (for [p ["a" "b" "c" "d" "e"] i (range 3)] (str p i)))]
    (with-fake-store ids
      (fn [db calls]
        (let [results (->> ids
                           (mapv (fn [id] [id (future (review/handle-review
                                                       {:id id :verdict "approve"}))]))
                           (mapv (fn [[id fut]] [id (deref fut 10000 ::timeout)])))]
          (testing "every review answered"
            (is (not-any? #(= ::timeout (second %)) results)))
          (doseq [[id resp] results]
            (testing (str "review of " id " is truthful")
              (is (= (saved? db id) (not (:isError resp)))
                  (pr-str id resp (get @db id)))))
          (testing "landed writes answer success, not 'update failed'"
            (doseq [[id resp] results
                    :when (#{"a" "b" "c" "e"} (subs id 0 1))]
              (is (saved? db id) id)
              (is (true? (:edit_applied (json/parse-string (:text resp) true))) id)))
          (testing "a write that never lands is reported failed, with the store's reason"
            (doseq [[id resp] results :when (= "d" (subs id 0 1))]
              (is (not (saved? db id)))
              (is (:isError resp))
              (is (re-find #"Memory store update failed" (str (:text resp))))
              (is (re-find #"gate-timeout" (str (:text resp))))))
          (testing "a lost write is retried exactly once"
            (doseq [id ids :when (#{"c" "d"} (subs id 0 1))]
              (is (= 2 (get @calls id)) id)))
          (testing "a landed write is never re-sent"
            (doseq [id ids :when (#{"a" "b" "e"} (subs id 0 1))]
              (is (= 1 (get @calls id)) id))))))))

(deftest edit-path-shares-the-readback-verdict
  (with-fake-store ["b9" "d9"]
    (fn [db _]
      (let [ok   (edit/handle-edit {:id "b9" :tags ["x"]})
            fail (edit/handle-edit {:id "d9" :tags ["x"]})]
        (is (not (:isError ok)))
        (is (= ["x"] (:tags (get @db "b9"))))
        (is (:isError fail))
        (is (not= ["x"] (:tags (get @db "d9"))))))))
