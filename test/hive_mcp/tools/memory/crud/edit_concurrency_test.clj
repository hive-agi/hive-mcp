(ns hive-mcp.tools.memory.crud.edit-concurrency-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.memory.crud.edit :as edit]))

(defn- store-for [state before-write]
  (reify proto/IMemoryStore
    (connect! [_ _] {:success? true})
    (disconnect! [_] nil)
    (connected? [_] true)
    (health-check [_] {:healthy? true})
    (add-entry! [_ entry] (:id entry))
    (get-entry [_ id] (when (= id (:id @state)) @state))
    (update-entry! [_ id updates]
      (when before-write (before-write))
      (swap! state merge updates))
    (delete-entry! [_ _] nil)
    (query-entries [_ _] [])
    (search-similar [_ _ _] [])
    (supports-semantic-search? [_] false)
    (cleanup-expired! [_] {:cleaned 0})
    (entries-expiring-soon [_ _ _] [])
    (find-duplicate [_ _ _ _] nil)
    (store-status [_] {})
    (reset-store! [_] nil)))

(defn- with-edit-store [store f]
  (let [previous (proto/registered-stores)]
    (try
      (proto/set-store! store)
      (f)
      (finally
        (proto/reset-registry!)
        (doseq [[slot old] previous]
          (proto/register-store! slot old))))))

(defn- body [response]
  (json/read-str (:text response) :key-fn keyword))

(deftest concurrent-edits-to-one-id-keep-both-changes
  (let [state (atom {:id "edit-concurrency" :type "note" :content "first second"})
        entered (promise)
        release (promise)
        calls (atom 0)
        store (store-for state #(when (= 1 (swap! calls inc))
                                  (deliver entered true)
                                  (deref release 5000 nil)))]
    (with-edit-store store
      #(let [a (future (edit/handle-edit {:id "edit-concurrency" :find "first" :replace "one"}))]
         (try
           (is (= true (deref entered 5000 nil)))
           (let [b (future (edit/handle-edit {:id "edit-concurrency" :find "second" :replace "two"}))]
             (deliver release true)
             (is (true? (:edit_applied (body (deref a 5000 nil)))))
             (is (true? (:edit_applied (body (deref b 5000 nil)))))
             (is (= "one two" (:content @state))))
           (finally (deliver release true)))))))

(deftest stale-read-before-write-is-rejected
  (let [state (atom {:id "edit-stale" :type "note" :content "original"})
        reads (atom 0)
        store (reify proto/IMemoryStore
                (connect! [_ _] {:success? true})
                (disconnect! [_] nil)
                (connected? [_] true)
                (health-check [_] {:healthy? true})
                (add-entry! [_ entry] (:id entry))
                (get-entry [_ id]
                  (when (= id (:id @state))
                    (if (= 1 (swap! reads inc)) @state
                        (assoc @state :content "external change"))))
                (update-entry! [_ _ _] (throw (ex-info "stale write" {})))
                (delete-entry! [_ _] nil)
                (query-entries [_ _] [])
                (search-similar [_ _ _] [])
                (supports-semantic-search? [_] false)
                (cleanup-expired! [_] {:cleaned 0})
                (entries-expiring-soon [_ _ _] [])
                (find-duplicate [_ _ _ _] nil)
                (store-status [_] {})
                (reset-store! [_] nil))]
    (with-edit-store store
      #(let [response (edit/handle-edit {:id "edit-stale" :content "replacement"})]
         (is (:isError response))
         (is (.contains (:text response) "Memory edit conflict"))
         (is (= "original" (:content @state)))))))

(deftest batch-rejects-duplicate-before-writing
  (let [state (atom {:id "edit-duplicate" :type "note" :content "before"})]
    (with-edit-store (store-for state nil)
      #(let [response (edit/handle-batch-edit
                       {:operations [{:id "edit-duplicate" :content "first"}
                                     {:id "edit-duplicate" :content "second"}]})]
         (is (:isError response))
         (is (.contains (:text response) "edit-duplicate"))
         (is (= "before" (:content @state)))))))

(deftest edit-response-does-not-echo-large-entry
  (let [large (apply str (repeat 20000 "x"))
        state (atom {:id "edit-large" :type "note" :content large
                     :embed-text large})]
    (with-edit-store (store-for state nil)
      #(let [result (body (edit/handle-edit {:id "edit-large" :content "small"}))]
         (is (= "edit-large" (:id result)))
         (is (true? (:content_changed result)))
         (is (some #{"content"} (:fields_changed result)))
         (is (< (count (pr-str result)) 500))
         (is (not (contains? result :content)))
         (is (not (contains? result :embed-text)))))))