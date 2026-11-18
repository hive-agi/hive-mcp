(ns hive-mcp.tools.memory.crud.edit-duration-test
  "Regression for card 20261102185846-041d31e2 (MEMORY-EDIT-DURATION): an edit
   preserves the stored :duration unless the caller passes `duration`
   explicitly, a review approve lands the approved type's registry duration
   (an axiom is permanent), and an add with no duration defaults to the type's
   registry duration rather than a blanket `long`.

   The store is a stub registered through the memory store-registry port
   (proto/register-store!) inside the registry-isolation fixture. No with-redefs."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.memory.type-registry :as type-registry]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.test-support.registry-isolation :as isolation]
            [hive-mcp.tools.memory.crud.edit :as edit]
            [hive-mcp.tools.memory.review :as review]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord MapStore [entries writes]
  proto/IMemoryStore
  (connect! [_ _] {:success? true})
  (disconnect! [_] nil)
  (connected? [_] true)
  (health-check [_] {:healthy? true})
  (add-entry! [_ entry] (:id entry))
  (get-entry [_ id] (get @entries id))
  (update-entry! [_ id updates]
    (swap! writes conj [id updates])
    (get (swap! entries update id merge updates) id))
  (delete-entry! [_ _] nil)
  (query-entries [_ _] [])
  (search-similar [_ _ _] [])
  (supports-semantic-search? [_] false)
  (cleanup-expired! [_] {:count 0 :deleted-ids []})
  (entries-expiring-soon [_ _ _] [])
  (find-duplicate [_ _ _ _] nil)
  (store-status [_] {:stub true})
  (reset-store! [_] nil))

(defn- map-store
  [& entries]
  (->MapStore (atom (into {} (map (juxt :id identity)) entries)) (atom [])))

(defn- isolated-registry
  [f]
  (isolation/with-restored-store-registry
    (fn []
      (isolation/restore-stores! {})
      (f))))

(use-fixtures :each isolated-registry)

(defn- install! [store] (proto/register-store! :default store) store)

(defn- stored [store id] (get @(:entries store) id))

(defn- written-keys [store]
  (into #{} (mapcat (comp keys second)) @(:writes store)))

(defn- body [response] (json/read-str (:text response) :key-fn keyword))

(def ^:private permanent-axiom
  {:id "ax-1" :type "axiom" :content "alpha beta" :tags ["INVIOLABLE"]
   :duration "permanent" :expires ""})

(deftest default-duration-reads-the-registry
  (is (= "permanent" (type-registry/default-duration "axiom")))
  (is (= "permanent" (type-registry/default-duration :principle)))
  (is (= "long" (type-registry/default-duration "decision")))
  (is (= "short" (type-registry/default-duration "note")))
  (is (= "medium" (type-registry/default-duration "some-novel-safe-type")))
  (is (nil? (type-registry/default-duration "bad type!"))))

(deftest add-defaults-to-the-registry-duration
  (let [effective-duration @(requiring-resolve
                             'hive-mcp.tools.memory.crud.write/effective-duration)]
    (is (= "permanent" (effective-duration "axiom-candidate" nil))
        "a parked axiom nomination is permanent, so approve never inherits long")
    (is (= "permanent" (effective-duration "principle" nil)))
    (is (= "long" (effective-duration "decision" nil)))
    (is (= "short" (effective-duration "note" "  ")))
    (is (= "medium" (effective-duration "axiom" "medium"))
        "an explicit duration always wins")
    (is (= "ephemeral" (effective-duration "note" :ephemeral)))))

(deftest edits-without-duration-preserve-it
  (doseq [[label params] [["content" {:content "gamma"}]
                          ["find/replace" {:find "beta" :replace "BETA"}]
                          ["tags" {:tags ["INVIOLABLE" "catchup-priority"]}]
                          ["abstraction level" {:abstraction_level 4}]
                          ["type, gate bypassed" {:type "axiom" :tags ["x"]}]
                          ["nil duration" {:content "delta" :duration nil}]
                          ["blank duration" {:content "eps" :duration "  "}]]]
    (testing label
      (let [store (install! (map-store permanent-axiom))
            resp  (binding [type-registry/*gate-bypass?* true]
                    (edit/handle-edit (assoc params :id "ax-1")))]
        (is (not (:isError resp)) (:text resp))
        (is (seq @(:writes store)) "the edit wrote something")
        (is (not-any? #{:duration :expires} (written-keys store))
            "no duration or expires was written")
        (is (= "permanent" (:duration (stored store "ax-1"))))
        (is (= "" (:expires (stored store "ax-1"))))))))

(deftest explicit-duration-is-applied
  (let [store (install! (map-store permanent-axiom))
        resp  (edit/handle-edit {:id "ax-1" :duration "short"})]
    (is (not (:isError resp)) (:text resp))
    (is (= "short" (:duration (stored store "ax-1"))))
    (is (not (str/blank? (:expires (stored store "ax-1"))))))
  (testing "a keyword duration is accepted as its name"
    (let [store (install! (map-store (assoc permanent-axiom :duration "long" :expires "x")))]
      (edit/handle-edit {:id "ax-1" :duration :permanent})
      (is (= "permanent" (:duration (stored store "ax-1"))))
      (is (= "" (:expires (stored store "ax-1")))))))

(deftest invalid-duration-is-refused-and-nothing-is-written
  (let [store (install! (map-store permanent-axiom))
        resp  (edit/handle-edit {:id "ax-1" :content "x" :duration "forever"})]
    (is (:isError resp))
    (is (str/includes? (:text resp) "duration must be one of"))
    (is (empty? @(:writes store)))
    (is (= "permanent" (:duration (stored store "ax-1"))))))

(deftest batch-edit-preserves-duration
  (let [store (install! (map-store permanent-axiom
                                   (assoc permanent-axiom :id "ax-2")))
        resp  (body (edit/handle-batch-edit
                     {:operations [{:id "ax-1" :content "one"}
                                   {:id "ax-2" :tags ["INVIOLABLE" "t"]}]}))]
    (is (= 2 (:edited resp)) (pr-str resp))
    (is (not-any? #{:duration :expires} (written-keys store)))
    (is (= ["permanent" "permanent"]
           (mapv #(:duration (stored store %)) ["ax-1" "ax-2"])))))

(defn- parked-candidate
  "An axiom nomination as the add path parks it, carrying the `long`
   duration the old add path gave every type."
  [id]
  (let [{:keys [type gate requested]} (type-registry/resolve-write-type "axiom")]
    {:id id :type type :content "candidate"
     :tags (type-registry/queued-tags ["INVIOLABLE"] gate requested)
     :duration "long" :expires "2026-12-01T00:00:00Z"}))

(deftest review-approve-lands-the-axiom-permanent
  (let [store (install! (map-store (parked-candidate "c-1")))
        resp  (review/handle-review {:id "c-1" :verdict "approve"})
        e     (stored store "c-1")]
    (is (not (:isError resp)) (:text resp))
    (is (= "axiom" (:type e)))
    (is (= "permanent" (:duration e)))
    (is (= "" (:expires e)) "a permanent entry has no expiry")))

(deftest review-reject-preserves-the-candidate-duration
  (let [store (install! (map-store (parked-candidate "c-2")))
        resp  (review/handle-review {:id "c-2" :verdict "reject" :as "note"})
        e     (stored store "c-2")]
    (is (not (:isError resp)) (:text resp))
    (is (= "note" (:type e)))
    (is (= "long" (:duration e)))
    (is (not-any? #{:duration :expires} (written-keys store)))))
