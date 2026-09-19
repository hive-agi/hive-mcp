(ns hive-mcp.tools.memory.crud.edit-params-test
  "memory edit param contract: unrecognised params are rejected, and
   find/replace is an exact unique-match substring swap. The store is a stub
   registered through the store-registry port."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.memory.crud.edit :as edit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord StubStore [entries writes]
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

(defn- stub-store
  [& entries]
  (->StubStore (atom (into {} (map (juxt :id identity)) entries)) (atom [])))

(defn- isolated-registry
  [f]
  (let [saved (proto/registered-stores)]
    (proto/reset-registry!)
    (try
      (f)
      (finally
        (proto/reset-registry!)
        (doseq [[k s] saved] (proto/register-store! k s))))))

(use-fixtures :each isolated-registry)

(defn- edit-with
  [store params]
  (proto/register-store! :default store)
  (edit/handle-edit params))

(defn- content-of [store id] (:content (get @(:entries store) id)))

(deftest unrecognised-params-are-named
  (is (= [] (edit/unrecognised-params {:id "m" :content "x" :command "edit"
                                       :directory "/d" :_caller_id "ling-1"})))
  (is (= ["bogus" "old_str"]
         (edit/unrecognised-params {:id "m" :old_str "a" :bogus 1 :tags []}))))

(deftest edit-rejects-unrecognised-params
  (testing "an unknown param is an error naming it, and nothing is written"
    (let [store    (stub-store {:id "m-1" :type "note" :content "hello world"})
          response (edit-with store {:id "m-1" :old_str "hello" :new_str "bye"})]
      (is (:isError response))
      (is (str/includes? (:text response) "new_str"))
      (is (str/includes? (:text response) "old_str"))
      (is (not (str/includes? (:text response) "\"noop\"")))
      (is (empty? @(:writes store)))
      (is (= "hello world" (content-of store "m-1"))))))

(deftest edit-find-replace-unique-match
  (let [store    (stub-store {:id "m-1" :type "note" :content "alpha beta gamma"})
        response (edit-with store {:id "m-1" :find "beta" :replace "BETA"})]
    (is (not (:isError response)))
    (is (str/includes? (:text response) "\"edit_applied\":true"))
    (is (= "alpha BETA gamma" (content-of store "m-1")))
    (is (= (proto/content-hash "alpha BETA gamma")
           (:content-hash (second (first @(:writes store))))))))

(deftest edit-find-replace-is-literal
  (testing "regex metacharacters and $ in find/replace are taken verbatim"
    (let [store (stub-store {:id "m-1" :type "note" :content "cost: a.b (x)"})]
      (edit-with store {:id "m-1" :find "a.b (x)" :replace "$1 \\n"})
      (is (= "cost: $1 \\n" (content-of store "m-1"))))))

(deftest edit-find-replace-empty-replace-deletes
  (let [store (stub-store {:id "m-1" :type "note" :content "keep DROP keep"})]
    (edit-with store {:id "m-1" :find " DROP" :replace ""})
    (is (= "keep keep" (content-of store "m-1")))))

(deftest edit-find-replace-errors
  (let [entry {:id "m-1" :type "note" :content "aaa one two one"}
        cases [["find absent"         {:find "zzz" :replace "y"}          "not found"]
               ["find ambiguous"      {:find "one" :replace "y"}          "more than once"]
               ["find overlapping"    {:find "aa" :replace "y"}           "more than once"]
               ["find without replace" {:find "two"}                      "together"]
               ["replace without find" {:replace "y"}                     "together"]
               ["blank find"          {:find "" :replace "y"}             "non-blank"]
               ["content plus find"   {:find "two" :replace "y" :content "z"} "mutually exclusive"]]]
    (doseq [[label params expected] cases]
      (testing label
        (let [store    (stub-store entry)
              response (edit-with store (assoc params :id "m-1"))]
          (is (:isError response))
          (is (str/includes? (:text response) expected))
          (is (empty? @(:writes store)))
          (is (= (:content entry) (content-of store "m-1"))))))))

(deftest batch-edit-flags-unrecognised-params-per-op
  (let [store (stub-store {:id "m-1" :type "note" :content "one"}
                          {:id "m-2" :type "note" :content "two"})
        _     (proto/register-store! :default store)
        text  (:text (edit/handle-batch-edit
                      {:operations [{:id "m-1" :bogus true}
                                    {:id "m-2" :find "two" :replace "TWO"}]}))]
    (is (str/includes? text "\"errors\":1"))
    (is (str/includes? text "\"edited\":1"))
    (is (str/includes? text "bogus"))
    (is (= "one" (content-of store "m-1")))
    (is (= "TWO" (content-of store "m-2")))))
