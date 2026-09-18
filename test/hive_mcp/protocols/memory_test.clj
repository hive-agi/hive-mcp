(ns hive-mcp.protocols.memory-test
  "TDD tests for multi-store registry in hive-mcp.protocols.memory."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.protocols.memory :as proto]
            [hive-spi.memory.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord StubStore [id]
  proto/IMemoryStore
  (connect! [_ _] nil)
  (disconnect! [_] nil)
  (connected? [_] true)
  (health-check [_] {:healthy? true})
  (add-entry! [_ e] e)
  (get-entry [_ _] nil)
  (update-entry! [_ _ _] nil)
  (delete-entry! [_ _] nil)
  (query-entries [_ _] [])
  (search-similar [_ _ _] [])
  (supports-semantic-search? [_] false)
  (cleanup-expired! [_] {:count 0 :deleted-ids []})
  (entries-expiring-soon [_ _ _] [])
  (find-duplicate [_ _ _ _] nil)
  (store-status [_] {:stub true})
  (reset-store! [_] true))

(defn- reset-registry-fixture [f]
  (proto/reset-registry!)
  (try (f) (finally (proto/reset-registry!))))

(use-fixtures :each reset-registry-fixture)

(deftest registry-isolation-test
  (testing "two stores under different keys retrievable independently"
    (let [a (->StubStore :a)
          b (->StubStore :b)]
      (proto/register-store! :a a)
      (proto/register-store! :b b)
      (is (= :a (:id (proto/get-store :a))))
      (is (= :b (:id (proto/get-store :b)))))))

(deftest default-arity-test
  (testing "(get-store) returns :default entry"
    (let [s (->StubStore :d)]
      (proto/register-store! :default s)
      (is (= :d (:id (proto/get-store)))))))

(deftest missing-default-throws-test
  (testing "(get-store) with empty registry throws ex-info"
    (is (thrown? clojure.lang.ExceptionInfo (proto/get-store)))
    (try (proto/get-store)
         (catch clojure.lang.ExceptionInfo e
           (is (contains? (ex-data e) :registry-keys))))))

(deftest missing-key-throws-test
  (testing "(get-store :nonexistent) throws with key + available in ex-data"
    (proto/register-store! :a (->StubStore :a))
    (is (thrown? clojure.lang.ExceptionInfo (proto/get-store :nonexistent)))
    (try (proto/get-store :nonexistent)
         (catch clojure.lang.ExceptionInfo e
           (let [d (ex-data e)]
             (is (= :nonexistent (:store-key d)))
             (is (some #{:a} (:available d))))))))

(deftest unregister-isolation-test
  (testing "unregister-store! removes only targeted key"
    (proto/register-store! :a (->StubStore :a))
    (proto/register-store! :b (->StubStore :b))
    (proto/unregister-store! :a)
    (is (thrown? clojure.lang.ExceptionInfo (proto/get-store :a)))
    (is (= :b (:id (proto/get-store :b))))))

(deftest set-store-backward-compat-test
  (testing "set-store! routes to :default for legacy callers"
    (let [s (->StubStore :legacy)]
      (proto/set-store! s)
      (is (= :legacy (:id (proto/get-store))))
      (is (= :legacy (:id (proto/get-store :default)))))))

(defrecord LateStore [])

(deftest re-exports-dispatch-to-impls-extended-after-this-ns-loaded
  (testing "a map-based `extend` registered after hive-mcp.protocols.memory loaded is reachable through every re-exported method"
    (let [late (fn [& _] ::late)
          all (fn [ks] (zipmap ks (repeat late)))]
      (extend LateStore
        ports/IMemoryStore
        (all [:connect! :disconnect! :connected? :health-check :add-entry! :get-entry
              :update-entry! :delete-entry! :query-entries :search-similar
              :supports-semantic-search? :cleanup-expired! :entries-expiring-soon
              :find-duplicate :store-status :reset-store!])
        ports/IMemoryStoreWithAnalytics (all [:log-access! :record-feedback! :get-helpfulness-ratio])
        ports/IMemoryStoreBatch (all [:get-entries])
        ports/IMemoryStoreMetadataWrite (all [:update-metadata!])
        ports/IMemoryStoreWithStaleness (all [:update-staleness! :get-stale-entries :propagate-staleness!])
        ports/IMemoryStoreWithRouting (all [:target-collection-for :relocate-entry!])
        ports/IMemoryStoreTemporal (all [:asof-entry :history-entry :asof-query :between-query]))
      (let [s (->LateStore)]
        (doseq [[method f args] [["connect!" proto/connect! [s {}]]
                                 ["disconnect!" proto/disconnect! [s]]
                                 ["connected?" proto/connected? [s]]
                                 ["health-check" proto/health-check [s]]
                                 ["add-entry!" proto/add-entry! [s {}]]
                                 ["get-entry" proto/get-entry [s "id"]]
                                 ["update-entry!" proto/update-entry! [s "id" {}]]
                                 ["delete-entry!" proto/delete-entry! [s "id"]]
                                 ["query-entries" proto/query-entries [s {}]]
                                 ["search-similar" proto/search-similar [s "q" {}]]
                                 ["supports-semantic-search?" proto/supports-semantic-search? [s]]
                                 ["cleanup-expired!" proto/cleanup-expired! [s]]
                                 ["entries-expiring-soon" proto/entries-expiring-soon [s 7 {}]]
                                 ["find-duplicate" proto/find-duplicate [s "note" "hash" {}]]
                                 ["store-status" proto/store-status [s]]
                                 ["reset-store!" proto/reset-store! [s]]
                                 ["log-access!" proto/log-access! [s "id"]]
                                 ["record-feedback!" proto/record-feedback! [s "id" :helpful]]
                                 ["get-helpfulness-ratio" proto/get-helpfulness-ratio [s "id"]]
                                 ["get-entries" proto/get-entries [s ["id"]]]
                                 ["update-metadata!" proto/update-metadata! [s "id" {}]]
                                 ["update-staleness!" proto/update-staleness! [s "id" {}]]
                                 ["get-stale-entries" proto/get-stale-entries [s 0.5 {}]]
                                 ["propagate-staleness!" proto/propagate-staleness! [s "id" 1]]
                                 ["target-collection-for" proto/target-collection-for [s {}]]
                                 ["relocate-entry!" proto/relocate-entry! [s "id"]]
                                 ["asof-entry" proto/asof-entry [s "id" "t"]]
                                 ["history-entry" proto/history-entry [s "id"]]
                                 ["asof-query" proto/asof-query [s {} "t"]]
                                 ["between-query" proto/between-query [s {} "t1" "t2"]]]]
          (is (= ::late (apply f args)) method))))))
