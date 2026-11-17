(ns hive-mcp.tools.memory.reconcile-store-scan-test
  "`memory reconcile` reads the active store's own id scan when no sources
   are installed (card 20260723023019-14943893: the verb answered
   not-wired on the live host because nothing called set-sources!).
   Port stubs only: a reify of IMemoryStoreScan stands in for Milvus."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.memory.reconcile :as core]
            [hive-mcp.memory.reconcile-sources :as rsrc]
            [hive-mcp.tools.memory.reconcile :as rec]
            [hive-spi.memory.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [t] (rec/clear-sources!) (try (t) (finally (rec/clear-sources!)))))

(defn- scanning-store
  "A store stub whose scan answers IDS and counts its calls in CALLS."
  [ids calls]
  (reify ports/IMemoryStoreScan
    (scan-ids [_ _opts] (swap! calls inc) ids)))

(defn- answer [resp] (json/read-str (:text resp) :key-fn keyword))

(deftest scan-sources-read-one-listing-once
  (let [calls (atom 0)
        s     (rsrc/scan-sources #(do (swap! calls inc) ["a" nil "b" "a" " "]))
        r     (core/report s)]
    (is (= 1 @calls) "store and vector sides share one scan")
    (is (= [] (:orphan-vector-ids r)))
    (is (= [] (:unindexed-store-ids r)))
    (is (= 2 (:nil-vector-ids r)) "nil and blank ids stay visible")
    (is (= 2 (:store-count r)))
    (is (false? (:consistent? r)))))

(deftest store-sources-needs-a-scan
  (is (nil? (rec/store-sources nil)))
  (is (nil? (rec/store-sources (Object.))) "a store without IMemoryStoreScan is not wired")
  (is (satisfies? core/IReconcileSources (rec/store-sources (scanning-store [] (atom 0))))))

(deftest handler-falls-back-to-the-active-store-scan
  (let [calls (atom 0)]
    (with-redefs-fn {#'rec/active-store (constantly (scanning-store ["x" "y" nil] calls))}
      (fn []
        (let [out (answer (rec/handle-reconcile {}))]
          (is (= "ok" (:status out)))
          (is (= 2 (:store-count out)))
          (is (= 1 (:nil-vector-ids out)))
          (is (= 1 @calls)))))))

(deftest installed-sources-win-over-the-store
  (let [calls (atom 0)]
    (rec/set-sources! (reify core/IReconcileSources
                        (store-ids [_] ["a"])
                        (vector-ids [_] ["a" "ghost"])))
    (with-redefs-fn {#'rec/active-store (constantly (scanning-store ["zzz"] calls))}
      (fn []
        (let [out (answer (rec/handle-reconcile {}))]
          (is (= ["ghost"] (:orphan-vector-ids out)))
          (is (zero? @calls) "the store scan is not read"))))))

(deftest no-store-and-no-sources-is-not-wired
  (with-redefs-fn {#'rec/active-store (constantly nil)}
    (fn [] (is (= "not-wired" (:status (answer (rec/handle-reconcile {}))))))))

(deftest a-failing-scan-is-a-failed-reconcile-not-a-clean-report
  (with-redefs-fn {#'rec/active-store
                   (constantly (reify ports/IMemoryStoreScan
                                 (scan-ids [_ _] (throw (ex-info "milvus down" {})))))}
    (fn []
      (let [resp (rec/handle-reconcile {})]
        (testing "an outage never reads as consistent"
          (is (:isError resp))
          (is (not (re-find #"consistent" (str (:text resp))))))))))
