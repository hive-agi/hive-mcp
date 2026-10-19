(ns hive-mcp.channel.drain-telemetry-contract-test
  "The offer/delivery ledger under instrumentation: a fixed drain history is
   folded in, and the snapshot, summary, shelf report (against a stub lookup)
   and commitment log are pinned as a golden."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.drain-telemetry :as dt]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'dt/record! #'dt/commitment-log #'dt/commitment-summary #'dt/snapshot
    #'dt/reset! #'dt/store-lookup #'dt/shelf-report #'dt/summary})

(use-fixtures :each
  (fn [f]
    (dt/reset!)
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f)
         (finally
           (mi/unstrument! {:filters [(mi/-filter-var contracted)]})
           (dt/reset!)))))

(def ^:private history
  [{:offered-ids ["a" "b"] :delivered-ids ["c"] :seq-num 1}
   {:offered-ids ["a"] :delivered-ids ["b" "c"] :seq-num 2}
   {:offered-ids ["a"] :delivered-ids nil :seq-num 3
    :batch [{:id "c" :T "axiom" :C "never delete production data"}]}])

(def ^:private stub-lookup
  {"a" {:access-count 0}
   "b" {:access-count 1 :helpful-count 2}
   "c" {:access-count 5 :unhelpful-count 1}})

(deftest-golden ledger-after-history
  "test/golden/channel/drain-telemetry-ledger.edn"
  (do (run! dt/record! history)
      {:snapshot (into (sorted-map) (dt/snapshot))
       :summary  (dt/summary)
       :shelf    (dt/shelf-report stub-lookup)
       :drains   (count (dt/commitment-log))
       :commit   (select-keys (dt/commitment-summary) [:drains])}))

(defn- shelf-of
  "Unary subject: the shelf report of one drain history against stub-lookup."
  [hist]
  (dt/reset!)
  (run! dt/record! hist)
  (dt/shelf-report stub-lookup))

(deftrifecta shelf-ranking
  hive-mcp.channel.drain-telemetry-contract-test/shelf-of
  {:golden-path "test/golden/channel/drain-telemetry-shelf.edn"
   :cases {:empty []
           :history history
           :never-read [{:offered-ids ["a" "a" "a"] :seq-num 9}]}
   :mutations [["unsorted" (fn [h] (vec (sort-by :id (shelf-of h))))]
               ["ignores-access" (fn [h] (mapv #(assoc % :access-count 0) (shelf-of h)))]]})

(deftest summary-when-nothing-delivered
  (dt/record! {:offered-ids ["z"]})
  (is (nil? (:offer-per-delivery (dt/summary))))
  (is (= 1 (:offers (dt/summary)))))

(deftest store-lookup-without-store-is-total
  (testing "a lookup fn is always returned and never throws"
    (let [lookup (dt/store-lookup)]
      (is (ifn? lookup))
      (is (or (nil? (lookup "missing")) (map? (lookup "missing")))))))
