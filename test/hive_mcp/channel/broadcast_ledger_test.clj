(ns hive-mcp.channel.broadcast-ledger-test
  "Trifectas for the broadcast volume ledger
   (`hive-mcp.channel.broadcast-ledger`), driven by its contracts.

   Split out of the former `hive-mcp.channel.broadcast-trifecta-test` so the
   test namespace is named after its source namespace; the forms are
   unchanged."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.channel.broadcast-ledger :as ledger]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (ledger/reset-ledger!) (try (f) (finally (ledger/reset-ledger!)))))

(def ^:private window 1000)

(hst/deftrifecta-from-schema live-contract
  hive-mcp.channel.broadcast-ledger/live
  {:in        [:cat [:vector {:max 6} [:int {:min 0 :max 3000}]] [:enum 2000 3000] [:= 1000]]
   :out       [:vector :int]
   ;; strictly inside the window: an entry exactly a window old has aged out
   :rel       (fn [[stamps now w] out] (= out (filterv #(> % (- now w)) stamps)))
   :num-tests 200
   :golden-path "test/golden/channel/broadcast-ledger-live.edn"})

(def ^:private LedgerIn
  [:map-of [:enum "hive" "global" "other"] [:vector {:max 4} [:int {:min 0 :max 3000}]]])

(hst/deftrifecta-from-schema prune-contract
  hive-mcp.channel.broadcast-ledger/prune
  {:in        [:cat [:maybe LedgerIn] [:enum 2000 3000] [:= 1000]]
   :out       ledger/Ledger
   ;; every survivor is live, and no project is left with nothing
   :rel       (fn [[l now w] out]
                (and (every? (fn [[_ ts]] (and (seq ts) (every? #(> % (- now w)) ts))) out)
                     (every? (fn [[k ts]] (= (seq (filterv #(> % (- now w)) ts)) (seq (get out k))))
                             l)))
   :num-tests 200})

(hst/deftrifecta-from-schema spent-contract
  hive-mcp.channel.broadcast-ledger/spent
  {:in        [:cat [:maybe LedgerIn] [:enum "hive" "" nil "global"] [:enum 2000] [:= 1000]]
   :out       [:int {:min 0}]
   ;; a blank or missing project is metered under "global"
   :rel       (fn [[l p now w] out]
                (= out (count (filter #(> % (- now w))
                                      (get l (if (or (nil? p) (= "" p)) "global" p))))))
   :num-tests 200})

(hst/deftrifecta-from-schema spend-contract
  hive-mcp.channel.broadcast-ledger/spend
  {:in        [:cat [:maybe LedgerIn] [:enum "hive" nil] [:enum 2000] [:= 1000]]
   :out       ledger/Ledger
   :rel       (fn [[l p now w] out]
                (let [k (if (nil? p) "global" p)]
                  (= (inc (ledger/spent l p now w)) (ledger/spent out p now w) (count (get out k)))))
   :num-tests 200})

(deftest held-ledger-contract-test
  (is (pos-int? (ledger/window-ms)))
  (is (= 0 (ledger/spent-recently "zz-proj")))
  (is (nil? (ledger/record-broadcast! "zz-proj")))
  (is (nil? (ledger/record-broadcast! "zz-proj" (System/currentTimeMillis))))
  (is (= 2 (ledger/spent-recently "zz-proj")))
  (is (= {"zz-proj" 2} (ledger/snapshot)))
  (is (= {} (ledger/snapshot (+ (System/currentTimeMillis) (* 2 (ledger/window-ms))))))
  (is (nil? (ledger/reset-ledger!)))
  (is (= {} (ledger/snapshot))))

(deftest ledger-schema-generates-test
  (is (seq (mg/sample ledger/Ledger {:size 3})) (pr-str (m/form ledger/Ledger))))
