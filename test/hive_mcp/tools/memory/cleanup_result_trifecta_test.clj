(ns hive-mcp.tools.memory.cleanup-result-trifecta-test
  "Golden + property + mutation pinning for normalize-cleanup-result.

   The store port leaves cleanup-expired!'s return shape open: Chroma and the
   stub answer {:count :deleted-ids :repaired}, hive-milvus answers a bare
   integer. The handler must report the deleted count for both. Incident
   20260728114115-04cbde89 saw {\"deleted\": null} because the integer was
   destructured as a map.

   Mutants are self-contained and never call the subject var."
  (:require [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.memory.lifecycle :as lifecycle]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gen-store-result
  (gen/one-of [gen/nat
               (gen/return nil)
               (gen/let [ids (gen/vector gen/string-alphanumeric 0 5)
                         repaired gen/nat]
                 {:count (count ids) :deleted-ids ids :repaired repaired})
               (gen/let [ids (gen/vector gen/string-alphanumeric 0 5)]
                 {:deleted-ids ids})]))

(defn- shape-ok? [r]
  (and (map? r)
       (nat-int? (:count r))
       (vector? (:deleted-ids r))
       (nat-int? (:repaired r))))

(deftrifecta normalize-cleanup-result-contract
  hive-mcp.tools.memory.lifecycle/normalize-cleanup-result
  {:golden-path "test/golden/tools/memory/normalize-cleanup-result.edn"
   :cases       {:milvus-integer  7
                 :milvus-zero     0
                 :chroma-map      {:count 2 :deleted-ids ["a" "b"] :repaired 1}
                 :map-without-count {:deleted-ids ["a" "b" "c"]}
                 :failed-nil      nil}
   :gen         gen-store-result
   :pred        shape-ok?
   :num-tests   200
   :mutations   [["destructure-as-map — the original bug, integer reads as nil count"
                  (fn [r] {:count (:count r) :deleted-ids (vec (:deleted-ids r))
                           :repaired (or (:repaired r) 0)})]
                 ["always-zero — drops the store's count"
                  (constantly {:count 0 :deleted-ids [] :repaired 0})]]
   :assert      (fn []
                  (is (= 7 (:count (lifecycle/normalize-cleanup-result 7)))
                      "a bare Milvus integer is the deleted count")
                  (is (= ["a" "b"]
                         (:deleted-ids (lifecycle/normalize-cleanup-result
                                        {:count 2 :deleted-ids ["a" "b"]})))
                      "a map keeps its ids for KG edge cleanup")
                  (is (= 0 (:count (lifecycle/normalize-cleanup-result nil)))
                      "a failed call reads as zero"))})
