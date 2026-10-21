(ns hive-mcp.tools.catchup.hierarchy-contract-test
  "catchup.hierarchy: the chunk arithmetic as a trifecta, the fan-out over the
   IMemoryStore PORT (seeded StubMemoryStore)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.catchup.contract-fixtures :as fx]
            [hive-mcp.tools.catchup.hierarchy :as hier]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.mutation :refer [deftest-mutations]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fx/with-seeded-store)

(deftrifecta-from-schema chunk-count-ceiling
  hive-mcp.tools.catchup.hierarchy/chunk-count
  {:in              [:cat [:vector {:gen/max 50} [:enum "a" "b"]]]
   :out             pos-int?
   :rel             (fn [[ids] out] (= out (max 1 (long (Math/ceil (/ (count ids) 15.0))))))
   :classify        (fn [[ids] _] (if (> (count ids) 15) :many :one))
   :classify-domain #{:many :one}
   :classify-floor  10
   :golden-path     "test/golden/catchup/hierarchy-chunk-count.edn"
   :num-tests       200})

(deftest-mutations chunk-count-partition
  hive-mcp.tools.catchup.hierarchy/chunk-count
  [["off by one" (fn [ids] (inc (count (partition-all 15 ids))))]
   ["no floor"   (fn [ids] (count (partition-all 15 ids)))]
   ["constant 1" (constantly 1)]]
  (fn []
    (is (= 1 (hier/chunk-count [])))
    (is (= 1 (hier/chunk-count (range 15))))
    (is (= 3 (hier/chunk-count (range 31))))))

(deftest hierarchy-fan-out
  (is (= ["e2" "e3" "e4" "e5"] (sort (map :id (hier/chunked-hierarchy-fetch (mem-proto/get-store) ["p"] 10))))
      "metadata rows for exactly the requested project")
  (is (= [] (hier/chunked-hierarchy-fetch (mem-proto/get-store) [] 10)))
  (is (nil? (hier/compute-hierarchy-project-ids "global")))
  (is (nil? (hier/compute-hierarchy-project-ids nil))))
