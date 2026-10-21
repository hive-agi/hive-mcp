(ns hive-mcp.multi.registry.batchables-test
  "Name-matched facets for `hive-mcp.multi.registry.batchables`: a trifecta
   over register! / deregister-by-owner! scripts against the live registry
   (state saved and restored around every script), checked against an
   independent model of the owned-registry policy (no overridable owner).
   The registry stores the record opaquely, so keywords stand in for
   Batchable records and keep the golden EDN."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.multi.registry.batchables :as batchables]
            [hive-mcp.multi.registry.script-support :as ss]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private registry
  {:register! batchables/register! :deregister-by-owner! batchables/deregister-by-owner!
   :lookup batchables/lookup :snapshot batchables/snapshot
   :reset-for-test! batchables/reset-for-test! :index :by-name})

(def ^:private Entry [:map [:record [:enum :memory-batchable :kg-batchable]]])

(defn run-batchables-script
  "Unary adapter: a script of steps -> the registry outcome as data."
  [ops]
  (ss/run-script registry ops))

(deftrifecta batchables-registry-policy
  hive-mcp.multi.registry.batchables-test/run-batchables-script
  {:golden-path "test/golden/hive-mcp/multi/registry-batchables.edn"
   :cases {:fresh      [[:register :addon/a "x" {:record :memory-batchable}]]
           :conflict   [[:register :multi/core "x" {:record :memory-batchable}]
                        [:register :addon/a "x" {:record :kg-batchable}]]
           :deregister [[:register :addon/a "x" {:record :memory-batchable}]
                        [:register :addon/b "y" {:record :kg-batchable}]
                        [:deregister :addon/b]]}
   :gen (mg/generator (ss/script-schema Entry))
   :property-type :oracle
   :oracle (fn [ops] (ss/model ops nil))
   :num-tests 200
   :mutations [["core-overridable" (fn [ops] (ss/model ops :multi/core))]
               ["empty" (constantly {:steps [] :index {} :by-owner {} :lookups {}})]]})

(deftest lookup-reads-the-live-index
  (doseq [[id entry] (get-in (batchables/snapshot) [:data :by-name])]
    (is (= entry (batchables/lookup id)))))

(deftest a-script-leaves-the-live-registry-unchanged
  (ss/restores-the-live-registry registry [[:register :addon/a "x" {:record :kg-batchable}]
                                           [:deregister :multi/core]]))
