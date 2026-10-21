(ns hive-mcp.multi.registry.tools-test
  "Name-matched facets for `hive-mcp.multi.registry.tools`: a trifecta over
   register! / deregister-by-owner! scripts against the live registry (state
   saved and restored around every script), checked against an independent
   model of the owned-registry policy. Unlike the other child registries,
   tools names :multi/core as its overridable owner: an addon replaces a
   core-seeded tool instead of conflicting with it."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.multi.registry.script-support :as ss]
            [hive-mcp.multi.registry.tools :as tools]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private registry
  {:register! tools/register! :deregister-by-owner! tools/deregister-by-owner!
   :lookup tools/lookup :snapshot tools/snapshot
   :reset-for-test! tools/reset-for-test! :index :by-name})

(def ^:private Entry
  "A tool entry; handlers are keywords (callable, and EDN for the golden)."
  [:map [:handler [:enum :echo :ping]] [:batchable {:optional true} [:enum :memory :kg]]])

(defn run-tools-script
  "Unary adapter: a script of steps -> the registry outcome as data."
  [ops]
  (ss/run-script registry ops))

(deftrifecta tools-registry-policy
  hive-mcp.multi.registry.tools-test/run-tools-script
  {:golden-path "test/golden/hive-mcp/multi/registry-tools.edn"
   :cases {:fresh          [[:register :addon/a "x" {:handler :echo}]]
           :core-overridden [[:register :multi/core "x" {:handler :echo}]
                             [:register :addon/a "x" {:handler :ping}]]
           :addon-wins-first [[:register :addon/a "x" {:handler :echo}]
                              [:register :multi/core "x" {:handler :ping}]]
           :deregister-core [[:register :multi/core "x" {:handler :echo}]
                             [:register :addon/b "y" {:handler :ping :batchable :kg}]
                             [:deregister :multi/core]]}
   :gen (mg/generator (ss/script-schema Entry))
   :property-type :oracle
   :oracle (fn [ops] (ss/model ops :multi/core))
   :num-tests 200
   :mutations [["core-not-overridable" (fn [ops] (ss/model ops nil))]
               ["empty" (constantly {:steps [] :index {} :by-owner {} :lookups {}})]]})

(deftest all-names-lists-the-live-index
  (is (= (sort (keys (get-in (tools/snapshot) [:data :by-name])))
         (sort (tools/all-names)))))

(deftest a-script-leaves-the-live-registry-unchanged
  (ss/restores-the-live-registry registry [[:register :addon/a "x" {:handler :echo}]
                                           [:deregister :multi/core]]))
