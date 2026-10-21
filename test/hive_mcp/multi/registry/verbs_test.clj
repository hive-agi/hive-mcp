(ns hive-mcp.multi.registry.verbs-test
  "Name-matched facets for `hive-mcp.multi.registry.verbs`: a trifecta over
   register! / deregister-by-owner! scripts against the live registry (state
   saved and restored around every script), checked against an independent
   model of the owned-registry policy (no overridable owner)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.multi.registry.script-support :as ss]
            [hive-mcp.multi.registry.verbs :as verbs]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private registry
  {:register! verbs/register! :deregister-by-owner! verbs/deregister-by-owner!
   :lookup verbs/lookup :snapshot verbs/snapshot
   :reset-for-test! verbs/reset-for-test! :index :by-code})

(def ^:private Entry [:map [:tool [:enum "memory" "kg"]] [:command [:enum "add" "search"]]])

(defn run-verbs-script
  "Unary adapter: a script of steps -> the registry outcome as data."
  [ops]
  (ss/run-script registry ops))

(deftrifecta verbs-registry-policy
  hive-mcp.multi.registry.verbs-test/run-verbs-script
  {:golden-path "test/golden/hive-mcp/multi/registry-verbs.edn"
   :cases {:fresh      [[:register :addon/a "x" {:tool "memory" :command "add"}]]
           :conflict   [[:register :multi/core "x" {:tool "memory" :command "add"}]
                        [:register :addon/a "x" {:tool "kg" :command "search"}]]
           :replace    [[:register :addon/a "x" {:tool "memory" :command "add"}]
                        [:register :addon/a "x" {:tool "memory" :command "search"}]]
           :deregister [[:register :addon/a "x" {:tool "memory" :command "add"}]
                        [:register "hive.rss" "y" {:tool "kg" :command "add"}]
                        [:deregister "hive.rss"]]}
   :gen (mg/generator (ss/script-schema Entry))
   :property-type :oracle
   :oracle (fn [ops] (ss/model ops nil))
   :num-tests 200
   :mutations [["core-overridable" (fn [ops] (ss/model ops :multi/core))]
               ["empty" (constantly {:steps [] :index {} :by-owner {} :lookups {}})]]})

(deftest all-codes-lists-the-live-index
  (is (= (sort (keys (get-in (verbs/snapshot) [:data :by-code])))
         (sort (verbs/all-codes)))))

(deftest a-script-leaves-the-live-registry-unchanged
  (ss/restores-the-live-registry registry [[:register :addon/a "x" {:tool "kg" :command "add"}]
                                           [:deregister :multi/core]]))
