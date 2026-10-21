(ns hive-mcp.multi.registry.aliases-test
  "Name-matched facets for `hive-mcp.multi.registry.aliases`: a trifecta over
   register! / deregister-by-owner! scripts against the live registry (state
   saved and restored around every script), checked against an independent
   model of the owned-registry policy, plus the read surface."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.multi.registry.aliases :as aliases]
            [hive-mcp.multi.registry.script-support :as ss]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private registry
  {:register! aliases/register! :deregister-by-owner! aliases/deregister-by-owner!
   :lookup aliases/lookup :snapshot aliases/snapshot
   :reset-for-test! aliases/reset-for-test! :index :by-short})

(def ^:private Entry [:map [:full [:enum :memory/content :kg/from :kg/to]]])

(defn run-aliases-script
  "Unary adapter: a script of steps -> the registry outcome as data."
  [ops]
  (ss/run-script registry ops))

(deftrifecta aliases-registry-policy
  hive-mcp.multi.registry.aliases-test/run-aliases-script
  {:golden-path "test/golden/hive-mcp/multi/registry-aliases.edn"
   :cases {:fresh       [[:register :addon/a "x" {:full :memory/content}]]
           :replace     [[:register :addon/a "x" {:full :memory/content}]
                         [:register :addon/a "x" {:full :kg/from}]]
           :conflict    [[:register :addon/a "x" {:full :memory/content}]
                         [:register :addon/b "x" {:full :kg/to}]]
           :core-is-not-overridable [[:register :multi/core "x" {:full :memory/content}]
                                     [:register :addon/a "x" {:full :kg/to}]]
           :deregister  [[:register :addon/a "x" {:full :memory/content}]
                         [:register :addon/b "y" {:full :kg/to}]
                         [:deregister :addon/a]]}
   :gen (mg/generator (ss/script-schema Entry))
   :property-type :oracle
   ;; aliases has no overridable owner: :multi/core entries conflict too
   :oracle (fn [ops] (ss/model ops nil))
   :num-tests 200
   :mutations [["core-overridable" (fn [ops] (ss/model ops :multi/core))]
               ["empty" (constantly {:steps [] :index {} :by-owner {} :lookups {}})]]})

(deftest aliases-agree-with-the-model
  (doseq [ops (mg/sample (ss/script-schema Entry) {:size 10 :seed 3})]
    (is (= (ss/model ops nil) (run-aliases-script ops)) (pr-str ops))))

(deftest all-aliases-reads-the-index
  (let [r (run-aliases-script [[:register :addon/a "x" {:full :memory/content}]])]
    (is (= {"x" {:full :memory/content :owner :addon/a}} (:index r))))
  (is (= (update-vals (get-in (aliases/snapshot) [:data :by-short]) :full)
         (aliases/all-aliases))))

(deftest a-script-leaves-the-live-registry-unchanged
  (ss/restores-the-live-registry registry [[:register :addon/a "x" {:full :kg/to}]
                                           [:deregister :multi/core]]))
