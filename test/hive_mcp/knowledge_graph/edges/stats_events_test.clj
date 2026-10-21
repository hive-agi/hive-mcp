(ns hive-mcp.knowledge-graph.edges.stats-events-test
  "The kg.edges/* event -> edge-stats wiring, driven through the event bus
   (the port this namespace subscribes to) inside a fresh store, so each
   handler's effect on the cache is observed without touching CRUD."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.events.core :as events]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.stats :as stats]
            [hive-mcp.knowledge-graph.edges.stats-events :as stats-events]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- view [] (select-keys (stats/snapshot) [:total-edges :by-relation :by-scope]))

(deftest-golden events-drive-the-stats-cache-golden
  "test/golden/hive-mcp/knowledge-graph/edges-stats-events.edn"
  (sup/in-fresh-store
   (fn []
     (let [registered (stats-events/register-handlers!)
           _          (stats/reset-cache!)
           _          (stats/refresh!)
           empty      (view)
           _          (events/dispatch-sync [:kg.edges/added {:relation :implements :scope "s1"}])
           _          (events/dispatch-sync [:kg.edges/added {:relation :implements :scope "s1"}])
           _          (events/dispatch-sync [:kg.edges/added {:relation :refines :scope nil}])
           added      (view)
           _          (events/dispatch-sync [:kg.edges/removed {:relation :implements :scope "s1"}])
           removed    (view)
           _          (events/dispatch-sync [:kg.edges/scope-migrated {:old-scope "s1" :new-scope "s2" :n 1}])
           migrated   (view)]
       {:registered registered :empty empty :added added :removed removed :migrated migrated}))))

(deftest registration-is-idempotent
  (is (nil? (stats-events/register-handlers!)))
  (is (nil? (stats-events/register-handlers!))))
