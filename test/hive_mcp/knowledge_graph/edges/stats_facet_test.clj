(ns hive-mcp.knowledge-graph.edges.stats-facet-test
  "Goldens over the edge-stats cache and its event wiring, inside a fresh
   store: the harness resets the process-wide cache before and after."
  (:require [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.stats :as stats]
            [hive-mcp.knowledge-graph.edges.stats-events :as stats-events]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- cache-view [c] (select-keys c [:initialized? :scanned? :total-edges :by-relation :by-scope]))

(defn- deltas []
  (sup/in-fresh-store
   (fn []
     (let [reset (stats/reset-cache!)
           plus (cache-view (stats/apply-delta! :implements "s1" 1))
           plus2 (cache-view (stats/apply-delta! :refines nil 1))
           minus (cache-view (stats/apply-delta! :implements "s1" -1))
           floor (cache-view (stats/apply-delta! :x "s9" -5))
           moved (cache-view (stats/migrate-scope! "s1" "s2" 0))
           _ (stats/apply-delta! :implements "s1" 2)
           moved2 (cache-view (stats/migrate-scope! "s1" "s2" 1))]
       {:reset reset :plus plus :plus2 plus2 :minus minus :floor floor
        :moved-zero moved :moved moved2}))))

(golden/deftest-golden stats-deltas-golden
  "test/golden/hive-mcp/m14-edges-stats-deltas.edn"
  (deltas))

(defn- scanned []
  (sup/in-fresh-store
   (fn []
     (sup/seed! write/add-edge!)
     (stats/reset-cache!)
     (let [first-read (stats/snapshot)
           refreshed (stats/refresh!)
           registered (stats-events/register-handlers!)
           _ (write/add-edge! {:from "e1" :to "e2" :relation :implements :scope "s1"})]
       {:first-read first-read
        :refresh refreshed
        :register registered
        :after-event (stats/snapshot)}))))

(golden/deftest-golden stats-scan-golden
  "test/golden/hive-mcp/m14-edges-stats-scan.edn"
  (scanned))
