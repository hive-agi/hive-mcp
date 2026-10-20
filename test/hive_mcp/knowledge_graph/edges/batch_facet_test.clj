(ns hive-mcp.knowledge-graph.edges.batch-facet-test
  "Golden over the batch readers on a seeded store: the non-empty cases the
   existing batch trifecta does not reach."
  (:require [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.edges.batch :as batch]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- by-node [m] (into (sorted-map) (map (fn [[k v]] [k (sup/shown-all v)])) m))

(defn- reads []
  (sup/in-fresh-store
   (fn []
     (sup/seed! write/add-edge!)
     (write/add-edge! {:from "c1" :to "c2" :relation :co-accessed :confidence 0.6})
     (write/add-edge! {:from "c3" :to "c1" :relation :co-accessed :confidence 0.2})
     (let [db (conn/db-snapshot)]
       {:from (by-node (batch/batch-get-edges-from ["n1" "n2" "zz"]))
        :from-scoped (by-node (batch/batch-get-edges-from ["n1"] "s2"))
        :to (by-node (batch/batch-get-edges-to '("n3")))
        :to-scoped (by-node (batch/batch-get-edges-to ["n3" "n3"] "s2"))
        :from-db (by-node (batch/batch-get-edges-from-with-db db ["n2"]))
        :from-db-scoped (by-node (batch/batch-get-edges-from-with-db db ["n1"] "s1"))
        :to-db (by-node (batch/batch-get-edges-to-with-db db ["n2"]))
        :to-db-scoped (by-node (batch/batch-get-edges-to-with-db db ["n3"] "nope"))
        :co (into (sorted-map) (batch/batch-get-co-accessed ["c1" "c2" "n1"]))}))))

(golden/deftest-golden batch-golden
  "test/golden/hive-mcp/m14-edges-batch.edn"
  (reads))
