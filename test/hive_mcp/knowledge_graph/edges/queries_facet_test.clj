(ns hive-mcp.knowledge-graph.edges.queries-facet-test
  "Golden over every edges.queries reader against a fresh seeded store."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.queries :as q]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- reads []
  (sup/in-fresh-store
   (fn []
     (let [empty-count (q/count-edges)
           id (write/add-edge! {:from "n0" :to "n1" :relation :applies-to})]
       (sup/seed! write/add-edge!)
       {:empty-count empty-count
        :get (sup/shown (q/get-edge id))
        :get-missing (q/get-edge "no-such-edge")
        :from (sup/shown-all (q/get-edges-from "n1"))
        :from-scoped (sup/shown-all (q/get-edges-from "n1" "s1"))
        :from-nil (q/get-edges-from nil)
        :to (sup/shown-all (q/get-edges-to "n3"))
        :to-scoped (sup/shown-all (q/get-edges-to "n3" "s2"))
        :by-relation (sup/shown-all (q/get-edges-by-relation :refines))
        :by-relation-scoped (sup/shown-all (q/get-edges-by-relation :implements "s2"))
        :by-scope (sup/shown-all (q/get-edges-by-scope "s2"))
        :find (sup/shown (q/find-edge "n1" "n2"))
        :find-relation (q/find-edge "n1" "n2" :refines)
        :between (sup/shown-all (q/find-edges-between #{"n1" "n2" "n3"}))
        :between-one (q/find-edges-between #{"n1"})
        :pulled (count (q/pull-edge-batch [1 2 3 4 5 999]))
        :all (count (q/get-all-edges))
        :all-scoped (sup/shown-all (q/get-all-edges "s1"))
        :all-batched (count (doall (q/get-all-edges nil {:batch-size 2})))
        :arcs (vec (sort-by (juxt :from :to) (q/get-all-edge-arcs)))
        :arcs-scoped (q/get-all-edge-arcs "s2")
        :count (q/count-edges)
        :count-scoped (q/count-edges "s1")
        :since (count (q/get-edges-since (java.time.Instant/ofEpochMilli 0) :limit 3))
        :since-scoped (sup/shown-all (q/get-edges-since (java.time.Instant/ofEpochMilli 0)
                                                        {:scope "s1" :exclude-relations #{}}))
        :since-future (q/get-edges-since (java.time.Instant/parse "2100-01-01T00:00:00Z"))}))))

(golden/deftest-golden queries-golden
  "test/golden/hive-mcp/m14-edges-queries.edn"
  (reads))

(deftest readers-return-their-contracted-shapes
  (sup/in-fresh-store
   (fn []
     (sup/seed! write/add-edge!)
     (is (m/validate (sup/out-schema 'hive-mcp.knowledge-graph.edges.queries 'get-all-edges)
                     (q/get-all-edges)))
     (is (m/validate (sup/out-schema 'hive-mcp.knowledge-graph.edges.queries 'get-all-edge-arcs)
                     (q/get-all-edge-arcs))))))
