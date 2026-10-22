(ns hive-mcp.knowledge-graph.edges.scoped-facet-test
  "Facets for hive-mcp.knowledge-graph.edges.scoped: the visibility rule
   (property over the pure `visible?`) and the indexed reads over a small
   fixed graph in a disposable DataScript store (golden)."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.edges.scoped :as scoped]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.knowledge-graph.store.harness :as harness]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.kg.protocol :as spi]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ScopeTag [:enum "scope:project:a" "scope:project:b" "scope:global"])

(deftrifecta-from-schema visible hive-mcp.knowledge-graph.edges.scoped/visible?
  {:in  [:cat [:map [:kg-edge/id :string] [:kg-edge/scope ScopeTag]]
         [:maybe [:set {:gen/max 1} ScopeTag]]]
   :out :boolean
   :rel (fn [[edge scope-set] out]
          (= out (or (nil? scope-set)
                     (not (contains? edge :kg-edge/scope))
                     (contains? scope-set (:kg-edge/scope edge)))))
   :classify (fn [_ out] (if out :visible :hidden))
   :classify-domain #{:visible :hidden}
   :num-tests 200})

(defrecord ^:private DsFactory []
  harness/StoreFactory
  (available? [_] true)
  (create [_]
    (let [ds (ds-store/create-store)]
      (spi/ensure-conn! ds)
      (harness/->EphemeralStore ds nil)))
  (dispose! [_ ephemeral] (spi/reset-conn! (:store ephemeral))))

(use-fixtures :each
  (fn [f]
    (harness/with-disposable-store
      (->DsFactory)
      (harness/thread-local-isolation :sync-writes? true)
      f)))

(def ^:private graph
  "A chain n0-n1-n2-n3 plus a branch n1-n4, a self-loop on n3, over two
   project scopes, global and scope-less edges."
  [{:kg-edge/id "e1" :kg-edge/from "n0" :kg-edge/to "n1" :kg-edge/relation :implements
    :kg-edge/scope "scope:project:a"}
   {:kg-edge/id "e2" :kg-edge/from "n1" :kg-edge/to "n2" :kg-edge/relation :depends-on}
   {:kg-edge/id "e3" :kg-edge/from "n2" :kg-edge/to "n3" :kg-edge/relation :refines
    :kg-edge/scope "scope:project:b"}
   {:kg-edge/id "e4" :kg-edge/from "n1" :kg-edge/to "n4" :kg-edge/relation :relates
    :kg-edge/scope "scope:global"}
   {:kg-edge/id "e5" :kg-edge/from "n3" :kg-edge/to "n3" :kg-edge/relation :refines
    :kg-edge/scope "scope:project:a"}])

(defn- ids [edges] (mapv :kg-edge/id edges))

(defn- reads-trace []
  (conn/transact! graph)
  {:visible (into (sorted-map)
                  (for [[label s] {"all" nil
                                   "none" #{}
                                   "a" #{"scope:project:a"}
                                   "a+global" #{"scope:project:a" "scope:global"}}]
                    [label (ids (scoped/visible-edges s))]))
   :neighborhood (into (sorted-map)
                       (for [[label [node opts]] {"n1-depth1-all" ["n1" {:max-depth 1}]
                                                  "n1-depth2-all" ["n1" {:max-depth 2}]
                                                  "n1-depth2-a"   ["n1" {:max-depth 2 :scope-set #{"scope:project:a"}}]
                                                  "n0-depth0"     ["n0" {:max-depth 0}]
                                                  "ghost"         ["nx" {:max-depth 3}]}]
                         [label (ids (scoped/neighborhood-edges node opts))]))
   :occurrences (into (sorted-map) (scoped/node-occurrences))
   :eid-count (count (scoped/visible-edge-eids nil))})

(golden/deftest-golden indexed-reads-golden
  "test/golden/hive-mcp/knowledge-graph/edges-scoped-reads.edn"
  (reads-trace))
