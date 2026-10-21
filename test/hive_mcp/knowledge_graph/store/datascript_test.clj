(ns hive-mcp.knowledge-graph.store.datascript-test
  "The DataScript IKGStore adapter driven through every port method on a
   fresh store: lazy connection, transact, both query arities, entity /
   entid / pull, the :aevt attribute scan, snapshot, reset and close."
  (:require [clojure.test :refer [deftest is]]
            [datascript.core :as d]
            [hive-mcp.knowledge-graph.store.datascript :as kds]
            [hive-mcp.protocols.kg :as kg]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-golden port-methods-golden
  "test/golden/hive-mcp/knowledge-graph/store-datascript-port.edn"
  (let [store   (kds/create-store)
        unopened (nil? @(:conn-atom store))
        _       (kg/ensure-conn! store)
        same?   (identical? (kg/ensure-conn! store) (kg/ensure-conn! store))
        _       (kg/transact! store [{:kg-edge/id "e1" :kg-edge/from "a" :kg-edge/to "b"
                                      :kg-edge/relation :implements}
                                     {:kg-edge/id "e2" :kg-edge/from "b" :kg-edge/to "c"
                                      :kg-edge/relation :refines}])
        eid     (kg/entid store [:kg-edge/id "e1"])
        q0      (vec (sort (kg/query store '[:find [?id ...] :where [_ :kg-edge/id ?id]])))
        q1      (kg/query store '[:find ?to . :in $ ?from :where
                                  [?e :kg-edge/from ?from] [?e :kg-edge/to ?to]] ["b"])
        by-attr (count (kg/eids-by-attr store :kg-edge/id))
        pulled  (kg/pull-entity store [:kg-edge/from :kg-edge/to] [:kg-edge/id "e2"])
        ent     (:kg-edge/relation (kg/entity store eid))
        snap?   (d/db? (kg/db-snapshot store))
        _       (kg/reset-conn! store)
        after   (kg/query store '[:find [?id ...] :where [_ :kg-edge/id ?id]])]
    {:unopened unopened :same-conn same? :entid? (pos-int? eid) :q0 q0 :q1 q1
     :by-attr by-attr :pulled pulled :entity-relation ent :snapshot-db? snap?
     :after-reset after :close (kg/close! store)}))

(deftest create-store-satisfies-its-contract
  (is (m/validate kds/KGStore (kds/create-store))))
