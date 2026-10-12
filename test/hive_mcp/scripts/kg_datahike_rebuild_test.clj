(ns hive-mcp.scripts.kg-datahike-rebuild-test
  "Trifecta coverage for the rebuild export classifier. The 2026-07-01 KG
   rebuild exported :kg-edge entities only, so the fresh store lost every
   :mem-mutation audit row (the temporal trail floor at 2026-07-02). The
   classifier must keep mutation rows and durable edges, drop carto
   structural edges.

   First run seeds the golden:
     UPDATE_GOLDEN=true clj -M:test --focus hive-mcp.scripts.kg-datahike-rebuild-test"
  (:require [clojure.test.check.generators :as gen]
            [hive-mcp.scripts.kg-datahike-rebuild :as rebuild]
            [hive-test.trifecta :refer [deftrifecta]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private uuid-a "0f8fad5b-d9cb-469f-a165-70867728950e")
(def ^:private uuid-b "7c9e6679-7425-40de-944b-e07fc1f90ae7")

(defn kind? [k]
  (contains? #{:mutation :edge-durable :edge-structural :unknown} k))

(def ^:private gen-endpoint
  (gen/one-of [(gen/elements [uuid-a uuid-b]) (gen/not-empty gen/string-alphanumeric)]))

(def ^:private gen-entity
  (gen/one-of
   [(gen/hash-map :mem-mutation/id (gen/fmap #(str "mut-" %) gen/string-alphanumeric)
                  :mem-mutation/op (gen/elements [:kanban-done :decay :feedback]))
    (gen/hash-map :kg-edge/id (gen/not-empty gen/string-alphanumeric)
                  :kg-edge/from gen-endpoint
                  :kg-edge/to gen-endpoint)
    (gen/return {:some/attr "x"})]))

(deftrifecta rebuild-entity-kind
  hive-mcp.scripts.kg-datahike-rebuild/entity-kind
  {:golden-path "test/golden/hive-mcp/kg-rebuild-entity-kind.edn"
   :cases       {:mutation        {:mem-mutation/id "mut-1" :mem-mutation/op :kanban-done}
                 :edge-durable    {:kg-edge/id "e1" :kg-edge/from "20260101-abc" :kg-edge/to uuid-a}
                 :edge-structural {:kg-edge/id "e2" :kg-edge/from uuid-a :kg-edge/to uuid-b}
                 :unknown-map     {:some/attr "x"}
                 :not-a-map       nil}
   :gen         gen-entity
   :pred        kind?
   :num-tests   200
   :mutations   [["edges-only"     (fn [e] (cond (not (map? e)) :unknown
                                                  (:kg-edge/id e) :edge-durable
                                                  :else :unknown))]
                 ["drop-mutations" (fn [e] (if (:mem-mutation/id e) :unknown :edge-durable))]
                 ["all-structural" (fn [_] :edge-structural)]]})

(deftrifecta rebuild-keep-entity
  hive-mcp.scripts.kg-datahike-rebuild/keep-entity?
  {:golden-path "test/golden/hive-mcp/kg-rebuild-keep-entity.edn"
   :cases       {:mutation        {:mem-mutation/id "mut-1" :mem-mutation/op :decay}
                 :edge-durable    {:kg-edge/id "e1" :kg-edge/from "a" :kg-edge/to "b"}
                 :edge-structural {:kg-edge/id "e2" :kg-edge/from uuid-a :kg-edge/to uuid-b}
                 :unknown-map     {:some/attr "x"}}
   :gen         gen-entity
   :pred        boolean?
   :num-tests   200
   :mutations   [["keep-all"          (fn [_] true)]
                 ["edges-only-legacy" (fn [e] (boolean (and (:kg-edge/id e)
                                                            (not (rebuild/structural-edge? e)))))]
                 ["keep-none"         (fn [_] false)]]})
