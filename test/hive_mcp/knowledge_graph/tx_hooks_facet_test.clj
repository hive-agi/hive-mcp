(ns hive-mcp.knowledge-graph.tx-hooks-facet-test
  "Property and golden facets for hive-mcp.knowledge-graph.tx-hooks: hooks
   compose in id order, a throwing hook refuses the whole transaction, and
   with no hook tx-data passes through untouched."
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.knowledge-graph.tx-hooks :as tx-hooks]
            [hive-test.golden :as golden]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- clean! [] (doseq [id (tx-hooks/registered)] (tx-hooks/unregister! id)))

(use-fixtures :each (fn [t] (clean!) (try (t) (finally (clean!)))))

(def ^:private TxData
  "Backend tx-data: entity maps or datom vectors."
  [:vector {:gen/max 5}
   [:or [:map [:kg-edge/id :string] [:kg-edge/relation :keyword]]
    [:tuple [:enum :db/add :db/retract] :int :keyword :string]]])

(def ^:private Scenario
  "tx-data, hook ids that each append their id, and an id that throws."
  [:tuple TxData
   [:set {:gen/max 5} [:enum :a/one :b/two :c/three :d/four :e/five]]
   [:maybe [:enum :b/two :d/four]]])

(defn- run-scenario [[tx ids refuse-id]]
  (clean!)
  (doseq [id ids]
    (tx-hooks/register! id (if (= id refuse-id)
                             (fn [_] (throw (ex-info "refused" {:by id})))
                             #(conj % id))))
  (let [out {:registered (tx-hooks/registered)
             :result (try {:tx (tx-hooks/apply-hooks tx)}
                          (catch clojure.lang.ExceptionInfo e {:refused-by (:by (ex-data e))}))}]
    (clean!)
    out))

(defspec hooks-compose-in-id-order-or-refuse 200
  (prop/for-all [[tx ids refuse-id :as sc] (mg/generator Scenario)]
    (let [{:keys [registered result]} (run-scenario sc)
          order (vec (sort ids))]
      (and (= order registered)
           (if (contains? ids refuse-id)
             (= {:refused-by refuse-id} result)
             (= {:tx (into tx order)} result))))))

(defspec no-hook-is-identity 100
  (prop/for-all [tx (mg/generator TxData)]
    (clean!)
    (= tx (tx-hooks/apply-hooks tx))))

(golden/deftest-golden tx-hooks-scenarios-golden
  "test/golden/hive-mcp/knowledge-graph/tx-hooks-scenarios.edn"
  (into (sorted-map)
        (for [[label sc] {"none"     [[{:kg-edge/id "e1" :kg-edge/relation :refines}] #{} nil]
                          "ordered"  [[[:db/add 1 :a "x"]] #{:c/three :a/one} nil]
                          "refused"  [[] #{:a/one :b/two :c/three} :b/two]}]
          [label (run-scenario sc)])))
