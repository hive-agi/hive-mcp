(ns hive-mcp.extensions.record-hooks-facet-test
  "Property and golden facets for the hive-mcp.extensions.record-hooks
   registry: registration order, chain application and drop semantics, over
   inputs generated from the contracts' own Point schema."
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.extensions.record-hooks :as hooks]
            [hive-test.golden :as golden]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private points (vec (rest hooks/Point)))

(defn- clean! []
  (doseq [p points id (hooks/registered p)] (hooks/unregister! p id)))

(use-fixtures :each (fn [t] (clean!) (try (t) (finally (clean!)))))

(def ^:private Scenario
  "A point, the hook ids to register there (each appends its own id to the
   record's :trail), and one id among them that drops instead."
  [:tuple hooks/Point
   [:set {:gen/max 6} [:enum :a/one :b/two :c/three :d/four :e/five :f/six]]
   [:maybe [:enum :a/one :c/three :f/six]]])

(defn- run-scenario [[point ids drop-id]]
  (clean!)
  (doseq [id ids]
    (hooks/register! point id (if (= id drop-id)
                                (constantly nil)
                                #(update % :trail conj id))))
  (let [out {:registered (hooks/registered point)
             :applied (hooks/apply-hooks point {:trail []})
             :elsewhere (hooks/apply-hooks (first (remove #{point} points)) {:trail []})}]
    (clean!)
    out))

(defspec hooks-run-in-id-order-until-one-drops 200
  (prop/for-all [[_ ids drop-id :as sc] (mg/generator Scenario)]
    (let [{:keys [registered applied elsewhere]} (run-scenario sc)
          order (vec (sort ids))
          kept (vec (take-while #(not= % drop-id) order))]
      (and (= order registered)
           (= {:trail []} elsewhere)
           (if (contains? ids drop-id)
             (nil? applied)
             (= {:trail kept} applied))))))

(defspec unregister-removes-only-that-id 100
  (prop/for-all [[point ids _] (mg/generator Scenario)]
    (clean!)
    (doseq [id ids] (hooks/register! point id identity))
    (let [victim (first (sort ids))
          answer (when victim (hooks/unregister! point victim))
          left (hooks/registered point)]
      (clean!)
      (and (= answer victim)
           (= (vec (sort (disj ids victim))) left)))))

(golden/deftest-golden record-hooks-scenarios-golden
  "test/golden/hive-mcp/extensions/record-hooks-scenarios.edn"
  (into (sorted-map)
        (for [[label sc] {"none"        [:export/entry #{} nil]
                          "two-ordered" [:archive/record #{:b/two :a/one} nil]
                          "drop-middle" [:kanban/history #{:a/one :c/three :d/four} :c/three]
                          "drop-first"  [:project/visible #{:a/one :b/two} :a/one]}]
          [label (run-scenario sc)])))
