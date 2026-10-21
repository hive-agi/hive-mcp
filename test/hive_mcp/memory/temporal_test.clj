(ns hive-mcp.memory.temporal-test
  "The :mem-mutation audit trail. prune-victims is pure and contracted: a
   trifecta with a hand-checkable oracle over generated rows (two entry ids,
   three timestamps around the cutoff, retained and prunable ops). The store
   arms are goldens inside a fresh in-memory KG store, which is NOT a temporal
   store, so every effectful fn must take its documented non-temporal path.

   Seam gap: the temporal arms need a store with (temporal-store?) true, which
   only the Datahike backend provides; no in-memory temporal adapter exists
   (see the f04 report)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.store.fixtures :as kg-fixtures]
            [hive-mcp.memory.temporal :as temporal]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private t0 #inst "2026-01-01T00:00:00.000-00:00")
(def ^:private cutoff #inst "2026-01-01T00:00:10.000-00:00")
(def ^:private t2 #inst "2026-01-01T00:00:20.000-00:00")
(def ^:private t3 #inst "2026-01-01T00:00:30.000-00:00")

(def ^:private Rows
  "Rows [eid entry-id ts op] with DISTINCT eids, drawn so aged, overflow and
   retained rows all occur."
  [:sequential {:gen/schema [:vector {:max 10}
                             [:tuple [:enum "e1" "e2"] [:enum :t0 :t2 :t3]
                              [:enum :update :feedback :kanban-move]]]
                :gen/fmap (fn [rs] (vec (map-indexed (fn [i [eid ts op]]
                                                       [i eid ({:t0 t0 :t2 t2 :t3 t3} ts) op])
                                                     rs)))}
   [:tuple :int :string inst? :keyword]])

(def ^:private Opts
  [:map [:max-per-entry [:int {:min 0 :max 3}]] [:batch-cap [:int {:min 0 :max 6}]]])

(defn- oracle
  "The victim set prune-victims documents, computed independently."
  [rows {:keys [max-per-entry batch-cap]}]
  (let [prunable (remove #(contains? temporal/retained-ops (nth % 3)) rows)
        aged     (set (keep (fn [[e _ ts]] (when (neg? (compare ts cutoff)) e)) prunable))
        overflow (set (mapcat (fn [[_ es]]
                                (let [n (- (count es) max-per-entry)]
                                  (when (pos? n) (map first (take n (sort-by #(nth % 2) es))))))
                              (group-by second prunable)))]
    {:all (into aged overflow) :cap batch-cap}))

(deftrifecta-from-schema prune-victims-selects-aged-and-overflow
  #'temporal/prune-victims
  {:in [:cat Rows [:= cutoff] Opts]
   :out [:vector :int]
   :rel (fn [[rows _ opts] out]
          (let [{:keys [all cap]} (oracle rows opts)]
            (and (= (count out) (count (distinct out)) (min cap (count all)))
                 (every? all out)
                 (not-any? #(contains? temporal/retained-ops (nth (nth rows %) 3)) out))))
   :classify (fn [_ out] (if (seq out) :prunes :keeps))
   :classify-domain #{:prunes :keeps}
   :golden-path "test/golden/hive-mcp/memory/temporal-prune-victims.edn"
   :num-tests 200})

(deftest-golden non-temporal-store-golden
  "test/golden/hive-mcp/memory/temporal-non-temporal-store.edn"
  (kg-fixtures/datascript-fixture
   (fn []
     {:record   (temporal/record-mutation! {:entry-id "e1" :op :promote :data {:x 1}})
      :silent   (temporal/record-mutation-silent! {:entry-id "e1" :op :feedback})
      :batch    (temporal/record-mutations-batch! [{:entry-id "e1" :op :decay}])
      :query    (temporal/query-mutations "e1")
      :count    (temporal/mutation-count)
      :prune    (temporal/prune-mutations! {:force? true})
      :retained (vec (sort temporal/retained-ops))})))

(deftest invalid-op-is-a-precondition
  (is (thrown? AssertionError (temporal/record-mutation! {:entry-id "e1" :op :not-an-op}))))
