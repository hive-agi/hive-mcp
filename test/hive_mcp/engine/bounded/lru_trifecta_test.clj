(ns hive-mcp.engine.bounded.lru-trifecta-test
  "Schema-derived property and mutation facets for the contracted pure LRU
   steps, plus the standing contract gate for engine/bounded."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.engine.bounded.lru :as lru]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage bounded-contract-coverage
  {:paths ["src/hive_mcp/engine/bounded"]})

(def ^:private SmallFlatState
  "FlatState sized so generated queues are both under and at capacity."
  [:map
   [:q [:fn {:gen/schema [:vector {:max 4} :int]
             :gen/fmap #(into clojure.lang.PersistentQueue/EMPTY %)}
        #(instance? clojure.lang.PersistentQueue %)]]
   [:capacity [:int {:min 1 :max 4}]]
   [:policy [:maybe [:enum :drop-oldest :drop-newest]]]
   [:added [:int {:min 0 :max 50}]]
   [:evicted [:int {:min 0 :max 50}]]
   [:rejected [:int {:min 0 :max 50}]]])

(defn- expected-offer
  "Independent oracle for one flat offer, read off the docstring."
  [{:keys [q capacity policy added evicted rejected] :as s} item]
  (let [items (vec q)]
    (cond
      (< (count items) capacity)
      [(assoc s :q (conj items item) :added (inc added)) {:outcome :added}]

      (= :drop-newest policy)
      [(assoc s :q items :rejected (inc rejected)) {:outcome :rejected :evicted item}]

      :else
      [(assoc s :q (conj (subvec items 1) item) :added (inc added) :evicted (inc evicted))
       {:outcome :added-and-evicted :evicted (first items)}])))

(defn- comparable [[state outcome]]
  [(update state :q vec) outcome])

;; No :golden-path: the state carries a PersistentQueue, which prints as
;; <-(..)-< and does not round-trip through EDN, so a snapshot cannot be read
;; back. The relation oracle and the mutation facet carry the check instead.
(deftrifecta-from-schema offer-flat-trifecta
  hive-mcp.engine.bounded.lru/offer-flat
  {:in [:cat SmallFlatState :int]
   :out [:tuple lru/FlatState lru/Outcome]
   :rel (fn [[state item] out]
          (= (comparable out) (expected-offer state item)))
   :num-tests 300
   :seed 37
   :n-cases 12})

(deftest drain-flat-empties-and-returns-items
  (let [s {:q (into (lru/empty-queue) [1 2 3]) :capacity 3}
        [s' out] (lru/drain-flat s)]
    (is (= [1 2 3] out))
    (is (empty? (:q s')))))
