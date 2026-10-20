(ns hive-mcp.recall.canary-contract-test
  "Contract facets for the pure canary taxonomy: every fault fn, the outcome
   and verdict folds, and the skip-regression detector. Inputs are generated so
   that every fault arm and the healthy arm are reached (:classify)."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [hive-mcp.recall.canary :as c]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:enum "e1" "e2" "e3" "e4"])
(def ^:private Label [:enum :lexical-anchor :supersession :carto-tag :carto-semantic])
(def ^:private GenRow [:map [:id Id] [:distance {:optional true} [:maybe [:double {:min 0.0 :max 2.0}]]]])
(def ^:private GenRows [:vector {:max 4} GenRow])

(defn- ids-of [rows] (set (keep :id rows)))

(deftrifecta-from-schema recall-fault-separates-empty-missing-and-healthy
  #'c/recall-fault
  {:in [:cat [:map [:label Label]
              ;; mostly populated, so the two fault arms are reached as often as
              ;; the healthy one
              [:populated? [:boolean {:gen/elements [true true true false]}]]
              [:results [:or [:= []] GenRows]]
              [:must-contain [:vector {:min 1 :max 2} Id]]]]
   :out [:maybe c/RecallFault]
   :rel (fn [[{:keys [populated? results must-contain]}] out]
          (let [missing (set/difference (set must-contain) (ids-of results))]
            (case (:fault out)
              nil (or (not populated?) (and (seq results) (empty? missing)))
              :recall/empty-from-populated-store (and populated? (empty? results))
              :recall/anchor-missing (and populated? (seq results)
                                          (= missing (set (:missing-ids out)))))))
   :classify (fn [_ out] (or (:fault out) :healthy))
   :classify-domain #{:healthy :recall/empty-from-populated-store :recall/anchor-missing}
   :golden-path "test/golden/hive-mcp/recall/canary-recall-fault.edn"
   :num-tests 200})

(deftrifecta-from-schema rank-fault-flags-any-descent-in-distance
  #'c/rank-fault
  {:in [:cat [:map [:label Label]
              [:results [:vector {:min 2 :max 4}
                         [:map [:id Id] [:distance [:or [:double {:min 0.0 :max 2.0}] :nil]]]]]]]
   :out [:maybe c/RankFault]
   :rel (fn [[{:keys [results]}] out]
          (let [ds (keep :distance results)]
            (if out
              (and (= (vec ds) (:distances out)) (some (fn [[a b]] (> a b)) (partition 2 1 ds)))
              (every? (fn [[a b]] (<= a b)) (partition 2 1 ds)))))
   :classify (fn [_ out] (if out :inverted :ordered))
   :classify-domain #{:inverted :ordered}
   :golden-path "test/golden/hive-mcp/recall/canary-rank-fault.edn"
   ;; Two or more rows per case, so the seeded cases include inversions and
   ;; the drop-key mutants have a fault map to break.
   :num-tests 200})

(def ^:private GenReading
  [:map [:collection [:enum "hive_384" "hive_768" "hive"]]
   [:expected {:optional true} [:enum 384 768]]
   [:actual {:optional true} [:maybe [:enum 384 768]]]])

(deftrifecta-from-schema dimension-fault-checks-only-derivable-widths
  #'c/dimension-fault
  {:in [:cat [:map [:label [:= :dimension-invariant]]
              [:readings [:vector {:gen/schema [:tuple [:vector {:max 4} GenReading] :boolean]
                                   ;; half the cases heal every known width, so the
                                   ;; healthy arm is reached as often as the faults
                                   :gen/fmap (fn [[rs heal?]]
                                               (if heal?
                                                 (mapv #(if (:expected %) (assoc % :actual (:expected %)) %) rs)
                                                 rs))}
                          GenReading]]]]
   :out [:maybe c/DimensionFault]
   :rel (fn [[{:keys [readings]}] out]
          (let [known (filter :expected readings)
                bad (remove #(= (:expected %) (:actual %)) known)]
            (case (:fault out)
              :recall/no-collections-configured (empty? known)
              :recall/dimension-mismatch (and (seq known) (= (vec bad) (:mismatched out)))
              nil (and (seq known) (empty? bad)))))
   :classify (fn [_ out] (or (:fault out) :healthy))
   :classify-domain #{:healthy :recall/no-collections-configured :recall/dimension-mismatch}
   :golden-path "test/golden/hive-mcp/recall/canary-dimension-fault.edn"
   :num-tests 200})

(deftrifecta-from-schema supersession-fault-catches-the-retracted-row
  #'c/supersession-fault
  {:in [:cat [:map [:label [:= :supersession]] [:results GenRows]
              [:superseded-id Id] [:current-id [:maybe Id]]]]
   :out [:maybe c/SupersessionFault]
   :rel (fn [[{:keys [results superseded-id current-id]}] out]
          (let [ids (ids-of results)]
            (case (:fault out)
              :recall/superseded-returned (contains? ids superseded-id)
              :recall/current-missing (and (not (contains? ids superseded-id))
                                           current-id (not (contains? ids current-id)))
              nil (and (not (contains? ids superseded-id))
                       (or (nil? current-id) (contains? ids current-id))))))
   :classify (fn [_ out] (or (:fault out) :healthy))
   :classify-domain #{:healthy :recall/superseded-returned :recall/current-missing}
   :golden-path "test/golden/hive-mcp/recall/canary-supersession-fault.edn"
   :num-tests 200})

(deftrifecta-from-schema presence-fault-wants-a-positive-count
  #'c/presence-fault
  {:in [:cat [:map [:label Label] [:count [:or [:int {:min -2 :max 3}] :nil :string]]
              [:probe :string] [:diagnosis {:optional true} [:maybe :string]]]]
   :out [:maybe c/PresenceFault]
   :rel (fn [[{:keys [count diagnosis]}] out]
          (if (and (number? count) (pos? count))
            (nil? out)
            (and (= count (:count out)) (if diagnosis (= diagnosis (:diagnosis out)) (string? (:diagnosis out))))))
   :classify (fn [_ out] (if out :empty :present))
   :classify-domain #{:empty :present}
   :golden-path "test/golden/hive-mcp/recall/canary-presence-fault.edn"
   :num-tests 200})

(def ^:private GenFault [:map [:fault [:enum :recall/anchor-missing :recall/rank-inverted]]])

(deftrifecta-from-schema outcome-prefers-skip-then-fault
  #'c/outcome
  {:in [:cat Label [:maybe GenFault] [:maybe [:enum "no store" "carto not loaded"]]]
   :out c/Outcome
   :rel (fn [[label fault skip] out]
          (and (= label (:label out))
               (= (:status out) (cond skip :skipped fault :fault :else :ok))))
   :classify (fn [_ out] (:status out))
   :classify-domain #{:ok :fault :skipped}
   :golden-path "test/golden/hive-mcp/recall/canary-outcome.edn"
   :num-tests 200})

(deftest outcome-two-arity-never-skips
  (is (= {:label :a :status :ok} (c/outcome :a nil)))
  (is (= :fault (:status (c/outcome :a {:fault :x})))))

(def ^:private GenOutcome
  [:multi {:dispatch :status}
   [:ok [:map [:label Label] [:status [:= :ok]]]]
   [:fault [:map [:label Label] [:status [:= :fault]] [:fault GenFault]]]
   [:skipped [:map [:label Label] [:status [:= :skipped]] [:reason [:enum "no store" "absent"]]]]])

(deftrifecta-from-schema verdict-counts-every-status
  #'c/verdict
  {:in [:cat [:vector {:max 5} GenOutcome]]
   :out c/Verdict
   :rel (fn [[os] out]
          (let [by (group-by :status os)]
            (and (= (count os) (:ran out))
                 (= (count (:ok by)) (:passed out))
                 (= (mapv :fault (:fault by)) (:faults out))
                 (= (empty? (:fault by)) (:ok? out))
                 (= (count (:skipped by)) (count (:skipped out)))
                 (= (set (map :label (concat (:ok by) (:fault by)))) (:ran-labels out)))))
   :classify (fn [_ out] (if (:ok? out) :ok :faulted))
   :classify-domain #{:ok :faulted}
   :golden-path "test/golden/hive-mcp/recall/canary-verdict.edn"
   :num-tests 200})

(def ^:private GenVerdict
  [:map [:ok? :boolean]
   [:ran-labels [:set Label]]
   [:skipped [:vector {:max 3} [:map [:label Label] [:reason [:enum "no store" "absent"]]]]]
   [:faults [:vector {:max 2} GenFault]]])

(deftrifecta-from-schema skip-regressions-names-what-went-dark
  #'c/skip-regressions
  {:in [:cat [:maybe GenVerdict] GenVerdict]
   :out [:vector c/Regression]
   :rel (fn [[prev cur] out]
          (let [dark (if prev
                       (set/intersection (:ran-labels prev) (set (map :label (:skipped cur))))
                       #{})]
            (and (= dark (set (map :label out)))
                 (= (map :label out) (sort (map :label out))))))
   :classify (fn [_ out] (if (seq out) :regressed :steady))
   :classify-domain #{:regressed :steady}
   :golden-path "test/golden/hive-mcp/recall/canary-skip-regressions.edn"
   :num-tests 200})

(deftrifecta-from-schema with-regressions-keeps-dark-probes-expected
  #'c/with-regressions
  {:in [:cat [:maybe GenVerdict] GenVerdict]
   :out [:maybe :map]
   :rel (fn [[prev cur] out]
          (let [regs (c/skip-regressions prev cur)]
            (if (seq regs)
              (and (false? (:ok? out))
                   (= (into (:faults cur) regs) (:faults out))
                   (set/subset? (set (map :label regs)) (:ran-labels out)))
              (= cur out))))
   :classify (fn [[prev cur] _] (if (seq (c/skip-regressions prev cur)) :regressed :unchanged))
   :classify-domain #{:regressed :unchanged}
   :num-tests 200})

(deftrifecta-from-schema fixture-looks-up-a-role
  #'c/fixture
  {:in [:cat [:enum :anchor :superseded :current :unknown nil]]
   :out [:maybe c/Fixture]
   :rel (fn [[role] out] (if (#{:anchor :superseded :current} role) (= role (:role out)) (nil? out)))
   :golden-path "test/golden/hive-mcp/recall/canary-fixture.edn"
   :num-tests 40})

(deftrifecta-from-schema fixture-tags-carry-the-canary-tag
  #'c/fixture-tags
  {:in [:cat [:enum :anchor :superseded :current :unknown]]
   :out [:maybe [:vector :string]]
   :rel (fn [[role] out] (if (= role :unknown) (nil? out) (= c/canary-tag (first out))))
   :num-tests 40})

(deftest every-fixture-validates
  (is (every? #(m/validate c/Fixture %) c/fixtures))
  (is (= [:anchor :superseded :current] (mapv :role c/fixtures))))
