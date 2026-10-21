(ns hive-mcp.resilience.breaker-test
  "Name-matched facets for `hive-mcp.resilience.breaker`.

   Pure transitions are driven with an EXPLICIT timestamp argument, so no
   facet here reads the wall clock:

     on-failure     trifecta, exact-output oracle (the state-machine table
                    of the ns docstring, re-derived independently)
     maybe-recover  trifecta, exact-output oracle at the cooldown boundary
     scripts        defspec: any sequence of failure / success / recover
                    steps keeps the record a Breaker, and :open is the only
                    state that blocks

   The atom orchestration (attempt / record-failure! / record-success! /
   snapshot / reset!) is pinned by a golden that strips :opened-at, the one
   field stamped from System/currentTimeMillis; the default 30 s cooldown
   means no step of the golden can cross it, so the snapshot does not
   depend on timing."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.resilience.breaker :as breaker]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private SmallBreaker
  "breaker/Breaker with test-sized numbers: cooldowns are capped by the
   policy in every reachable record, so an unbounded int would only
   exercise long overflow, a state no transition produces. Failures stay
   below SmallPolicy's widest threshold so the closed arm is reached about
   half the time; :open is weighted so the recovery boundary is reached."
  [:map
   [:state [:enum :open :open :open :closed :half-open]]
   [:failures [:int {:min 0 :max 2}]]
   [:opened-at [:maybe [:int {:min 0 :max 500}]]]
   [:cooldown-ms [:int {:min 0 :max 400}]]])

(def ^:private SmallPolicy
  [:map
   [:max-failures [:int {:min 1 :max 6}]]
   [:initial-cooldown-ms [:int {:min 1 :max 100}]]
   [:max-cooldown-ms [:int {:min 100 :max 400}]]])

(defn- expected-failure
  "Oracle for on-failure: the transition table of the ns docstring."
  [{:keys [state failures cooldown-ms] :as b}
   {:keys [max-failures initial-cooldown-ms max-cooldown-ms]} ts]
  (let [n     (inc failures)
        open? (or (= :half-open state) (>= n max-failures))]
    (if open?
      (assoc b :state :open :failures n :opened-at ts
             :cooldown-ms (min max-cooldown-ms (max initial-cooldown-ms (* 2 cooldown-ms))))
      (assoc b :state :closed :failures n))))

(deftrifecta-from-schema on-failure-follows-the-transition-table
  #'breaker/on-failure
  {:in [:cat SmallBreaker SmallPolicy [:int {:min 0 :max 2000}]]
   :out breaker/Breaker
   :rel (fn [[b p ts] out] (= out (expected-failure b p ts)))
   :classify (fn [_ out] (:state out))
   :classify-domain #{:open :closed}
   :golden-path "test/golden/hive-mcp/resilience/breaker-transition-table.edn"
   :num-tests 300})

(deftrifecta-from-schema maybe-recover-promotes-at-the-boundary
  #'breaker/maybe-recover
  {:in [:cat SmallBreaker [:int {:min 0 :max 1500}]]
   :out breaker/Breaker
   :rel (fn [[{:keys [state opened-at cooldown-ms] :as b} ts] out]
          (= out (if (and (= :open state) opened-at (>= ts (+ opened-at cooldown-ms)))
                   (assoc b :state :half-open)
                   b)))
   :classify (fn [[b] out] (if (= (:state b) (:state out)) :unchanged :promoted))
   :classify-domain #{:unchanged :promoted}
   :num-tests 300})

(def ^:private Script
  "A run of breaker steps, each with its own explicit timestamp."
  [:vector {:max 12}
   [:or [:tuple [:= :fail] [:int {:min 0 :max 500}]]
    [:tuple [:= :ok]]
    [:tuple [:= :recover] [:int {:min 0 :max 1500}]]]])

(defn- run-script [policy steps]
  (reductions (fn [b [op ts]]
                (case op
                  :fail    (breaker/on-failure b policy ts)
                  :ok      (breaker/on-success b)
                  :recover (breaker/maybe-recover b ts)))
              (breaker/fresh)
              steps))

(defspec every-script-stays-a-breaker-and-only-open-blocks 200
  (prop/for-all [policy (mg/generator SmallPolicy)
                 steps  (mg/generator Script)]
    (every? (fn [b]
              (and (m/validate breaker/Breaker b)
                   (<= (:cooldown-ms b) (:max-cooldown-ms policy))
                   (= (= :block (breaker/decision b)) (= :open (:state b)))))
            (run-script policy steps))))

(defspec success-always-closes 100
  (prop/for-all [policy (mg/generator SmallPolicy)
                 steps  (mg/generator Script)]
    (= (breaker/fresh) (breaker/on-success (last (run-script policy steps))))))

(deftest-golden breaker-atom-orchestration-golden
  "test/golden/hive-mcp/resilience/breaker-orchestration.edn"
  (let [a      (atom {})
        p      breaker/default-policy
        strip  #(cond-> % (:opened-at %) (assoc :opened-at :stamped))
        steps  [[:attempt     (strip (breaker/attempt a :slot/x p))]
                [:failure-1   (strip (breaker/record-failure! a :slot/x p))]
                [:failure-2   (strip (breaker/record-failure! a :slot/x p))]
                [:failure-3   (strip (breaker/record-failure! a :slot/x p))]
                [:attempt-open (strip (breaker/attempt a :slot/x p))]
                [:other-slot  (strip (breaker/attempt a :slot/y p))]]
        snap   (update-vals (breaker/snapshot a) strip)
        ok     (update-vals (breaker/record-success! a :slot/x) strip)
        reset  (breaker/reset! a)]
    {:steps steps
     :decisions (mapv (fn [[k b]] [k (breaker/decision b)]) steps)
     :snapshot snap
     :after-success ok
     :after-reset reset
     :snapshot-after-reset (breaker/snapshot a)}))

(deftest fresh-is-closed-and-passes
  (is (= {:state :closed :failures 0 :opened-at nil :cooldown-ms 0} (breaker/fresh)))
  (is (= :pass (breaker/decision (breaker/fresh)))))
