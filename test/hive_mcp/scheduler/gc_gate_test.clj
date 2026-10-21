(ns hive-mcp.scheduler.gc-gate-test
  "The explicit-GC gate, driven by an injected memory probe and a recording gc!.
   Nothing here touches the real heap or calls System/gc."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :as coverage]
            [malli.core :as m]
            [malli.generator :as mg]
            [hive-mcp.scheduler.gc-gate :as gate]
            [hive-mcp.scheduler.housekeeping :as housekeeping]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private MB (* 1024 1024))

(defn- probe-seq
  "A stub probe answering the given readings in order (last one repeats)."
  [& readings]
  (let [left (atom readings)]
    (fn []
      (let [[r & more] @left]
        (when more (reset! left more))
        r))))

(defn- recording-gc []
  (let [calls (atom 0)]
    {:calls calls :gc! (fn [] (swap! calls inc))}))

(defn- reading [committed-mb used-mb]
  {:committed (* committed-mb MB) :used (* used-mb MB)})

(deftest fragmented-needs-both-ratio-and-gap
  (testing "the live reading that motivated the card: 7688 committed vs 5670 used"
    (is (false? (gate/fragmented? (reading 7688 5670)))))
  (testing "ratio above 2x but gap under 1 GB"
    (is (false? (gate/fragmented? (reading 900 300)))))
  (testing "gap above 1 GB but ratio under 2x"
    (is (false? (gate/fragmented? (reading 5000 3000)))))
  (testing "both exceeded"
    (is (true? (gate/fragmented? (reading 3576 622)))))
  (testing "exactly at the boundary is not fragmented (strict >)"
    (is (false? (gate/fragmented? (reading 2048 1024)))))
  (testing "zero used is never fragmented"
    (is (false? (gate/fragmented? (reading 4096 0)))))
  (testing "thresholds are injectable"
    (is (true? (gate/fragmented? (reading 900 300) {:min-gap-bytes (* 100 MB)})))))

(deftest gate-closed-skips-gc
  (let [{:keys [calls gc!]} (recording-gc)
        r (gate/gc-hint! {:probe (probe-seq (reading 7688 5670)) :gc! gc!})]
    (is (= 0 @calls) "no explicit GC below the fragmentation gate")
    (is (false? (:gc? r)))
    (is (= :not-fragmented (:reason r)))
    (is (= 7688 (:committed-mb r)))
    (is (= 5670 (:used-mb r)))
    (is (= 0 (:freed-mb r)))
    (is (m/validate gate/GcHintResult r))))

(deftest gate-open-collects-once-and-reports-the-drop
  (let [{:keys [calls gc!]} (recording-gc)
        r (gate/gc-hint! {:probe (probe-seq (reading 3576 622) (reading 800 500))
                          :gc!   gc!})]
    (is (= 1 @calls))
    (is (true? (:gc? r)))
    (is (= :fragmented (:reason r)))
    (is (= 122 (:freed-mb r)))
    (is (= 2776 (:committed-freed-mb r)))
    (is (false? (:still-fragmented? r)))
    (is (m/validate gate/GcHintResult r))))

(deftest gc-that-does-not-uncommit-stays-fragmented
  (let [{:keys [gc!]} (recording-gc)
        r (gate/gc-hint! {:probe (probe-seq (reading 3576 622) (reading 3576 600))
                          :gc!   gc!})]
    (is (true? (:gc? r)))
    (is (true? (:still-fragmented? r)) "the ratchet housekeeping warns on")))

(deftest failing-ports-never-throw
  (testing "probe throws: no gc, :probe-failed"
    (let [{:keys [calls gc!]} (recording-gc)
          r (gate/gc-hint! {:probe #(throw (ex-info "mx down" {})) :gc! gc!})]
      (is (= 0 @calls))
      (is (= :probe-failed (:reason r)))
      (is (m/validate gate/GcHintResult r))))
  (testing "gc! throws: :gc-failed"
    (let [r (gate/gc-hint! {:probe (probe-seq (reading 3576 622))
                            :gc!   #(throw (ex-info "no" {}))})]
      (is (false? (:gc? r)))
      (is (= :gc-failed (:reason r)))
      (is (m/validate gate/GcHintResult r)))))

(deftest housekeeping-sweep-routes-through-the-gate
  (testing "an unfragmented heap: the sweep runs every task and calls no gc"
    (let [{:keys [calls gc!]} (recording-gc)
          r (housekeeping/housekeeping-sweep!
             {:gc-ports {:probe (probe-seq (reading 7688 5670)) :gc! gc!}})]
      (is (= 0 @calls))
      (is (= :not-fragmented (get-in r [:gc-hint :reason])))))
  (testing "a fragmented heap: exactly one gc"
    (let [{:keys [calls gc!]} (recording-gc)
          r (housekeeping/housekeeping-sweep!
             {:gc-ports {:probe (probe-seq (reading 3576 622) (reading 700 500))
                         :gc!   gc!}})]
      (is (= 1 @calls))
      (is (true? (get-in r [:gc-hint :gc?]))))))

(coverage/deftest-contract-coverage gc-gate-contracts
  {:paths  ["src/hive_mcp/scheduler/gc_gate.clj"]
   :exempt {}})

(deftest the-gate-is-not-vacuous
  (is (= 3 (count (:universe (coverage/coverage {:paths ["src/hive_mcp/scheduler/gc_gate.clj"]}))))))

(deftest every-gc-gate-contract-generates-its-input-side
  (doseq [[fn-sym {:keys [schema]}] (get (m/function-schemas) 'hive-mcp.scheduler.gc-gate)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          :let  [input (first (m/children arity))]]
    (is (= 3 (count (mg/sample input {:size 3 :seed 7})))
        (str fn-sym " " (pr-str (m/form input))))))
