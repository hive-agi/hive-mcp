(ns hive-mcp.system.sweepers.heap-pressure-test
  "That the heap-pressure sweep only pays for a Full GC when one is warranted.

   The policy is the whole point of this sweep: `System.gc` is stop-the-world,
   so a sweep that collects too eagerly is worse than no sweep at all. These
   tests therefore pin BOTH directions - that a wasteful heap is collected and
   that a merely large one is left alone - and drive the coordinator's own
   `run-due-sweeps!` for the same reason the async-result tests do: asserting
   registration proves only that a name sits in a map."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.registry :as reg]
            [hive-mcp.system.sweep-coordinator :as sc]
            [hive-mcp.system.sweepers.heap-pressure :as sweeper]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private run-due-sweeps! @#'sc/run-due-sweeps!)
(def ^:private last-run-at @#'sc/last-run-at)
(def ^:private run-counts @#'sc/run-counts)

(def ^:private sweep-name "system/heap-pressure")

(defn- gb [n] (long (* n 1024 1024 1024)))

(defn- isolate-registry
  "Run `f` with ONLY the sweep under test registered, then restore.

   `run-due-sweeps!` walks the global registry, so without this the test would
   also fire the headless watchdog and the async-result reclaim."
  [f]
  (let [saved (vec (reg/registered-sweeps))]
    (doseq [s saved] (reg/unregister-sweep! (lifecycle/sweep-name s)))
    (reset! last-run-at {})
    (reset! run-counts {})
    (try
      (f)
      (finally
        (doseq [s saved] (reg/unregister-sweep! (lifecycle/sweep-name s)))
        (reset! last-run-at {})
        (reset! run-counts {})
        (doseq [s saved] (reg/register-sweep! s))))))

(use-fixtures :each isolate-registry)

(defn- fixed-heap
  "A snapshot-fn returning `committed`/`used` unchanged, for the no-op cases."
  [committed used]
  (fn [] {:committed committed :used used :max (gb 16)}))

(defn- shrinking-heap
  "A snapshot-fn that reports `before` until `collected?` flips, then `after`.

   Stands in for a real Full GC: the sweep's only evidence that collecting
   worked is that the second snapshot committed less than the first."
  [before after collected?]
  (fn [] (if @collected? after before)))

;; ---------------------------------------------------------------- policy

(deftest the-measured-coordinator-heap-is-worth-collecting
  ;; The live shape that motivated this namespace: 8.80 GB committed to hold
  ;; 3.03 GB of live data (pid 3407495, 2026-09-16).
  (is (sweeper/worth-collecting? {:committed 9448928051 :used 3253453619})))

(deftest a-heap-wasting-less-than-the-floor-is-left-alone
  (is (not (sweeper/worth-collecting? {:committed (gb 5) :used (gb 4)}))
      "1 GB of waste does not justify a stop-the-world pause"))

(deftest a-large-heap-that-is-genuinely-full-is-left-alone
  ;; 2 GB of waste clears `min-waste-bytes` on its own. The ratio test is the
  ;; only thing standing between this heap and a pointless Full GC.
  (is (>= (sweeper/reclaimable {:committed (gb 12) :used (gb 10)})
          sweeper/min-waste-bytes)
      "precondition: absolute waste alone would have passed")
  (is (not (sweeper/worth-collecting? {:committed (gb 12) :used (gb 10)}))
      "but collecting a heap that is 83% live would return almost nothing"))

(deftest reclaimable-never-goes-negative
  (is (zero? (sweeper/reclaimable {:committed (gb 1) :used (gb 2)}))
      "used above committed is nonsense, not a negative reclaim")
  (is (zero? (sweeper/reclaimable {}))
      "and a snapshot missing both keys reads as nothing to reclaim"))

;; ---------------------------------------------------------------- sweep!

(deftest a-heap-under-no-pressure-is-not-collected-at-all
  (let [collected? (atom false)
        sweep (sweeper/->HeapPressureSweep (fixed-heap (gb 5) (gb 4))
                                           #(reset! collected? true))
        result (lifecycle/sweep! sweep {})]
    (is (false? @collected?) "System.gc must not be called on a healthy heap")
    (is (= {:swept 0 :errors []} result))))

(deftest a-wasteful-heap-is-collected-and-the-return-reported
  (let [collected? (atom false)
        sweep (sweeper/->HeapPressureSweep
               (shrinking-heap {:committed (gb 9) :used (gb 3)}
                               {:committed (gb 4) :used (gb 3)}
                               collected?)
               #(reset! collected? true))
        result (lifecycle/sweep! sweep {})]
    (is (true? @collected?))
    (is (= 5120 (:swept result)) "5 GB returned, reported in MB")
    (is (= [] (:errors result)))))

(deftest a-collection-that-returns-nothing-is-still-a-clean-sweep
  ;; -XX:+DisableExplicitGC makes System.gc a nop. The sweep logs a warning
  ;; naming it, but must not report a phantom error or a negative reclaim.
  (let [sweep (sweeper/->HeapPressureSweep (fixed-heap (gb 9) (gb 3))
                                           (fn [] nil))
        result (lifecycle/sweep! sweep {})]
    (is (= {:swept 0 :errors []} result))))

(deftest a-throwing-snapshot-is-reported-rather-than-swallowed
  (let [sweep (sweeper/->HeapPressureSweep
               (fn [] (throw (ex-info "no MXBean" {})))
               (fn [] nil))
        {:keys [swept errors]} (lifecycle/sweep! sweep {})]
    (is (zero? swept))
    (is (= 1 (count errors)))
    (is (= sweep-name (:sweep (first errors))))
    (is (re-find #"no MXBean" (:error (first errors)))
        "the message survives, or the report cannot be acted on")))

;; ---------------------------------------------------------------- wiring

(deftest the-coordinator-runs-this-sweep-on-its-own-name-and-interval
  (let [collected? (atom false)]
    (reg/register-sweep!
     (sweeper/->HeapPressureSweep
      (shrinking-heap {:committed (gb 9) :used (gb 3)}
                      {:committed (gb 4) :used (gb 3)}
                      collected?)
      #(reset! collected? true)))
    (testing "one tick fires it"
      (run-due-sweeps!)
      (is (true? @collected?))
      (is (= 1 (get @run-counts sweep-name))))
    (testing "and the interval, not the heartbeat, limits how often"
      (run-due-sweeps!)
      (run-due-sweeps!)
      (is (= 1 (get @run-counts sweep-name))))))

(deftest the-interval-is-slower-than-the-other-sweeps
  ;; Not a style assertion: this interval is also the floor on how often a
  ;; stop-the-world pause can be provoked, so shortening it is a decision
  ;; that has to be made deliberately.
  (is (= 600 (lifecycle/sweep-interval-s (sweeper/->HeapPressureSweep nil nil))))
  (is (> sweeper/sweep-interval-s 300)))
