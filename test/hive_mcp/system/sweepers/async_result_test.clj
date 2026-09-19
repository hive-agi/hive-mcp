(ns hive-mcp.system.sweepers.async-result-test
  "That the async-result sweep RUNS, not that it is registered.

   Asserting registration proves only that a name is in a map. These tests
   drive the coordinator's own `run-due-sweeps!`, which is the code path that
   decides whether a registered sweep is invoked, so a sweep that registers and
   never fires fails here."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.async-result :as ar]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.registry :as reg]
            [hive-mcp.system.sweep-coordinator :as sc]
            [hive-mcp.system.sweepers.async-result :as sweeper]
            [hive-mcp.channel.async-result-journal :as journal]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private run-due-sweeps! @#'sc/run-due-sweeps!)
(def ^:private last-run-at @#'sc/last-run-at)
(def ^:private run-counts @#'sc/run-counts)

(def ^:private sweep-name "channels/async-result-gc")

(defn- isolate-registry
  "Run `f` with ONLY the sweep under test registered, then put the real
   registry back.

   `run-due-sweeps!` iterates the global registry, so without this the test
   would also fire the headless watchdog and the ling liveness sweep, which
   have side effects well outside this namespace."
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

(defn- with-temp-journal
  "Point the durability journal at a throwaway file.

   The sweep calls the real `gc-expired!`, which now compacts the journal, so
   without this a test run would rewrite the REAL coordinator's journal at
   ~/.config/hive-mcp/data."
  [t]
  (let [p (str (System/getProperty "java.io.tmpdir") "/hive-async-sweep-test-" (random-uuid) ".edn")]
    (binding [journal/*journal-path* p]
      (try (t) (finally (.delete (java.io.File. p)))))))

(use-fixtures :each
  (fn [t] (with-temp-journal #(do (ar/reset-all!) (isolate-registry t) (ar/reset-all!)))))

(defn- register-counting-sweep!
  "Register the sweep with a counting stand-in for the reclaim, and return the
   counter."
  []
  (let [calls (atom 0)]
    (reg/register-sweep! (sweeper/->AsyncResultSweep (fn [] (swap! calls inc) 0)))
    calls))

(deftest a-registered-sweep-actually-runs-when-the-coordinator-ticks
  (let [calls (register-counting-sweep!)]
    (is (zero? @calls) "precondition: registering has not run anything")
    (run-due-sweeps!)
    (is (= 1 @calls) "one tick, one sweep")
    (is (= 1 (get @run-counts sweep-name))
        "and the coordinator recorded the run under this sweep's name")))

(deftest a-sweep-does-not-run-again-before-its-interval-elapses
  (let [calls (register-counting-sweep!)]
    (run-due-sweeps!)
    (run-due-sweeps!)
    (run-due-sweeps!)
    (is (= 1 @calls)
        "the interval is what limits the work, not the heartbeat")))

(deftest a-sweep-becomes-due-again-once-its-interval-has-passed
  (let [calls (register-counting-sweep!)]
    (run-due-sweeps!)
    (is (= 1 @calls))
    ;; Backdate the recorded run rather than sleeping out a 300s interval.
    (swap! last-run-at update sweep-name - (* 1000 (inc sweeper/sweep-interval-s)))
    (run-due-sweeps!)
    (is (= 2 @calls) "due again")))

(deftest a-reclaim-that-throws-is-reported-rather-than-swallowed
  (testing "the sweep itself returns the failure"
    (let [boom (sweeper/->AsyncResultSweep (fn [] (throw (ex-info "boom" {}))))
          {:keys [swept errors]} (lifecycle/sweep! boom {})]
      (is (= 0 swept))
      (is (= 1 (count errors)))
      (is (re-find #"boom" (:error (first errors)))
          "the message survives into the report, so the log is attributable")))

  (testing "and a throwing sweep does not stop the heartbeat"
    (reg/register-sweep! (sweeper/->AsyncResultSweep (fn [] (throw (ex-info "boom" {})))))
    (is (nil? (run-due-sweeps!)) "the tick completes")))

(deftest the-sweep-reclaims-real-entries-when-the-coordinator-ticks
  ;; No injected seam here: the production reclaim runs against real buffers,
  ;; which is the only version of this test that proves the wiring is useful.
  (reg/register-sweep! (sweeper/->AsyncResultSweep nil))
  (ar/enqueue-result! "sweep-test-caller"
                      {:task-id "delivered" :tool "t" :status :ok :result {}})
  (swap! ar/buffers (fn [bufs] (reduce-kv (fn [acc k b] (assoc acc k (assoc b :cursor 1))) {} bufs)))
  (swap! ar/buffers (fn [bufs]
                      (reduce-kv (fn [acc k b]
                                   (assoc acc k (update b :entries
                                                        (fn [es]
                                                          (mapv #(update % :timestamp - 100000) es)))))
                                 {} bufs)))
  (is (seq @ar/buffers) "precondition: there is something to reclaim")
  (run-due-sweeps!)
  (is (empty? @ar/buffers)
      "the delivered entry was reclaimed by the scheduled sweep, not by a drain"))

(deftest the-sweep-schedules-on-the-name-and-interval-the-coordinator-reads
  (let [impl (sweeper/->AsyncResultSweep nil)]
    (is (= sweep-name (lifecycle/sweep-name impl)))
    (is (= sweeper/sweep-interval-s (lifecycle/sweep-interval-s impl)))
    (is (= ar/ttl-seconds sweeper/sweep-interval-s)
        "the cadence tracks the delivered TTL; if that moves, this should move with it")))
