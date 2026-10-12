(ns hive-mcp.server.log-sink-test
  "Pins the console log sink: admission is bounded and never blocks, and a
   blocked writer cannot stall the logging caller.

   The writer is a stub LineWriter injected through the port; nothing here
   touches *err*, Timbre's global config or a running server.

   Mutants are self-contained and never call the subject var, which the
   trifecta has rebound to the mutant while it runs them."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.server.log-sink :as sink]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Stubs
;; =============================================================================

(defrecord NullWriter []
  sink/LineWriter
  (write-line! [_ _] nil))

(defrecord BlockedWriter [gate entered written]
  sink/LineWriter
  (write-line! [_ line]
    (deliver entered true)
    @gate
    (swap! written conj line)))

(defn- blocked-writer []
  (->BlockedWriter (promise) (promise) (atom [])))

;; =============================================================================
;; Admission — golden + property + mutation
;; =============================================================================

(defn run-admission
  "Unary adapter: {:capacity n :lines [s]} -> admission summary, over a sink
   with no drain thread so the outcome is deterministic."
  [{:keys [capacity lines]}]
  (let [s (sink/make-sink (->NullWriter) {:capacity capacity :start? false})
        results (mapv #(sink/offer! s %) lines)]
    {:queued  (count (filter #{:queued} results))
     :dropped (sink/dropped s)
     :depth   (sink/depth s)
     :results results}))

(defn- admission-holds?
  [{:keys [queued dropped depth results]}]
  (and (= (count results) (+ queued dropped))
       (= queued depth)
       ;; drops only ever follow a full queue: no :queued after a :dropped
       (let [[_ tail] (split-with #{:queued} results)]
         (every? #{:dropped} tail))))

(deftrifecta admission-contract
  hive-mcp.server.log-sink-test/run-admission
  {:golden-path "test/golden/server/log-sink-admission.edn"
   :cases       {:empty        {:capacity 2 :lines []}
                 :under        {:capacity 3 :lines ["a" "b"]}
                 :exactly-full {:capacity 2 :lines ["a" "b"]}
                 :overflow     {:capacity 2 :lines ["a" "b" "c" "d" "e"]}}
   :gen         (gen/let [cap (gen/choose 1 8)
                          lines (gen/vector gen/string-alphanumeric 0 20)]
                  {:capacity cap :lines lines})
   :pred        (fn [out] (admission-holds? out))
   :num-tests   200
   :mutations   [["unbounded — queues everything, the old synchronous shape"
                  (fn [{:keys [lines]}]
                    {:queued (count lines) :dropped 0 :depth (count lines)
                     :results (mapv (constantly :queued) lines)})]
                 ["drops-everything"
                  (fn [{:keys [lines]}]
                    {:queued 0 :dropped (count lines) :depth 0
                     :results (mapv (constantly :dropped) lines)})]
                 ["uncounted-drops"
                  (fn [{:keys [capacity lines]}]
                    (let [q (min capacity (count lines))]
                      {:queued q :dropped 0 :depth q
                       :results (vec (concat (repeat q :queued)
                                             (repeat (- (count lines) q) :dropped)))}))]]
   :assert      (fn []
                  (let [out (run-admission {:capacity 2 :lines ["a" "b" "c"]})]
                    (is (= [:queued :queued :dropped] (:results out)))
                    (is (= 1 (:dropped out)))
                    (is (= 2 (:depth out)))))})

;; =============================================================================
;; Backpressure — a blocked writer never blocks the logging caller
;; =============================================================================

(deftest blocked-writer-does-not-block-callers
  (let [w (blocked-writer)
        s (sink/make-sink w {:capacity 16})
        log! (sink/appender-fn s)
        n 10000]
    (try
      (log! {:output_ (delay "first")})
      (is (true? (deref (:entered w) 2000 false))
          "the drain thread is now stuck inside the writer")
      (let [t0 (System/nanoTime)]
        (dotimes [i n] (log! {:output_ (delay (str "line " i))}))
        (let [ms (/ (- (System/nanoTime) t0) 1e6)]
          (is (< ms 2000.0)
              (str n " log calls must return while the writer is blocked; took " ms " ms"))))
      (testing "overflow is dropped and counted, the queue stays bounded"
        (is (<= (sink/depth s) 16))
        (is (>= (sink/dropped s) (- n 16))))
      (finally
        (deliver (:gate w) true)
        (sink/stop! s)))
    (testing "once released, the writer resumes draining"
      (let [deadline (+ (System/currentTimeMillis) 2000)]
        (while (and (empty? @(:written w))
                    (< (System/currentTimeMillis) deadline))
          (Thread/sleep 10)))
      (is (some #{"first"} @(:written w))))))
