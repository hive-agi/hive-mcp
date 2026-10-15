(ns hive-mcp.concurrency.pool
  "Central bounded thread-pool registry for hive-mcp.

   Thin registry layer: the underlying bounded ThreadPoolExecutor
   construction, safe submit/await, and lifecycle all live in
   `hive-weave.pool`. This namespace holds the *named instances* of
   those pools so the rest of hive-mcp can resolve a pool by role.

   Pools:
   1. :io-pool     — blocking I/O (HTTP, file, DB, embeddings)
                      Size: (+ 2 (* 2 available-processors))
   2. :compute-pool — CPU-bound work (KG traversal, compression)
                      Size: available-processors
   3. :event-pool   — fire-and-forget event dispatch
                      Size: 4
   4. :memory-pool  — memory tool IO (Chroma HTTP, KG mutations, plan
                      indexing). Isolated from io-pool so slow memory
                      calls cannot starve other IO consumers.
                      Size: (max 4 available-processors)

   All pools use bounded queues with CallerRunsPolicy backpressure
   (see `hive-weave.pool/make-pool`).

   Client code depends on `hive-weave.pool` primitives (submit!,
   await!, with-pool-await) and passes one of the named pool instances
   below — it should not reach into `java.util.concurrent` directly."
  (:require [hive-weave.pool :as wpool]
            [malli.core :as m]
            [clojure.test.check.generators :as gen]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Pool Sizes
;; =============================================================================

(def ^:private available-processors
  (.availableProcessors (Runtime/getRuntime)))

(def ^:private io-pool-size
  (+ 2 (* 2 available-processors)))

(def ^:private compute-pool-size
  available-processors)

(def ^:private event-pool-size
  4)

(def ^:private memory-pool-size
  (max 4 available-processors))

(def ^:private catchup-pool-size
  4)

;; =============================================================================
;; Pool Instances (lazy init via delay)
;; =============================================================================

(defonce ^:private io-pool-delay
  (delay (wpool/make-pool {:name "hive-io" :size io-pool-size})))

(defonce ^:private compute-pool-delay
  (delay (wpool/make-pool {:name "hive-compute" :size compute-pool-size})))

(defonce ^:private event-pool-delay
  (delay (wpool/make-pool {:name "hive-event" :size event-pool-size})))

(defonce ^:private memory-pool-delay
  (delay (wpool/make-pool {:name "hive-memory" :size memory-pool-size})))

(defonce ^:private catchup-pool-delay
  (delay (wpool/make-pool {:name "hive-catchup" :size catchup-pool-size})))

(defn io-pool       [] @io-pool-delay)

(m/=> io-pool [:=> [:cat] [:fn {:gen/gen (gen/return (io-pool))} #(instance? java.util.concurrent.ThreadPoolExecutor %)]])
(defn compute-pool  [] @compute-pool-delay)

(m/=> compute-pool [:=> [:cat] [:fn {:gen/gen (gen/return (compute-pool))} #(instance? java.util.concurrent.ThreadPoolExecutor %)]])
(defn event-pool    [] @event-pool-delay)

(m/=> event-pool [:=> [:cat] [:fn {:gen/gen (gen/return (event-pool))} #(instance? java.util.concurrent.ThreadPoolExecutor %)]])
(defn memory-pool   [] @memory-pool-delay)

(m/=> memory-pool [:=> [:cat] [:fn {:gen/gen (gen/return (memory-pool))} #(instance? java.util.concurrent.ThreadPoolExecutor %)]])
(defn catchup-pool  [] @catchup-pool-delay)

(m/=> catchup-pool [:=> [:cat] [:fn {:gen/gen (gen/return (catchup-pool))} #(instance? java.util.concurrent.ThreadPoolExecutor %)]])

;; =============================================================================
;; Submit API (thin delegation; prefer hive-weave.pool directly)
;; =============================================================================

(defn submit-io!
  "Submit a blocking I/O task to the IO pool."
  [f]
  (wpool/submit! (io-pool) f))

(m/=> submit-io! [:=> [:cat ifn?] [:fn {:gen/gen (gen/return (future nil))} #(instance? java.util.concurrent.Future %) ]])

(defn submit-compute!
  "Submit a CPU-bound task to the compute pool."
  [f]
  (wpool/submit! (compute-pool) f))

(m/=> submit-compute! [:=> [:cat ifn?] [:fn {:gen/gen (gen/return (future nil))} #(instance? java.util.concurrent.Future %) ]])

(defn submit-event!
  "Submit a fire-and-forget event-dispatch task."
  [f]
  (wpool/submit! (event-pool) f))

(m/=> submit-event! [:=> [:cat ifn?] [:fn {:gen/gen (gen/return (future nil))} #(instance? java.util.concurrent.Future %)]])

(defn submit-memory!
  "Submit a memory tool IO task (Chroma HTTP, KG mutation, plan indexing)."
  [f]
  (wpool/submit! (memory-pool) f))

(m/=> submit-memory! [:=> [:cat ifn?] [:fn {:gen/gen (gen/return (future nil))} #(instance? java.util.concurrent.Future %)]])

(defn submit-catchup!
  "Submit a catchup-branch task (parallel Chroma/KG sub-queries in catchup)."
  [f]
  (wpool/submit! (catchup-pool) f))

(m/=> submit-catchup! [:=> [:cat ifn?] [:fn {:gen/gen (gen/return (future nil))} #(instance? java.util.concurrent.Future %)]])

;; =============================================================================
;; DSL Macros — sugar that matches the hive-weave.pool primitives
;; =============================================================================

(defmacro with-io        [& body] `(submit-io!      (fn [] ~@body)))
(defmacro with-compute   [& body] `(submit-compute! (fn [] ~@body)))
(defmacro with-event     [& body] `(submit-event!   (fn [] ~@body)))
(defmacro with-memory    [& body] `(submit-memory!  (fn [] ~@body)))
(defmacro with-catchup   [& body] `(submit-catchup! (fn [] ~@body)))

(defmacro with-solo
  "Submit body to Clojure's solo executor (unbounded cached thread pool).
   Use for coordinator tasks that internally spawn bounded-pool work —
   avoids nested-pool contention where a bounded-pool thread blocks
   while spawning more work on the same pool."
  [& body]
  `(future ~@body))

;; =============================================================================
;; Monitoring
;; =============================================================================

(defn pool-stats
  "Snapshot of every named pool."
  []
  {:io      (wpool/pool-stats (io-pool))
   :compute (wpool/pool-stats (compute-pool))
   :event   (wpool/pool-stats (event-pool))
   :memory  (wpool/pool-stats (memory-pool))
   :catchup (wpool/pool-stats (catchup-pool))})

(m/=> pool-stats [:=> [:cat] [:map [:io :map] [:compute :map] [:event :map] [:memory :map] [:catchup :map]]])

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn shutdown-pools!
  "Orderly shutdown of all realized pools. Waits up to 5 seconds per
   pool, then force-shuts. Call during JVM shutdown or REPL teardown."
  []
  (doseq [pool-delay [io-pool-delay compute-pool-delay
                      event-pool-delay memory-pool-delay
                      catchup-pool-delay]]
    (when (realized? pool-delay)
      (wpool/shutdown! @pool-delay {:await-ms 5000}))))

(m/=> shutdown-pools! [:=> [:cat] :nil])
