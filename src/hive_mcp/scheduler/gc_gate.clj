(ns hive-mcp.scheduler.gc-gate
  "The housekeeping explicit-GC gate: hint a full GC only on real fragmentation.

   Housekeeping used to call System/gc on every 5-minute sweep. With
   -XX:-ExplicitGCInvokesConcurrent (deps.edn :jvm-opts) that is a
   stop-the-world Old collection each time: 18 of them took 25.5 s in 83 min,
   the last one 4333 ms. G1PeriodicGCInterval already uncommits idle heap on
   its own, so the explicit hint is kept for the one case periodic GC does not
   fix: committed heap far above the live set.

   The gate fires when committed > min-ratio x used AND committed - used >
   min-gap-bytes (defaults 2x and 1 GB, the condition housekeeping already
   warned on). Below it the sweep reports the reading and collects nothing.

   Ports are injected, so the gate is testable without touching the heap:
     :probe  (fn [] {:committed bytes :used bytes})   default `heap-usage`
     :gc!    (fn [] any)                               default Runtime.gc"
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def defaults
  "Fragmentation thresholds. Both must be exceeded for the gate to open."
  {:min-ratio     2.0
   :min-gap-bytes (* 1024 1024 1024)})

(def HeapUsage
  [:map [:committed nat-int?] [:used nat-int?]])

(def Thresholds
  [:map
   [:min-ratio {:optional true} [:double {:min 1.0 :max 100.0}]]
   [:min-gap-bytes {:optional true} nat-int?]])

(def Ports
  [:map
   [:probe {:optional true} [:=> [:cat] HeapUsage]]
   [:gc! {:optional true} [:=> [:cat] :any]]
   [:thresholds {:optional true} Thresholds]])

(def GcHintResult
  [:map
   [:gc? :boolean]
   [:reason [:enum :fragmented :not-fragmented :probe-failed :gc-failed]]
   [:before-mb nat-int?]
   [:used-mb nat-int?]
   [:committed-mb nat-int?]
   [:freed-mb :int]
   [:committed-freed-mb :int]
   [:still-fragmented? :boolean]])

(defn heap-usage
  "Default probe: the heap's committed and used bytes from the MemoryMXBean."
  []
  (let [u (.getHeapMemoryUsage (java.lang.management.ManagementFactory/getMemoryMXBean))]
    {:committed (.getCommitted u) :used (.getUsed u)}))

(m/=> heap-usage [:=> [:cat] HeapUsage])

(defn fragmented?
  "True when committed heap exceeds min-ratio x used AND the gap exceeds
   min-gap-bytes. A zero used reading is never fragmented (no ratio to take)."
  ([usage] (fragmented? usage defaults))
  ([{:keys [committed used]} thresholds]
   (let [{:keys [min-ratio min-gap-bytes]} (merge defaults thresholds)]
     (boolean (and (pos? used)
                   (> committed (* min-ratio used))
                   (> (- committed used) min-gap-bytes))))))

(m/=> fragmented? [:function
                   [:=> [:cat HeapUsage] :boolean]
                   [:=> [:cat HeapUsage Thresholds] :boolean]])

(defn- mb [b] (Math/round (/ (double b) 1048576.0)))

(defn- report
  [gc? reason before after thresholds]
  {:gc?                gc?
   :reason             reason
   :before-mb          (mb (:used before))
   :used-mb            (mb (:used after))
   :committed-mb       (mb (:committed after))
   :freed-mb           (mb (- (:used before) (:used after)))
   :committed-freed-mb (mb (- (:committed before) (:committed after)))
   :still-fragmented?  (fragmented? after thresholds)})

(def ^:private zero-reading {:committed 0 :used 0})

(defn gc-hint!
  "Read the heap through :probe; call :gc! only when the reading is fragmented,
   then read again. Never throws: a failing probe reports :probe-failed and
   collects nothing, a failing gc! reports :gc-failed."
  ([] (gc-hint! {}))
  ([{:keys [probe gc! thresholds]
     :or   {probe heap-usage
            gc!   #(.gc (Runtime/getRuntime))}}]
   (let [thresholds (merge defaults thresholds)
         before     (try (probe) (catch Exception _ nil))]
     (cond
       (nil? before)
       (report false :probe-failed zero-reading zero-reading thresholds)

       (not (fragmented? before thresholds))
       (report false :not-fragmented before before thresholds)

       :else
       (if (try (gc!) true (catch Exception _ false))
         (report true :fragmented before
                 (or (try (probe) (catch Exception _ nil)) before) thresholds)
         (report false :gc-failed before before thresholds))))))

(m/=> gc-hint! [:function
                [:=> [:cat] GcHintResult]
                [:=> [:cat Ports] GcHintResult]])
