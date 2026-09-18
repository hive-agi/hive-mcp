(ns hive-mcp.system.sweepers.heap-pressure
  "Return committed-but-empty heap to the OS.

   G1 grows the heap under load and, on a busy JVM, never gives it back.
   `-XX:G1PeriodicGCInterval` is the flag that is supposed to prevent this,
   but a periodic GC only fires when NO other GC happened during the
   interval, so the busier the process the less the flag does. The
   coordinator allocates fast enough that young collections run continuously,
   the periodic GC is therefore never triggered, and JEP 346's uncommit
   never engages.

   Measured on the live coordinator (pid 3407495, 2026-09-16): 8.80 GB of
   heap resident to hold 3.03 GB of live data. One explicit Full GC took
   1.07 s and dropped committed from 9.23 GB to 4.19 GB; process RSS went
   12.04 GB -> 7.30 GB. Nothing else in the process was near that size:
   direct byte buffers totalled 1 MB and metaspace 0.86 GB.

   That 4.7 GB is the margin the process was killed for. The kernel OOM
   killer picked this JVM twice on 2026-09-16 (15:40:32 at 15.9 GB anon-rss,
   15:59:05 at 20.3 GB) during a global OOM that also took qdrant and two
   browsers, and a SIGKILL runs no shutdown hook, so every async result
   still sitting in `hive-mcp.channel.async-result/buffers` died with it.
   Shrinking the resident set is the cheapest thing that makes that kill
   less likely.

   The trade is explicit: a Full GC is stop-the-world. It is taken only
   when the heap is holding at least `min-waste-bytes` of nothing AND is
   committed to more than `min-waste-ratio` times its live set, at most
   once per `sweep-interval-s`. A heap that is merely large is left alone."
  (:require [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.registry :as reg]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const sweep-interval-s
  "Seconds between heap-pressure checks.

   Ten minutes, not the five the other sweeps use. The check itself is
   free, but the action it can take is a stop-the-world pause, and heap
   waste accumulates over minutes rather than seconds. This is also the
   floor on how often a Full GC can be provoked."
  600)

(def ^:const min-waste-bytes
  "Committed-but-empty heap, in bytes, below which nothing is done.

   A Full GC that reclaims less than this is not worth its pause. Two
   gigabytes is roughly the point at which the measured 1 s pause buys
   back more than it costs on this process."
  (* 2 1024 1024 1024))

(def ^:const min-waste-ratio
  "How far committed must exceed live before a collection is justified.

   Guards the case where the live set really is large: a 12 GB heap holding
   10 GB of live data wastes 2 GB and would pass `min-waste-bytes` alone,
   but collecting it would return almost nothing. Both tests must pass."
  1.5)

(defn heap-snapshot
  "Current heap figures as `{:committed b :used b :max b}`. IMPURE.

   `Runtime.totalMemory` is what the JVM has committed from the OS and
   `totalMemory - freeMemory` is what is live inside it. The gap between
   them is the quantity this namespace exists to shrink."
  []
  (let [rt (Runtime/getRuntime)
        committed (.totalMemory rt)
        free (.freeMemory rt)]
    {:committed committed
     :used (- committed free)
     :max (.maxMemory rt)}))

(defn reclaimable
  "Bytes of committed heap holding nothing, for `snap`. PURE."
  [{:keys [committed used]}]
  (max 0 (- (or committed 0) (or used 0))))

(defn worth-collecting?
  "True when `snap` justifies paying for a Full GC. PURE.

   Both conditions hold or neither counts: enough absolute waste to be
   worth a pause, and enough waste RELATIVE to the live set that a
   collection can actually return it. Kept pure and public so the policy
   can be tested without provoking a real collection."
  [{:keys [committed used] :as snap}]
  (and (>= (reclaimable snap) min-waste-bytes)
       (> (or committed 0) (* min-waste-ratio (max (or used 0) 1)))))

(defn- mb [bytes] (long (/ bytes 1048576)))

(defrecord HeapPressureSweep [snapshot-fn collect-fn]
  lifecycle/ISweepable
  (sweep-interval-s [_] sweep-interval-s)
  (sweep-name [_] "system/heap-pressure")
  (sweep! [_ _ctx]
    ;; Constructor-injected seams, nil in production, so a test can choose
    ;; what the heap looks like and whether collecting changes it without
    ;; running a real GC (20260726172005-562432b9).
    (let [snapshot (or snapshot-fn heap-snapshot)
          collect (or collect-fn #(System/gc))]
      (try
        (let [before (snapshot)]
          (if-not (worth-collecting? before)
            {:swept 0 :errors []}
            (do
              (collect)
              (let [after (snapshot)
                    returned (- (:committed before) (:committed after))]
                (if (pos? returned)
                  (log/info "heap-pressure: returned" (mb returned) "MB to the OS"
                            {:committed-mb-before (mb (:committed before))
                             :committed-mb-after (mb (:committed after))
                             :used-mb-before (mb (:used before))})
                  ;; A collection that committed nothing back is a
                  ;; misconfiguration, not a quiet no-op: -XX:+DisableExplicitGC
                  ;; turns System.gc into a nop, and this sweep would then pay
                  ;; the check forever and never help.
                  (log/warn "heap-pressure: collection returned nothing;"
                            "-XX:+DisableExplicitGC would explain it"
                            {:committed-mb (mb (:committed after))
                             :used-mb (mb (:used after))}))
                {:swept (max 0 (mb returned)) :errors []}))))
        (catch Throwable t
          ;; Reported rather than swallowed, for the same reason the
          ;; async-result sweep reports: the coordinator's own catch keeps the
          ;; heartbeat alive but discards the outcome, and a silent zero is
          ;; indistinguishable from a sweep that had nothing to do.
          (log/error t "heap-pressure sweep threw")
          {:swept 0 :errors [{:sweep "system/heap-pressure"
                              :error (str (.getName (class t)) ": " (.getMessage t))}]})))))

(def ^:private -registered?
  "Auto-registration. `def`, deliberately NOT `defonce`.

   Registration is keyed by sweep-name and overwrites, so a repeated load
   cannot duplicate this sweep. `defonce` only guaranteed that a hot reload
   would keep running the OLD code, and that an unregister could never be
   undone (kanban 20260916134011-1246379c)."
  (do (reg/register-sweep! (->HeapPressureSweep nil nil)) true))
