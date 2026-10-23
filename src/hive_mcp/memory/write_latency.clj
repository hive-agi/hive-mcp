(ns hive-mcp.memory.write-latency
  "Write-path health for memory add (kanban 20260928204307-01ca0962).

   A bounded last-N ring of per-add stage latencies, kept as a pure value
   (`empty-ring`, `record`, `summarize`, `health`) with one atom at the
   boundary (`ring`, `record!`). `gauges` reads the live embed-gate and
   memory-pool counters. `write-health` folds both into the shape catchup's
   memory-status block carries: status :degraded when p95 add latency is over
   `degraded-p95-ms`.

   Stages are reported canonically: :dedup, :embed, :upsert (index minus the
   embed it contained), :kg, :read-after-write and :total."
  (:require [hive-mcp.embeddings.shared-gate :as embed-gate]
            [hive-mcp.concurrency.pool :as pool]
            [hive-weave.pool :as wpool]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-capacity
  "How many recent adds the ring keeps."
  128)

(def degraded-p95-ms
  "p95 of total add latency above which the write path reports :degraded."
  10000)

(def canonical-stages [:dedup :embed :upsert :kg :read-after-write :total])

(def Sample
  [:map
   [:outcome [:enum :ok :error :timeout]]
   [:stages [:map-of :keyword [:int {:min 0}]]]])

(def Ring
  [:map
   [:capacity pos-int?]
   [:samples [:vector Sample]]])

(def StageStats
  [:map [:n nat-int?] [:p50 nat-int?] [:p95 nat-int?] [:max nat-int?]])

;; ---------------------------------------------------------------------------
;; Pure core
;; ---------------------------------------------------------------------------

(defn empty-ring
  "A ring keeping at most `capacity` samples."
  ([] (empty-ring default-capacity))
  ([capacity] {:capacity capacity :samples []}))

(defn record
  "Pure: `ring` with `sample` appended, the oldest dropped past capacity."
  [{:keys [capacity samples] :as ring} sample]
  (let [v (conj samples sample)
        over (- (count v) capacity)]
    (assoc ring :samples (if (pos? over) (subvec v over) v))))

(defn canonical-sample
  "Pure: fold raw write-stage timings (ms by `run-stage!` stage) plus the
   embed time measured inside them and the add's wall time into a Sample.
   The embed runs inside :index, so :upsert is :index minus :embed."
  [{:keys [timings embed-ms total-ms outcome]}]
  (let [t (or timings {})
        embed (long (or embed-ms 0))
        idx (long (get t :index 0))
        stages (cond-> {:total (long (max 0 (or total-ms 0)))}
                 (or (:dedup-lookup t) (:dedup-update t))
                 (assoc :dedup (long (+ (get t :dedup-lookup 0) (get t :dedup-update 0))))
                 (pos? embed) (assoc :embed embed)
                 (contains? t :index) (assoc :upsert (max 0 (- idx embed)))
                 (or (:kg-edges t) (:link-outgoing t))
                 (assoc :kg (long (+ (get t :kg-edges 0) (get t :link-outgoing 0))))
                 (contains? t :fetch) (assoc :read-after-write (long (:fetch t))))]
    {:outcome (or outcome :ok) :stages stages}))

(defn percentile
  "Nearest-rank percentile `p` (0-100] of `xs`; 0 for an empty seq."
  [xs p]
  (if (empty? xs)
    0
    (let [v (vec (sort xs))
          rank (int (Math/ceil (* (/ p 100.0) (count v))))]
      (nth v (max 0 (dec rank))))))

(defn summarize
  "Pure: {stage {:n :p50 :p95 :max}} over the ring, for every canonical stage
   that has at least one sample, plus :outcomes counts."
  [{:keys [samples]}]
  (let [per (into {}
                  (keep (fn [stage]
                          (let [xs (keep #(get-in % [:stages stage]) samples)]
                            (when (seq xs)
                              [stage {:n (count xs)
                                      :p50 (long (percentile xs 50))
                                      :p95 (long (percentile xs 95))
                                      :max (long (apply max xs))}]))))
                  canonical-stages)]
    {:stages per
     :outcomes (frequencies (map :outcome samples))}))

(defn health
  "Pure: {:status :warnings} for a `summarize` result and a gauges map.
   :degraded when p95 total exceeds `threshold-ms`."
  ([summary gauges] (health summary gauges degraded-p95-ms))
  ([summary gauges threshold-ms]
   (let [p95 (get-in summary [:stages :total :p95] 0)
         slow? (> p95 threshold-ms)]
     {:status   (if slow? :degraded :ok)
      :warnings (if slow?
                  [{:label "memory-write-latency"
                    :status :degraded
                    :message (str "memory add p95 " p95 "ms > " threshold-ms "ms"
                                  " (embed-gate free " (get-in gauges [:embed-gate :free])
                                  ", queued " (get-in gauges [:embed-gate :queued])
                                  "; memory-pool active " (get-in gauges [:memory-pool :active])
                                  ", queued " (get-in gauges [:memory-pool :queued]) ")")}]
                  [])})))

(defn gate-gauge
  "Pure over a gate value: free permits (all lanes) and queued waiters."
  [{:keys [permits interactive-reserve active waiting]}]
  (let [cap (+ (long permits) (long (or interactive-reserve 0)))
        w @waiting]
    {:capacity cap
     :free (max 0 (- cap @active))
     :queued (long (reduce + 0 (vals w)))}))

;; ---------------------------------------------------------------------------
;; Boundary: one atom, live gauges
;; ---------------------------------------------------------------------------

(defonce ^{:doc "The process-wide last-N add latency ring."} ring
  (atom (empty-ring)))

(def ^:dynamic *embed-ms*
  "Bound by a memory add to an atom; the resilient embedder adds the wall time
   of every embed it serves on that add's thread. nil outside an add."
  nil)

(defn add-embed-ms!
  "Credit `ms` of embedding time to the add in flight, if any."
  [ms]
  (when-let [a *embed-ms*] (swap! a + (long ms)))
  nil)

(defn record!
  "Record one add (raw timings, see `canonical-sample`) into `ring-atom`."
  ([raw] (record! ring raw))
  ([ring-atom raw]
   (swap! ring-atom record (canonical-sample raw))
   nil))

(defn gauges
  "Live embed-gate and memory-pool counters."
  []
  {:embed-gate  (gate-gauge embed-gate/process-gate)
   :memory-pool (select-keys (wpool/pool-stats (pool/memory-pool))
                             [:active :queued :pool-size])})

(defn write-health
  "{:status :warnings :write-path {:stages :outcomes :gauges}} for catchup's
   memory-status block."
  ([] (write-health @ring (gauges)))
  ([ring-value gauges-value]
   (let [summary (summarize ring-value)]
     (assoc (health summary gauges-value)
            :write-path (assoc summary :gauges gauges-value)))))

(m/=> empty-ring [:function [:=> [:cat] Ring] [:=> [:cat pos-int?] Ring]])
(m/=> record [:=> [:cat Ring Sample] Ring])
(m/=> canonical-sample [:=> [:cat :map] Sample])
(m/=> percentile [:=> [:cat [:sequential number?] number?] number?])
(m/=> summarize [:=> [:cat Ring] [:map [:stages [:map-of :keyword StageStats]] [:outcomes :map]]])
(m/=> health [:function
              [:=> [:cat :map :map] [:map [:status [:enum :ok :degraded]] [:warnings [:vector :map]]]]
              [:=> [:cat :map :map int?] [:map [:status [:enum :ok :degraded]] [:warnings [:vector :map]]]]])
