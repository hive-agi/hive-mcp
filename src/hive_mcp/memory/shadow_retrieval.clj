(ns hive-mcp.memory.shadow-retrieval
  "Non-fatal shadow retrieval seam for memory search (kanban
   20260712224939-7039813b, RETRIEVAL-03).

   An addon may register a shadow retriever under the opaque extension key
   `:memory/retrieval` (`hive-mcp.extensions.registry/register!`). The
   retriever is a fn of one ShadowRequest map: the query, the limit, the RAW
   store and ingest candidates captured BEFORE the distance merge, and the ids
   the primary search answered. Whatever it returns is recorded for comparison
   only.

   Guarantees:
   - Default OFF. With nothing registered `dispatch-shadow!` answers :off and
     builds nothing, submits nothing, records nothing: zero behaviour change.
   - Authoritative primary. The shadow receives immutable values and its
     answer is never read by the search path, so it cannot change, reorder or
     delay the returned results. The primary response stays byte-for-byte.
   - Asynchronous and bounded. The shadow runs on its own small :abort pools,
     never on the caller's thread; each call is capped by
     `*shadow-timeout-ms*` (default 500 ms). A saturated pool sheds the call
     (:rejected) instead of queueing without bound.
   - Non-fatal and observable. A throw, a timeout, a rejection, or a failure
     building the request is swallowed and counted; `shadow-stats` answers the
     counters and the most recent comparison records."
  (:require [hive-mcp.extensions.registry :as ext]
            [hive-weave.pool :as wpool]
            [hive-weave.safe :as safe]
            [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schemas
;; =============================================================================

(def ShadowRequest
  "What a shadow retriever receives. Candidates are raw, pre-merge."
  [:map
   [:query [:maybe :string]]
   [:limit nat-int?]
   [:store-candidates [:vector :any]]
   [:ingest-candidates [:vector :any]]
   [:primary-ids [:vector :any]]])

(def Outcome
  "How one shadow call ended."
  [:enum :ok :error :timeout :rejected])

(def DispatchAnswer
  "What `dispatch-shadow!` answers its caller, synchronously."
  [:enum :off :dispatched :rejected :error])

(def ShadowSummary
  "The comparison facts recorded for a shadow answer."
  [:map
   [:shadow-ids [:vector :any]]
   [:overlap nat-int?]])

(def ShadowStats
  "Counters plus the most recent comparison records."
  [:map
   [:counts [:map-of :keyword nat-int?]]
   [:recent [:vector :map]]])

;; =============================================================================
;; State
;; =============================================================================

(def extension-key
  "Extension-registry key a shadow retriever is registered under."
  :memory/retrieval)

(def ^:dynamic *shadow-timeout-ms*
  "Ceiling for one shadow call, in milliseconds."
  500)

(def ^:private recent-cap
  "How many comparison records `shadow-stats` keeps."
  32)

(def ^:private empty-stats
  {:counts {:dispatched 0 :ok 0 :error 0 :timeout 0 :rejected 0}
   :recent []})

(defonce ^:private stats (atom empty-stats))

;; The coordinator pool runs the fire-and-forget task that waits on the shadow;
;; the worker pool runs the shadow itself, so a timed-out worker that ignores
;; interrupts holds a worker thread, never the caller's. Both shed on
;; saturation (:abort) rather than run on the submitting thread.
(defonce ^:private coordinator-pool
  (delay (wpool/make-pool {:name "hive-memory-shadow" :size 2
                           :queue-capacity 16 :rejection :abort})))

(defonce ^:private worker-pool
  (delay (wpool/make-pool {:name "hive-memory-shadow-worker" :size 2
                           :queue-capacity 16 :rejection :abort})))

;; =============================================================================
;; Pure helpers
;; =============================================================================

(defn shadow-request
  "Pure: the ShadowRequest handed to the shadow retriever. Takes one map,
   {:query :limit :store-candidates :ingest-candidates :primary-ids}; every
   collection is realized into a vector so the shadow sees plain values."
  [{:keys [query limit store-candidates ingest-candidates primary-ids]}]
  {:query             (when (string? query) query)
   :limit             (if (nat-int? limit) limit 0)
   :store-candidates  (vec store-candidates)
   :ingest-candidates (vec ingest-candidates)
   :primary-ids       (vec primary-ids)})

(defn summarize-shadow-answer
  "Pure: the comparison facts for one shadow ANSWER against PRIMARY-IDS. An
   answer is a sequence of hits (maps with :id) or a map carrying :results;
   anything else summarizes as no ids. :overlap counts the distinct shadow ids
   the primary also answered."
  [answer primary-ids]
  (let [hits (cond
               (sequential? answer)                                answer
               (and (map? answer) (sequential? (:results answer))) (:results answer)
               :else                                               [])
        ids  (into [] (comp (filter map?) (keep :id)) hits)]
    {:shadow-ids ids
     :overlap    (count (filter (set primary-ids) (distinct ids)))}))

(defn- weave-outcome
  "Map a hive-weave Result from safe-future-call to an Outcome."
  [r]
  (cond
    (contains? r :ok)              :ok
    (= :weave/timeout (:error r))  :timeout
    (= :weave/rejected (:error r)) :rejected
    :else                          :error))

;; =============================================================================
;; Recording
;; =============================================================================

(defn- count! [k]
  (swap! stats update-in [:counts k] (fnil inc 0)))

(defn- record! [rec]
  (swap! stats
         (fn [s]
           (-> s
               (update-in [:counts (:outcome rec)] (fnil inc 0))
               (update :recent (fn [r] (vec (take-last recent-cap (conj r rec)))))))))

(defn shadow-stats
  "Counters and the most recent comparison records of the shadow seam."
  []
  @stats)

(defn reset-shadow-stats!
  "Zero the counters and drop the recent records. For tests and operators."
  []
  (reset! stats empty-stats)
  nil)

;; =============================================================================
;; Dispatch
;; =============================================================================

(defn resolve-shadow-retriever
  "The registered shadow retriever, or nil when the seam is off."
  []
  (ext/get-extension extension-key))

(defn- run-shadow
  "Coordinator task: run SHADOW on the worker pool under TIMEOUT-MS and record
   the outcome. Never throws."
  [shadow request timeout-ms]
  (let [t0 (System/nanoTime)]
    (try
      (let [r       (safe/safe-future-call
                     {:timeout-ms timeout-ms :name "memory-search/shadow"
                      :pool @worker-pool}
                     #(shadow request))
            outcome (weave-outcome r)
            elapsed (quot (- (System/nanoTime) t0) 1000000)
            summary (when (= :ok outcome)
                      (summarize-shadow-answer (:ok r) (:primary-ids request)))]
        (when-not (= :ok outcome)
          (log/debug "memory shadow retrieval" outcome
                     (or (:message r) (:error r))))
        (record! (cond-> {:outcome      outcome
                          :elapsed-ms   elapsed
                          :query        (:query request)
                          :store-count  (count (:store-candidates request))
                          :ingest-count (count (:ingest-candidates request))
                          :primary-ids  (:primary-ids request)}
                   summary (merge summary))))
      (catch Throwable t
        (log/debug "memory shadow retrieval: recording failed" (ex-message t))
        (count! :error)))))

(defn dispatch-shadow!
  "Hand one search's candidates to the registered shadow retriever, without
   waiting for it. Takes the map `shadow-request` takes.

   Answers at once: :off when no retriever is registered (nothing built,
   submitted or counted), :dispatched when the shadow was queued, :rejected
   when the shadow pool is saturated, :error when dispatching itself failed.
   Never throws, and its answer never influences the primary search."
  [ctx]
  (if-let [shadow (resolve-shadow-retriever)]
    (try
      (let [request    (shadow-request ctx)
            timeout-ms *shadow-timeout-ms*]
        (wpool/submit! @coordinator-pool #(run-shadow shadow request timeout-ms))
        (count! :dispatched)
        :dispatched)
      (catch Throwable t
        (if (wpool/rejected? t)
          (do (count! :rejected) :rejected)
          (do (log/debug "memory shadow retrieval: dispatch failed" (ex-message t))
              (count! :error)
              :error))))
    :off))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> shadow-request
      [:=> [:cat [:map
                  [:query {:optional true} [:maybe :string]]
                  [:limit {:optional true} [:maybe nat-int?]]
                  [:store-candidates {:optional true} [:maybe [:sequential :any]]]
                  [:ingest-candidates {:optional true} [:maybe [:sequential :any]]]
                  [:primary-ids {:optional true} [:maybe [:sequential :any]]]]]
       ShadowRequest])

(m/=> summarize-shadow-answer [:=> [:cat :any [:sequential :any]] ShadowSummary])

(m/=> shadow-stats [:=> [:cat] ShadowStats])

(m/=> reset-shadow-stats! [:=> [:cat] :nil])

(m/=> resolve-shadow-retriever [:=> [:cat] :any])

(m/=> dispatch-shadow! [:=> [:cat :map] DispatchAnswer])
