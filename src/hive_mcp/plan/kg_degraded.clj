(ns hive-mcp.plan.kg-degraded
  "KG-degraded escape hatch for plan-to-kanban.

   Why this module exists
   ----------------------
   Plan-to-kanban creates kanban tasks (load-bearing) AND emits KG edges
   (best-effort enrichment). The KG layer can wedge — write-coalescing
   queue stalls, datalevin conn closed mid-flight, konserve load-order —
   and `with-tx-batch` then blocks the calling thread indefinitely.
   Without an escape hatch, plan-to-kanban hangs forever even though the
   tasks are perfectly creatable. This module provides a bounded,
   degraded-safe wrapper so kanban always lands; edges that miss can be
   backfilled later via `kg edge`.

   Stratified design (CPPB)
   ------------------------
   Layers, top-down:

     Collect   — `kg-call-timeout-ms` resolves the per-call budget from
                 (1) ~/.config/hive-mcp/config.edn :plan.kg-call-timeout-ms
                 (2) HIVE_PLAN_KG_TIMEOUT_MS env (via hive-di defconfig)
                 (3) literal default (5000ms)
     Plan      — `warning-tag` shapes a Result error into the short
                 `kg-<reason>:<label>:<detail>` string the MCP response
                 surfaces.
     Process   — `call-with-timeout` runs one KG thunk with a hard
                 timeout via hive-weave's `safe-future-call`. Always
                 returns a hive-dsl Result.
     Build     — `apply-kg-calls` aggregates a vector of named thunks
                 into `{:edges all-ok-edges :warnings […] :degraded? bool}`.

   Reading direction: `apply-kg-calls` (Build) calls `call-with-timeout`
   (Process) which calls `kg-call-timeout-ms` (Collect); warnings flow
   back up through `warning-tag` (Plan)."
  (:require [hive-di.core :as di]
            [hive-dsl.result :as r]
            [hive-mcp.config.core :as config]
            [hive-weave.safe :as weave-safe]
            [taoensso.timbre :as log]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Collect — Config DI
;; =============================================================================

(di/defconfig PlanKgConfig
  :call-timeout-ms (di/env "HIVE_PLAN_KG_TIMEOUT_MS"
                           :default 5000
                           :type :int
                           :doc "Hard timeout per KG batch call in plan-to-kanban."))

(defn- read-config-edn-override
  "Read :plan.kg-call-timeout-ms from ~/.config/hive-mcp/config.edn.
   Returns int or nil (rescued — config layer must never break us)."
  []
  (try
    (when-let [v (config/get-config-value "plan.kg-call-timeout-ms")]
      (cond (integer? v) v
            (string? v)  (parse-long v)
            :else        nil))
    (catch Throwable _ nil)))

(defn kg-call-timeout-ms
  "Resolve the timeout budget. config.edn wins over env wins over default.
   Pure-ish: reads global config + env, never throws."
  []
  (let [edn-override (read-config-edn-override)
        overrides    (cond-> {} edn-override (assoc :call-timeout-ms edn-override))
        result       (resolve-PlanKgConfig overrides)]
    (or (:call-timeout-ms (:ok result))
        5000)))

;; =============================================================================
;; Plan — Pure shaping
;; =============================================================================

(defn warning-tag
  "Shape a Result error into a short, human-grep-able warning string.
   Pure — no I/O, no side effects."
  [label err-result]
  (let [err-cat (:error err-result)
        data    (dissoc err-result :error)]
    (case err-cat
      :weave/timeout
      (str "kg-timeout:" label ":" (:timeout-ms data) "ms")

      :weave/exception
      (str "kg-error:" label ":" (or (:message data) (:class data) "unknown"))

      ;; Unknown error category — preserve raw shape for debugging.
      (str "kg-degraded:" label ":" (pr-str err-cat)))))

;; =============================================================================
;; Process — Bounded execution
;; =============================================================================

(defn call-with-timeout
  "Run KG thunk `f` with the resolved timeout (or `timeout-ms` when given).
   Returns a hive-dsl Result:
     (r/ok value)  — thunk completed
     (r/err …)     — :weave/timeout or :weave/exception per hive-weave

   The future is cancelled on timeout (hive-weave guarantees this)."
  ([label f] (call-with-timeout label f nil))
  ([label f timeout-ms]
   (weave-safe/safe-future-call
    {:timeout-ms (or timeout-ms (kg-call-timeout-ms)) :name label}
    f)))

(defn- edge-spec-row
  "An edge spec as the response names it: from, to, relation (a string) and
   the call label that tried to write it."
  [label {:keys [from to relation]}]
  {:from from :to to
   :relation (if (keyword? relation) (name relation) (str relation))
   :label label})

;; =============================================================================
;; Build — Aggregation
;; =============================================================================

(defn apply-kg-calls
  "Run a vector of named KG thunks. Aggregates results.

   `calls` is a seq of `[label thunk specs?]` vectors OR maps
   {:label … :thunk … :specs … :timeout-ms …}. `specs` are the edges the
   call writes, each {:from :to :relation}; a call that times out or throws
   names every one of them under :missing-edges, so the caller knows exactly
   which edges to re-link (`kg edge from= to= relation=`).

   Returns:
     {:edges         vector of edge ids (concatenated from ok results, nils dropped)
      :warnings      vector of warning-tag strings (one per degraded call)
      :degraded?     boolean
      :missing-edges [{:from :to :relation :label :reason}] (empty when none)
      :specs         [{:from :to :relation :label}] of the calls that landed}

   Each thunk's value is treated as either a single edge id, a vector of
   edges, or nil; all are flattened into `:edges`. Caller decides whether
   to log; this fn does not log on its own to keep the result pure."
  [calls]
  (let [normalized (map (fn [c]
                          (if (map? c)
                            [(:label c) (:thunk c) (:specs c) (:timeout-ms c)]
                            [(first c) (second c) (nth c 2 nil) nil]))
                        calls)
        results    (mapv (fn [[label thunk specs timeout-ms]]
                           [label (call-with-timeout label thunk timeout-ms) specs])
                         normalized)
        ok-edges   (->> results
                        (keep (fn [[_label res]]
                                (when (r/ok? res) (:ok res))))
                        (mapcat (fn [v]
                                  (cond
                                    (nil? v)        nil
                                    (sequential? v) v
                                    :else           [v])))
                        vec)
        warnings   (->> results
                        (keep (fn [[label res]]
                                (when-not (r/ok? res)
                                  (warning-tag label res))))
                        vec)
        missing    (into []
                         (mapcat (fn [[label res specs]]
                                   (when-not (r/ok? res)
                                     (let [reason (warning-tag label res)]
                                       (map #(assoc (edge-spec-row label %) :reason reason)
                                            specs)))))
                         results)
        landed     (into []
                         (mapcat (fn [[label res specs]]
                                   (when (r/ok? res)
                                     (map #(edge-spec-row label %) specs))))
                         results)]
    {:edges         ok-edges
     :warnings      warnings
     :degraded?     (boolean (seq warnings))
     :missing-edges missing
     :specs         landed}))

(defn with-durability
  "Add to an `apply-kg-calls` result whether its queued edges are readable yet.
   `readable?` takes an edge id and returns truthy once that edge can be read
   back from the store.

   The edges are enqueued in order and the writer flushes in order, so the last
   edge id being readable means all of them are. Polls that id until it reads
   back or the `kg-call-timeout-ms` budget (or `opts` :timeout-ms) runs out. It
   asks about this call's own edges, not about the writer queue: a queue that
   other writers keep busy says nothing about these edges.

   Adds :pending? and :pending-edges to the result:
     false, [] - the edges are readable, or there were none to wait for.
     true, [..] - not confirmed within the budget. The edges are queued and
             land when the writer drains, so the caller must not create them
             again; a `kg-pending:` warning says so, and :pending-edges names
             each queued edge {:from :to :relation :label} (the :specs of the
             calls that landed) so the caller can check exactly those. Not
             degraded: nothing is lost.

   A throwing `readable?` counts as not readable yet. Never throws."
  ([kg-result readable?] (with-durability kg-result readable? nil))
  ([{:keys [edges] :as kg-result} readable? opts]
   (if (empty? edges)
     (assoc kg-result :pending? false :pending-edges [])
     (let [timeout-ms (or (:timeout-ms opts) (kg-call-timeout-ms))
           deadline   (+ (System/currentTimeMillis) timeout-ms)
           last-id    (peek edges)
           readable?  (fn [] (try (boolean (readable? last-id))
                                  (catch Throwable _not-readable-yet false)))
           landed?    (loop []
                        (cond
                          (readable?) true
                          (>= (System/currentTimeMillis) deadline) false
                          :else (do (Thread/sleep 10) (recur))))]
       (if landed?
         (assoc kg-result :pending? false :pending-edges [])
         (-> kg-result
             (assoc :pending? true :pending-edges (vec (:specs kg-result)))
             (update :warnings conj
                     (str "kg-pending:edges-queued:" timeout-ms "ms"))))))))

(defn log-degradation!
  "Side-effecting helper — caller invokes once if `:degraded?` is true."
  [{:keys [warnings]} ctx]
  (log/warn "plan_to_kanban: KG enrichment degraded — tasks created, edges skipped"
            (merge {:warnings warnings} ctx)))

(defn missing-edges-hint
  "How to re-link the edges a degraded call names, or nil when none."
  [missing-edges]
  (when (seq missing-edges)
    (str (count missing-edges) " edge(s) did not land; re-link each with"
         " kg edge from=<from> to=<to> relation=<relation> (see kg-missing-edges)")))

;; =============================================================================
;; Function Contracts (m16 coverage batch)
;; =============================================================================

(m/=> kg-call-timeout-ms
      [:=> [:cat]
       :int])

(m/=> warning-tag
      [:=> [:cat :string :map]
       :string])

(def EdgeRow
  "An edge a KG call tried to write, as the response names it."
  [:map [:from :any] [:to :any] [:relation :string] [:label :string]
   [:reason {:optional true} :string]])

(m/=> call-with-timeout
      [:function
       [:=> [:cat :string [:fn ifn?]] :map]
       [:=> [:cat :string [:fn ifn?] [:maybe pos-int?]] :map]])

(m/=> apply-kg-calls
      [:=> [:cat [:sequential :any]]
       [:map
        [:edges [:vector :any]]
        [:warnings [:vector :string]]
        [:degraded? :boolean]
        [:missing-edges [:vector EdgeRow]]
        [:specs [:vector EdgeRow]]]])

(def ^:private DurabilityIn
  [:map [:edges [:vector :any]] [:warnings [:vector :string]] [:degraded? :boolean]])

(def ^:private DurabilityOut
  [:map
   [:edges [:vector :any]]
   [:warnings [:vector :string]]
   [:degraded? :boolean]
   [:pending? :boolean]
   [:pending-edges [:vector :map]]])

(m/=> with-durability
      [:function
       [:=> [:cat DurabilityIn [:fn ifn?]] DurabilityOut]
       [:=> [:cat DurabilityIn [:fn ifn?] [:maybe [:map [:timeout-ms {:optional true} pos-int?]]]]
        DurabilityOut]])

(m/=> missing-edges-hint [:=> [:cat [:maybe [:sequential :map]]] [:maybe :string]])

(m/=> log-degradation!
      [:=> [:cat :map :map]
       :any])
