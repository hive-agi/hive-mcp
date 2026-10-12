(ns hive-mcp.embeddings.resilient
  "Bounded, failover EmbeddingProvider decorator over a same-dimension provider
   chain. Satisfies EmbeddingProvider; callers cannot distinguish it from a raw
   provider."
  (:require [hive-mcp.embeddings.protocol :as proto]
            [hive-dsl.result :as r]
            [hive-weave.safe :as safe]
            [taoensso.timbre :as log]
            [hive-mcp.embeddings.deadline :as dl]
            [hive-dsl.adt :as adt]
            [clojure.string :as str]
            [hive-mcp.embeddings.shared-gate :as shared]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-budget-ms
  "Per-attempt wall-clock budget (ms). Capped by whatever the deadline has left."
  12000)

(def default-total-budget-ms
  "Wall-clock budget for the WHOLE chain — permit wait included.

   MUST stay strictly below the caller's write budget. A chain that can outlive
   its caller cannot report failure: the caller times out first, and the entry
   it was embedding is silently dropped."
  20000)

(def default-cold-budget-ms
  "Per-attempt budget for a provider not known warm in this process (never
   answered yet, or its last attempt timed out): that call may pay a model
   load. Capped by whatever the deadline has left."
  18000)

(def default-permits
  "Shared gate capacity; all embedding providers use one permit at a time."
  1)

(defn- describe
  "Log label for a chain entry."
  [{:keys [provider-key provider]}]
  (or provider-key
      (some-> provider class .getSimpleName)
      :unknown))

(adt/defadt AttemptOutcome
  "What one provider attempt can yield. Closed, so the chain loop cannot
   silently forget a case."
  [:attempt/ok     {:value any?}]
  [:attempt/failed {:provider any? :error any? :message any?}])

(defonce ^{:doc "Process-wide set of provider identities ([provider-key model])
   that answered within budget and have not timed out since. Shared across
   embedders because the host builds a fresh ResilientEmbedder per write."}
  warm-providers
  (atom #{}))

(def default-cooldown-ms
  "How long a failed provider is skipped before one half-open probe."
  30000)

(defonce ^{:doc "Process-wide per-[provider-key model] circuit state. Open entries
   carry :until (monotonic nanoseconds); :probing reserves the sole half-open
   attempt. Shared because callers construct new embedders per write."}
  provider-health
  (atom {}))

(defn- model-of
  "The model a chain entry's provider embeds with, when it says. Unwrap the
   shared admission gate so boot probes and write attempts share an identity."
  [{:keys [provider]}]
  (let [provider (shared/ungated provider)]
    (when (map? provider) (:model provider))))

(defn- identity-of
  "Warmth key for a chain entry: provider-key plus model."
  [entry]
  [(describe entry) (model-of entry)])

(defn- claim-provider!
  "Admit a closed provider, or reserve the sole half-open probe after cooldown.
   CAS makes the reservation atomic across independently constructed embedders."
  [health id now]
  (loop []
    (let [old @health
          {:keys [until probing]} (get old id)]
      (cond
        probing false
        (nil? until) true
        (< now until) false
        (compare-and-set! health old (assoc old id {:probing true})) true
        :else (recur)))))

(defn- record-health!
  "A completed provider attempt closes on success and opens on failure.
   Gate denial is not an attempt and never reaches this function."
  [health id now cooldown-ms success?]
  (if success?
    (swap! health dissoc id)
    (swap! health assoc id {:until (+ now (* 1000000 (long cooldown-ms)))})))

(defn- attempt-budget
  "Per-attempt budget for `entry`: `budget-ms` when known warm, else the
   larger `cold-budget-ms`, since a cold model may still be loading."
  [warmth entry budget-ms cold-budget-ms]
  (if (contains? @warmth (identity-of entry))
    budget-ms
    (max budget-ms cold-budget-ms)))

(defn- attempt
  "One bounded call against `entry`'s provider. `call` is (fn [provider] -> x).
   `budget-ms` is already capped by the deadline. Success marks the provider
   warm in `warmth`; a timeout marks it cold again (it may have been evicted)."
  [warmth entry call budget-ms]
  (let [pk  (describe entry)
        id  (identity-of entry)
        t0  (System/nanoTime)
        res (safe/safe-future-call
             {:timeout-ms budget-ms :name (str "embed:" pk)}
             #(call (:provider entry)))
        ms  (long (/ (- (System/nanoTime) t0) 1e6))]
    (if (r/ok? res)
      (do (swap! warmth conj id)
          (attempt-outcome :attempt/ok {:value (:ok res)}))
      (do (when (= :weave/timeout (:error res))
            (swap! warmth disj id))
          (log/warn "embedding provider" pk "model" (model-of entry)
                    "failed after" ms "ms (budget" budget-ms "ms):"
                    (:error res) (or (:message res) ""))
          (attempt-outcome :attempt/failed {:provider   pk
                                            :model      (model-of entry)
                                            :elapsed-ms ms
                                            :budget-ms  budget-ms
                                            :error      (:error res)
                                            :message    (:message res)})))))

(defn- failures-summary
  "One human line per failed attempt: provider, model, elapsed vs budget, cause."
  [failures]
  (->> failures
       (map (fn [{:keys [provider model elapsed-ms budget-ms error message]}]
              (str provider (when model (str " (model " model ")"))
                   " " error " after " elapsed-ms "ms of " budget-ms "ms"
                   (when message (str ": " message)))))
       (str/join "; ")))

(defn- admitted-attempt
  "One attempt that first waits for `entry`'s shared-gate permit, but only for
   what the deadline has left. Returns ::not-admitted when the wait runs out:
   a provider never admitted was never TRIED, so it is reported as untried,
   not as a failure.

   The permit is held by the worker running the provider call and returned
   when that call actually ends. A timed-out attempt therefore keeps its
   permit until the abandoned HTTP call returns, so a cancelled request cannot
   free a slot the provider is still busy serving (the retry storm of
   friction 20261003172907-4abbcf92). A worker cancelled before it started
   returns the permit here."
  [warmth entry call dl per]
  (let [provider (:provider entry)
        gate     (shared/gate-of provider)]
    (if-not (shared/try-acquire! gate shared/*lane* (dl/remaining-ms dl))
      ::not-admitted
      (let [released (atom false)
            started  (atom false)
            release  #(when (compare-and-set! released false true)
                        (shared/release! gate))
            outcome  (try
                       (attempt warmth (assoc entry :provider (shared/ungated provider))
                                (fn [p]
                                  (reset! started true)
                                  (try (call p) (finally (release))))
                                (dl/attempt-budget-ms dl per))
                       (catch Throwable t
                         (when-not @started (release))
                         (throw t)))]
        (when-not @started (release))
        outcome))))

(defn- run-chain
  "Try each provider in `chain`, bounded by ONE deadline.

   The deadline starts before the first permit wait, so a slow queue eats the
   same budget a slow provider would. Each attempt waits on the shared gate
   (embeddings.shared-gate, the caller's *lane*) only for what is left, then
   may spend only what is left, so the chain always returns, successfully or
   with an exception, inside `total-budget-ms`. A provider not known warm gets
   `cold-budget-ms` for its attempt instead of `budget-ms`; a warm provider
   that times out is retried once on the cold budget before the chain moves on.

   Returns the first success value; throws ex-info :embedder/chain-exhausted
   (:exhausted-by :providers | :deadline) when none succeeds. The message names
   every provider tried, its model and the elapsed time, and :untried names
   every provider never admitted or never reached."
  [{:keys [chain budget-ms cold-budget-ms total-budget-ms warmth health cooldown-ms clock]} call]
  (let [t0 (System/nanoTime)
        elapsed #(long (/ (- (System/nanoTime) t0) 1e6))
        dl (dl/deadline total-budget-ms)
        exhausted (fn [untried failures]
                    (let [ms (elapsed)]
                      (ex-info (str "Embedding chain ran out of time after " ms
                                    "ms (total budget " total-budget-ms "ms)"
                                    (when (seq failures)
                                      (str " — " (failures-summary failures)))
                                    (when (seq untried)
                                      (str " — never tried: "
                                           (str/join ", " (map describe untried)))))
                               {:error :embedder/chain-exhausted
                                :exhausted-by :deadline
                                :elapsed-ms ms
                                :total-budget-ms total-budget-ms
                                :untried (mapv describe untried)
                                :failures failures})))]
    (loop [[entry & more] chain failures [] skipped [] forced? false]
      (cond
        ;; Every provider was skipped by an open circuit. A call never fails
        ;; without one real attempt, so a single-provider chain still embeds
        ;; after a transient failure: retry the skipped entries, circuit bypassed.
        (and (nil? entry) (empty? failures) (seq skipped) (not forced?)
             (not (dl/expired? dl)))
        (recur skipped [] [] true)

        (and (nil? entry) (not (dl/expired? dl)))
        (let [ms (elapsed)]
          (throw (ex-info (str "All embedding providers in the chain failed after "
                               ms "ms — " (failures-summary failures))
                          {:error :embedder/chain-exhausted
                           :exhausted-by :providers
                           :elapsed-ms ms
                           :failures failures
                           :untried skipped})))
        (dl/expired? dl)
        (throw (exhausted (concat skipped (when entry (cons entry more))) failures))
        :else
        (let [id (identity-of entry)]
          (if-not (or forced? (claim-provider! health id (clock)))
            (recur more failures (conj skipped entry) forced?)
            (let [was-warm (contains? @warmth id)
                  per (attempt-budget warmth entry budget-ms cold-budget-ms)
                  outcome (try (admitted-attempt warmth entry call dl per)
                               (catch Throwable t
                                 (record-health! health id (clock) cooldown-ms false)
                                 (throw t)))]
              (if (= ::not-admitted outcome)
                (do (swap! health #(if (get-in % [id :probing]) (dissoc % id) %))
                    (throw (exhausted (concat skipped (cons entry more)) failures)))
                (adt/adt-case AttemptOutcome outcome
                              :attempt/ok (do (record-health! health id (clock) cooldown-ms true)
                                              (:value outcome))
                              :attempt/failed (let [failures (conj failures
                                                                   (select-keys outcome
                                                                                [:provider :model :elapsed-ms
                                                                                 :budget-ms :error :message]))
                                                    retry? (and was-warm
                                                                (= :weave/timeout (:error outcome))
                                                                (not (get-in @health [id :probing])))]
                                                (when-not retry?
                                                  (record-health! health id (clock) cooldown-ms false))
                                                (recur (if retry? (cons entry more) more)
                                                       failures skipped forced?)))))))))))

(defrecord ResilientEmbedder [chain budget-ms total-budget-ms cold-budget-ms warmth health cooldown-ms clock]
  shared/AdmissionOwner
  proto/EmbeddingProvider
  (embed-text [this text]
    (run-chain this (fn [p] (proto/embed-text p text))))
  (embed-batch [this texts]
    (run-chain this (fn [p] (proto/embed-batch p texts))))
  (embedding-dimension [_]
    (proto/embedding-dimension (:provider (first chain)))))

(defn resilient-embedder
  "Wrap an ordered, primary-first chain of resolved providers
   ({:provider EmbeddingProvider, :provider-key kw?} …) in a bounded failover
   EmbeddingProvider.

   `total-budget-ms` bounds the whole chain; `budget-ms` bounds one attempt on
   a provider known warm. The options map may also carry `:cold-budget-ms`
   (attempt budget for a provider not known warm; defaults to
   `default-cold-budget-ms`, or `budget-ms` in the positional arities) and
   `:warmth` (an atom of warm provider identities; defaults to the process-wide
   `warm-providers`). Throws on an empty chain. `:health` stores per-identity
   circuit state, `:clock` returns monotonic nanoseconds, and `:cooldown-ms`
   controls when a failed provider is eligible for a single half-open probe."
  ([chain] (resilient-embedder chain {}))
  ([chain budget-ms-or-opts]
   (if (map? budget-ms-or-opts)
     (let [{:keys [budget-ms total-budget-ms cold-budget-ms warmth health cooldown-ms clock]
            :or   {budget-ms       default-budget-ms
                   total-budget-ms default-total-budget-ms
                   cold-budget-ms  default-cold-budget-ms
                   warmth          warm-providers
                   health          provider-health
                   cooldown-ms     default-cooldown-ms
                   clock           #(System/nanoTime)}} budget-ms-or-opts]
       (when (empty? chain)
         (throw (ex-info "resilient-embedder: empty provider chain" {})))
       (->ResilientEmbedder (vec chain) budget-ms total-budget-ms cold-budget-ms
                            warmth health cooldown-ms clock))
     (resilient-embedder chain budget-ms-or-opts default-total-budget-ms)))
  ([chain budget-ms total-budget-ms]
   (resilient-embedder chain {:budget-ms       budget-ms
                              :total-budget-ms total-budget-ms
                              :cold-budget-ms  budget-ms})))