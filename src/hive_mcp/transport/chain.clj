(ns hive-mcp.transport.chain
  "TransportChain core: an ordered list of adapters for one capability port,
   with a sticky active adapter, retry/backoff, and an annotated Result.

   Pure half (values in, values out):
     normalize-spec    fill a ChainSpec with the defaults
     init-state        a fresh ChainState, every adapter :unknown
     order             the try-order for a call
     step              fold one adapter outcome into the state
     probe-candidates  adapters ranked above :active that are due a probe
     backoff-delay     the pause before the next attempt

   Effectful half:
     run-chain!        call adapters in order with retry, annotate the Result

   Laws (see plan 20261116115559-0a44db02):
     1. config :chain order is the preference
     2. sticky: the adapter that last worked is :active and is tried first
     3. failure: retry with backoff, mark :degraded, try the next adapter
     4. a recovered adapter ranked above :active takes over (config order wins)
     5. liveness is the recency of :last-ok-at, never a flag
     6. every Result names :via, and says whether a fallback fired
     7. an addressing error (:error/kind :addressing) never falls back

   Result shapes (hive-dsl.result):
     ok  -> {:ok {:value v :via kw :resolved-by :primary|:fallback :skipped [..]}}
     err -> {:error cat :error/kind kind :via kw|nil :skipped [..]}

   :resolved-by is :primary only when the head of the config :chain served
   the call. A sticky non-head adapter serving it is still :fallback: the
   preferred transport is not in use, and that must stay visible."
  (:require [hive-dsl.result :as r]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schemas
;; =============================================================================

(def Retry
  [:map
   [:attempts [:int {:min 1}]]
   [:backoff-ms [:vector [:int {:min 0}]]]])

(def ChainSpec
  [:map
   [:chain [:vector :keyword]]
   [:retry {:optional true} Retry]
   [:probe-interval-ms {:optional true} [:int {:min 0}]]
   [:sticky? {:optional true} :boolean]])

(def AdapterStatus [:enum :ok :degraded :unknown])

(def AdapterState
  [:map
   [:status AdapterStatus]
   [:last-ok-at {:optional true} [:maybe :int]]
   [:last-attempt-at {:optional true} [:maybe :int]]
   [:last-err {:optional true} [:maybe :map]]
   [:fails {:optional true} [:int {:min 0}]]])

(def ChainState
  [:map
   [:active [:maybe :keyword]]
   [:adapters [:map-of :keyword AdapterState]]])

;; =============================================================================
;; Pure core
;; =============================================================================

(def default-spec
  "Defaults merged under every ChainSpec."
  {:retry             {:attempts 3 :backoff-ms [200 1000 5000]}
   :probe-interval-ms 30000
   :sticky?           true})

(defn normalize-spec
  "Fill spec with defaults; a partial :retry keeps the other default key."
  [spec]
  (-> (merge default-spec spec)
      (update :retry #(merge (:retry default-spec) %))
      (update :chain vec)))

(defn init-state
  "A fresh ChainState for spec: nothing active, every adapter :unknown."
  [spec]
  {:active   nil
   :adapters (into {} (map (fn [k] [k {:status :unknown :fails 0}])) (:chain spec))})

(defn- adapter-status
  [state k]
  (get-in state [:adapters k :status] :unknown))

(defn- degraded?
  [state k]
  (= :degraded (adapter-status state k)))

(defn order
  "Try-order for a call: config order skipping :degraded adapters; with
   :sticky? and a healthy :active, the active adapter goes first."
  [state spec]
  (let [{:keys [chain sticky?]} spec
        active  (:active state)
        healthy (vec (remove #(degraded? state %) chain))]
    (if (and sticky? (some #(= active %) healthy))
      (into [active] (remove #(= active %)) healthy)
      healthy)))

(defn addressing?
  "True when outcome is an addressing error (bad or unknown target)."
  [outcome]
  (and (r/err? outcome)
       (= :addressing (:error/kind outcome))))

(defn- failure-entry
  [prev outcome now]
  {:status          :degraded
   :last-ok-at      (:last-ok-at prev)
   :last-attempt-at now
   :last-err        (assoc (select-keys outcome [:error :error/kind :message]) :at now)
   :fails           (inc (or (:fails prev) 0))})

(defn step
  "Fold one adapter outcome (a Result) into the state.
   ok          -> adapter :ok, becomes :active (the sticky law)
   addressing  -> state unchanged: a bad target says nothing about health
   other error -> adapter :degraded; if it was :active, :active is cleared"
  [state adapter outcome now]
  (cond
    (addressing? outcome)
    state

    (r/ok? outcome)
    (-> state
        (assoc :active adapter)
        (assoc-in [:adapters adapter]
                  {:status          :ok
                   :last-ok-at      now
                   :last-attempt-at now
                   :last-err        nil
                   :fails           0}))

    :else
    (cond-> (update-in state [:adapters adapter] failure-entry outcome now)
      (= adapter (:active state)) (assoc :active nil))))

(defn- due?
  [state k interval now]
  (let [{:keys [last-ok-at last-attempt-at]} (get-in state [:adapters k])
        t (when (or last-ok-at last-attempt-at)
            (max (or last-ok-at 0) (or last-attempt-at 0)))]
    (or (nil? t) (>= (- now t) interval))))

(defn probe-candidates
  "Adapters ranked above :active (all of them when nothing is active) whose
   last ok or last attempt is at least :probe-interval-ms old. Config order."
  [state spec now]
  (let [{:keys [chain probe-interval-ms]} (normalize-spec spec)
        active (:active state)
        above  (if (some #(= active %) chain)
                 (take-while #(not= active %) chain)
                 chain)]
    (filterv #(due? state % probe-interval-ms now) above)))

(defn backoff-delay
  "Pause in ms after `failed` failed attempts (1-based) before the next one.
   Past the end of :backoff-ms the last value repeats; empty means 0."
  [retry failed]
  (let [b (:backoff-ms retry)]
    (if (seq b)
      (nth b (min (max 0 (dec failed)) (dec (count b))))
      0)))

(m/=> normalize-spec [:=> [:cat ChainSpec] ChainSpec])
(m/=> init-state [:=> [:cat ChainSpec] ChainState])
(m/=> order [:=> [:cat ChainState ChainSpec] [:vector :keyword]])
(m/=> step [:=> [:cat ChainState :keyword :map :int] ChainState])
(m/=> probe-candidates [:=> [:cat ChainState ChainSpec :int] [:vector :keyword]])
(m/=> backoff-delay [:=> [:cat Retry [:int {:min 1}]] [:int {:min 0}]])

;; =============================================================================
;; Effectful runner
;; =============================================================================

(def ^:private default-clock
  {:now-fn   (fn [] (System/currentTimeMillis))
   :sleep-fn (fn [ms] (when (pos? ms) (Thread/sleep (long ms))))})

(defn- call-adapter
  "Invoke one adapter on op. A throw becomes a :transport/exception error of
   kind :unavailable; a bare (non-Result) return counts as ok."
  [adapter-fn op]
  (try
    (r/ensure-result (adapter-fn op))
    (catch Exception e
      (r/err :transport/exception
             {:error/kind :unavailable
              :message    (ex-message e)}))))

(defn- attempt-adapter
  "Call adapter up to :attempts times, pausing per :backoff-ms between tries.
   Stops at the first ok or addressing error. Returns the last outcome."
  [adapter-fn op retry sleep-fn]
  (let [attempts (:attempts retry)]
    (loop [n 1]
      (let [out (call-adapter adapter-fn op)]
        (if (or (r/ok? out) (addressing? out) (>= n attempts))
          out
          (do (sleep-fn (backoff-delay retry n))
              (recur (inc n))))))))

(defn- resolved-by
  [spec via]
  (if (= via (first (:chain spec))) :primary :fallback))

(defn- annotate-ok
  [spec outcome via skipped]
  (r/ok {:value       (:ok outcome)
         :via         via
         :resolved-by (resolved-by spec via)
         :skipped     skipped}))

(defn- annotate-err
  [outcome via skipped]
  (assoc outcome :via via :skipped skipped))

(defn- exhausted
  [via skipped]
  (r/err :transport/chain-exhausted
         {:error/kind :unavailable
          :via        via
          :skipped    skipped}))

(defn- skip-entry
  [k reason outcome]
  (cond-> {:adapter k :reason reason}
    outcome (assoc :error (select-keys outcome [:error :error/kind :message]))))

(defn- plan
  "[todo skipped]: the adapters to try and the degraded ones left out. When
   every adapter is degraded, try them all in config order as a last resort
   so the chain is never dead until a probe fires."
  [state spec]
  (let [o (order state spec)]
    (if (seq o)
      [o (mapv #(skip-entry % :degraded nil)
               (remove (set o) (:chain spec)))]
      [(:chain spec) []])))

(defn run-chain!
  "Run op through the chain. adapters is {kw (fn [op]) -> Result}; state-atom
   holds a ChainState. Optional opts override :now-fn / :sleep-fn (tests).

   Walks the `order`, retrying each adapter per :retry. The first ok wins and
   is annotated {:value :via :resolved-by :skipped}. An addressing error stops
   the walk at once, no fallback. Every outcome is folded into state-atom
   through `step`. When nothing works: :transport/chain-exhausted."
  ([spec state-atom adapters op]
   (run-chain! spec state-atom adapters op nil))
  ([spec state-atom adapters op opts]
   (let [spec (normalize-spec spec)
         {:keys [now-fn sleep-fn]} (merge default-clock opts)
         [todo pre-skipped] (plan @state-atom spec)]
     (loop [todo todo, skipped pre-skipped, last-k nil]
       (if-let [k (first todo)]
         (if-let [f (get adapters k)]
           (let [out (attempt-adapter f op (:retry spec) sleep-fn)]
             (swap! state-atom step k out (now-fn))
             (cond
               (r/ok? out)       (annotate-ok spec out k skipped)
               (addressing? out) (annotate-err out k skipped)
               :else (recur (rest todo)
                            (conj skipped (skip-entry k :failed out))
                            k)))
           (recur (rest todo) (conj skipped (skip-entry k :unregistered nil)) last-k))
         (exhausted last-k skipped))))))

(m/=> run-chain!
      [:function
       [:=> [:cat ChainSpec :any [:map-of :keyword ifn?] :any] :map]
       [:=> [:cat ChainSpec :any [:map-of :keyword ifn?] :any [:maybe :map]] :map]])
