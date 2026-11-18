(ns hive-mcp.await.ling-events
  "Wake bounded context, L2 boundary: the ling-lifecycle listener that closes
   the two-phase park (decision 20261111151358-4b8e5a50).

   create! leaves an awaiter :pending; nothing resumes until `arm!`. This
   namespace is what calls arm!: it listens to every hivemind shout through
   the IDeliveryChannel port (hive-mcp.protocols.delivery-channel), the one
   fanout both an in-process shout and a NATS-inbound shout reach, and acts
   on a ling's TERMINAL shout:

     :parked   hive-agent's park tool ended the run (hive-agent #191). The
               exit shout's :data carries {:termination :parked :awaiter-id
               :run-id :ling-id}; the awaiter is armed with the run id, and a
               wake recorded while it was pending fires now.
     :completed :truncated :error :context-death :aborted
               the ling ended some other way, so nothing will ever arm its
               :pending awaiters: they are cancelled now instead of leaking
               until the pending TTL.

   Only a runtime exit shout counts: it carries :termination in its :data
   and is not :deliberate?. A ling that shouts \"completed\" through the
   hivemind tool mid-run, between creating its awaiter and parking, must not
   cancel the wait it is about to park on.

   Strata: Action and Outcome value objects; `classify` and `owned-pending`
   are pure; `apply-action!` and `handle!` execute against a registry;
   LingEventChannel is the port adapter. A delivery channel has a 500 ms
   budget and a breaker, while arm! persists with an fsync, so the channel
   only classifies on the fanout thread and hands the work to its own
   single-thread executor (which also serialises arms and cancels).
   `install!` registers it; :hive/await-registry calls that after boot."
  (:require [clojure.string :as str]
            [hive-mcp.await.domain :as domain]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.protocols.delivery-channel :as dc]
            [malli.core :as m]
            [taoensso.timbre :as log])
  (:import (java.util.concurrent Executors ExecutorService ThreadFactory)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Vocabulary and value objects
;; =============================================================================

(def listener-id
  "The IDeliveryChannel id this listener registers under."
  :await-ling-events)

(def ending-events
  "Terminal ling events other than :parked: a ling that ends with one of
   these never arms the awaiters it left :pending."
  #{:completed :truncated :error :context-death :aborted})

(def Action
  "What a shout asks of the registry: arm one awaiter for a parked run, or
   cancel every :pending awaiter a ling that ended left behind."
  [:multi {:dispatch :op}
   [:arm [:map {:closed true}
          [:op [:= :arm]]
          [:awaiter-id :string]
          [:run-id [:maybe :string]]
          [:ling-id [:maybe :string]]]]
   [:cancel-owned [:map {:closed true}
                   [:op [:= :cancel-owned]]
                   [:ling-id :string]
                   [:event-type :keyword]]]])

(def Outcome
  "What `apply-action!` did. :arm answers whether THIS call armed the
   awaiter, whether a recorded wake fired, and why it refused when it did;
   :cancel-owned answers the ids it cancelled."
  [:map {:closed true}
   [:op [:enum :arm :cancel-owned]]
   [:awaiter-id {:optional true} :string]
   [:ling-id {:optional true} [:maybe :string]]
   [:armed? {:optional true} :boolean]
   [:fired? {:optional true} :boolean]
   [:refused {:optional true} [:maybe [:enum :unknown-awaiter :other-ling]]]
   [:cancelled {:optional true} [:vector :string]]])

;; =============================================================================
;; Pure
;; =============================================================================

(defn- field
  "`k` of map `m`, read as a keyword key or, for a payload that crossed a
   JSON hop, as its string name."
  [m k]
  (when (map? m)
    (let [v (get m k)]
      (if (some? v) v (get m (name k))))))

(defn- text
  "`v` as a non-blank string (a keyword by its name), else nil."
  [v]
  (when (some? v)
    (let [s (if (keyword? v) (name v) (str v))]
      (when-not (str/blank? s) s))))

(defn- event-kw
  "`v` (keyword or string) as an event keyword, else nil."
  [v]
  (cond (keyword? v) v
        (text v)     (keyword (text v))
        :else        nil))

(defn classify
  "The Action a delivered shout `event` asks for, or nil. Pure.

   `event` is a shout as the delivery fanout hands it: {:agent-id
   :event-type :data {...} :deliberate? ...}, keys keyword or string, the
   event type keyword or string."
  [event]
  (let [etype (event-kw (field event :event-type))
        data  (let [d (field event :data)] (if (map? d) d {}))
        ling  (or (text (field event :agent-id)) (text (field data :ling-id)))]
    (cond
      (not (map? event))                 nil
      (true? (field event :deliberate?)) nil

      (= :parked etype)
      (when-let [aid (text (field data :awaiter-id))]
        {:op :arm :awaiter-id aid :run-id (text (field data :run-id)) :ling-id ling})

      (and (contains? ending-events etype) ling (some? (field data :termination)))
      {:op :cancel-owned :ling-id ling :event-type etype}

      :else nil)))

(m/=> classify [:=> [:cat :any] [:maybe Action]])

(defn owned-pending
  "The :pending awaiters among `awaiters` whose owner is ling `ling-id`.
   Pure."
  [awaiters ling-id]
  (filterv (fn [a] (and (domain/pending? a)
                        (= (str ling-id) (some-> (get-in a [:owner :ling-id]) str))))
           awaiters))

(m/=> owned-pending [:=> [:cat [:sequential domain/Awaiter] :string] [:vector domain/Awaiter]])

;; =============================================================================
;; Executing an action
;; =============================================================================

(defn apply-action!
  "Execute `action` (an Action) against registry `reg`. An :arm for an
   awaiter another ling owns is refused, so a shout can only arm the
   awaiter its own ling created. Answers the Outcome."
  [reg {:keys [op] :as action}]
  (case op
    :arm
    (let [{:keys [awaiter-id run-id ling-id]} action
          a          (registry/lookup reg awaiter-id)
          owner-ling (some-> (get-in a [:owner :ling-id]) str)]
      (cond
        (nil? a)
        {:op :arm :awaiter-id awaiter-id :ling-id ling-id
         :armed? false :fired? false :refused :unknown-awaiter}

        (and owner-ling ling-id (not= owner-ling ling-id))
        {:op :arm :awaiter-id awaiter-id :ling-id ling-id
         :armed? false :fired? false :refused :other-ling}

        :else
        (let [r (registry/arm! reg awaiter-id run-id)]
          {:op :arm :awaiter-id awaiter-id :ling-id ling-id
           :armed? (domain/pending? a) :fired? (boolean (:fired? r))})))

    :cancel-owned
    (let [{:keys [ling-id]} action
          ids (mapv :id (owned-pending (registry/awaiters reg) ling-id))]
      (doseq [id ids] (registry/cancel! reg id))
      {:op :cancel-owned :ling-id ling-id :cancelled ids})))

(m/=> apply-action! [:=> [:cat registry/Registry Action] Outcome])

(defn handle!
  "Classify `event` and execute its action against the registry `reg-fn`
   (zero-arg) answers. Never throws. Answers the Outcome, or nil when the
   event asks nothing of the registry (or the action failed, logged).

   An awaiter this registry does not hold is logged at debug only: with
   several hive-mcp processes on one backbone, every one of them hears a
   :parked shout, and only the one that created the awaiter holds it."
  [reg-fn event]
  (when-let [action (classify event)]
    (try
      (let [out (apply-action! (reg-fn) action)]
        (case (:refused out)
          :unknown-awaiter (log/debug "await: :parked shout for an awaiter this registry does not hold" out)
          :other-ling      (log/warn "await: a :parked shout named an awaiter another ling owns; not armed" out)
          (cond
            (seq (:cancelled out))
            (log/info "await: cancelled the pending awaiters of a ling that ended without parking" out)
            (:armed? out)
            (log/info "await: armed on the :parked event" out)
            :else nil))
        out)
      (catch Throwable t
        (log/warn t "await: ling-event action failed" {:action action})
        nil))))

(m/=> handle! [:=> [:cat [:fn ifn?] :any] [:maybe Outcome]])

;; =============================================================================
;; Port adapter
;; =============================================================================

(defonce ^:private executor
  ;; One daemon thread: arms and cancels run in shout order, off the fanout.
  (delay (Executors/newSingleThreadExecutor
          (reify ThreadFactory
            (newThread [_ r]
              (doto (Thread. ^Runnable r "hive-await-ling-events")
                (.setDaemon true)))))))

(defn- submit-async!
  "Run `f` on the listener's own executor."
  [f]
  (.execute ^ExecutorService @executor ^Runnable f))

(defrecord LingEventChannel [reg-fn submit-fn]
  dc/IDeliveryChannel
  (channel-id [_this] listener-id)
  (available? [_this] true)
  (deliver! [_this event]
    ;; Classify on the fanout thread (pure, cheap) so only the shouts this
    ;; listener acts on cost an executor hop.
    (when (classify event)
      (submit-fn (fn [] (handle! reg-fn event))))
    nil))

(defn listener-channel
  "The listener as an IDeliveryChannel. `reg-fn` answers the registry (the
   process registry by default) and `submit-fn` runs a zero-arg fn (the
   listener's own executor by default; a test passes one that runs it
   inline)."
  ([] (listener-channel registry/default-registry submit-async!))
  ([reg-fn submit-fn] (->LingEventChannel reg-fn submit-fn)))

(m/=> listener-channel [:function
                        [:=> [:cat] dc/Channel]
                        [:=> [:cat [:fn ifn?] [:fn ifn?]] dc/Channel]])

(defn install!
  "Register the listener (default: over the process registry) with the
   delivery-channel registry, replacing any earlier one. Answers its id."
  ([] (install! (listener-channel)))
  ([channel]
   (dc/register-channel! channel)
   listener-id))

(m/=> install! [:function
                [:=> [:cat] [:= :await-ling-events]]
                [:=> [:cat dc/Channel] [:= :await-ling-events]]])

(defn uninstall!
  "Remove the listener from the delivery-channel registry. Answers
   :uninstalled."
  []
  (dc/unregister-channel! listener-id)
  :uninstalled)

(m/=> uninstall! [:=> [:cat] [:= :uninstalled]])
