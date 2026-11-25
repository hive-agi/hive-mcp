(ns hive-mcp.await.domain
  "Wake bounded context, L0 values and L1 calculations: an Awaiter waits on
   one Subscription until a matching event or its deadline wakes it.

   Pure: no I/O, no atoms, no clock. The caller passes `now` (epoch ms).
   A deadline is a wake like any other, with :reason :timeout, so the one
   step function decides every wake and the boundary only executes it.

   Two-phase park (decision 20261111151358-4b8e5a50): an awaiter is created
   :pending, because the lingering ling does not know its chat-point run id
   before it parks. While :pending the source subscribes, but a matching
   event or a passed deadline is only RECORDED (:pending-wake, first one
   wins, deduped); no owner is resumed. `arm` promotes :pending -> :armed on
   the :parked event and hands the recorded wake back, so a wake can never
   race a run that is still turning.

   Lifecycle: :pending -> :armed -> :waking -> :woken | :cancelled.
   Only :armed wakes, and :waking is the in-flight resume (at-least-once:
   a failed or deferred resume stays :waking and is re-driven)."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; L0 - Vocabulary and schemas
;; =============================================================================

(def statuses
  "Every status an awaiter may have, in lifecycle order."
  [:pending :armed :waking :woken :cancelled])

(def live-statuses
  "The statuses in which an awaiter still holds a source: it can still wake."
  #{:pending :armed :waking})

(def Subscription
  "What an awaiter listens for. :type names the source (open set, see
   hive-mcp.await.ports/open-source). :filter is a map every key of which
   must equal the event's :data value at that key. :dedup-key names the
   event :data key whose value identifies an event: an awaiter never wakes
   twice on the same value."
  [:map
   [:type :keyword]
   [:filter {:optional true} [:maybe [:map-of :keyword :any]]]
   [:dedup-key {:optional true} [:maybe :keyword]]])

(def Owner
  "Who resumes on a wake. :kind names the resumer (open set, see
   hive-mcp.await.ports/open-resumer); the other keys are the resumer's.
   A :hive-agent-run owner has no :run-id until `arm` binds it."
  [:map [:kind :keyword]])

(def Awaiter
  "One parked wait. :deadline is epoch ms or nil (wait forever). :prompt is
   the continuation the owner resumes with. :seen holds dedup values already
   consumed; :pending-wake is the wake recorded before arming; :wake is the
   wake being delivered; :attempt counts resume attempts."
  [:map
   [:id :string]
   [:owner Owner]
   [:subscription Subscription]
   [:deadline [:maybe :int]]
   [:prompt [:maybe :string]]
   [:status (into [:enum] statuses)]
   [:created {:optional true} [:maybe :int]]
   [:seen {:optional true} [:maybe [:set :any]]]
   [:pending-wake {:optional true} [:maybe :map]]
   [:wake {:optional true} [:maybe :map]]
   [:attempt {:optional true} [:maybe :int]]])

(def Event
  "One event a source delivers: its source :type and its :data."
  [:map
   [:type :keyword]
   [:data {:optional true} :any]])

(def StepResult
  "What step answers: whether the awaiter woke, the wake payload, and the
   awaiter as it is after the event (the awaiter-prime of the design)."
  [:map
   [:wake? :boolean]
   [:payload [:maybe :map]]
   [:awaiter Awaiter]])

(def ArmResult
  "What arm answers: the awaiter as it is after arming, and the wake it
   recorded while :pending (nil when none did)."
  [:map
   [:awaiter Awaiter]
   [:pending-wake [:maybe :map]]])

(def AwaiterId
  "How a caller names an awaiter: the registry reads it through `str`."
  [:or :string :uuid :keyword :int])

(defn new-awaiter
  "A :pending awaiter from a create request, stamped `now`. Pure."
  [{:keys [id owner subscription deadline prompt]} now]
  {:id           (str id)
   :owner        owner
   :subscription (merge {:filter nil :dedup-key nil} subscription)
   :deadline     deadline
   :prompt       prompt
   :status       :pending
   :created      now
   :seen         #{}
   :pending-wake nil
   :wake         nil
   :attempt      0})

(defn pending?
  "True when `awaiter` is parked but not yet armed. Pure."
  [awaiter]
  (= :pending (:status awaiter)))

(m/=> pending? [:=> [:cat Awaiter] :boolean])

(defn armed?
  "True when `awaiter` can still wake. Pure."
  [awaiter]
  (= :armed (:status awaiter)))

(m/=> armed? [:=> [:cat Awaiter] :boolean])

(defn waking?
  "True when `awaiter`'s resume is in flight. Pure."
  [awaiter]
  (= :waking (:status awaiter)))

(m/=> waking? [:=> [:cat Awaiter] :boolean])

(defn live?
  "True while `awaiter` still holds a source (:pending, :armed or :waking).
   Pure."
  [awaiter]
  (contains? live-statuses (:status awaiter)))

(m/=> live? [:=> [:cat Awaiter] :boolean])

(defn expired?
  "True when `awaiter` has a deadline at or before `now`. Pure."
  [{:keys [deadline]} now]
  (boolean (and (some? deadline) (some? now) (<= deadline now))))

(m/=> expired? [:=> [:cat [:map [:deadline [:maybe :int]]] [:maybe :int]] :boolean])

(defn matches?
  "True when `event` is of the subscription's :type and its :data agrees
   with every :filter entry. Pure."
  [{:keys [type filter]} event]
  (and (= type (:type event))
       (let [data (:data event)]
         (every? (fn [[k v]] (and (map? data) (= v (get data k)))) filter))))

(m/=> matches? [:=> [:cat Subscription Event] :boolean])

(defn dedup-value
  "The value that identifies `event` under `subscription`, or nil when the
   subscription names no :dedup-key. Pure."
  [{:keys [dedup-key]} event]
  (when dedup-key
    (let [data (:data event)]
      (when (map? data) (get data dedup-key)))))

(m/=> dedup-value [:=> [:cat Subscription Event] :any])

(defn- woken
  "`awaiter` woken with `payload`. Pure."
  [awaiter payload]
  (assoc awaiter :status :woken :wake payload))

(defn- no-wake
  [awaiter]
  {:wake? false :payload nil :awaiter awaiter})

(defn- wake
  [awaiter payload]
  {:wake? true :payload payload :awaiter (woken awaiter payload)})

(defn- seen?
  [awaiter sub event]
  (contains? (or (:seen awaiter) #{}) (dedup-value sub event)))

(defn- event-wake
  "The payload a matching `event` produces at `base`, or nil when the event
   does not match or its dedup value was already consumed. Pure."
  [awaiter event base]
  (let [sub (:subscription awaiter)]
    (when (and (some? event)
               (matches? sub event)
               (not (seen? awaiter sub event)))
      (assoc base :reason :event :event event))))

(defn- record-pending
  "`awaiter` with the wake its event or deadline produces recorded as
   :pending-wake (first one wins). Exactly the payload a plain wake would
   carry, so arming later delivers the same thing. Pure."
  [awaiter event now]
  (let [base (event-wake awaiter event {:awaiter-id (:id awaiter) :at now})
        base (or base
                 (when (expired? awaiter now)
                   {:awaiter-id (:id awaiter) :at now :reason :timeout
                    :deadline (:deadline awaiter)}))]
    (cond
      (nil? base) (no-wake awaiter)
      (:pending-wake awaiter) (no-wake awaiter)
      :else
      (no-wake (cond-> (assoc awaiter :pending-wake base)
                 (some? (dedup-value (:subscription awaiter) event))
                 (update :seen (fnil conj #{}) (dedup-value (:subscription awaiter) event)))))))

(defn step
  "Decide what `event` (nil for a bare clock tick) does to `awaiter` at
   `now`. Answers {:wake? :payload :awaiter}, the awaiter as it is after.

   Order of rules:
     1. :pending               record a matching event or a reached deadline
                               as :pending-wake; never wake, never resume;
     2. not :armed             no wake, unchanged (a wake happens once);
     3. deadline reached       wake, payload {:reason :timeout};
     4. no event               no wake, unchanged;
     5. event does not match   no wake, unchanged;
     6. dedup value seen       no wake, unchanged;
     7. otherwise              wake, payload {:reason :event :event e}.
   A reached deadline outranks a late event: the wait already timed out.
   Pure."
  [awaiter event now]
  (let [base {:awaiter-id (:id awaiter) :at now}
        sub  (:subscription awaiter)]
    (cond
      (pending? awaiter)
      (record-pending awaiter event now)

      (not (armed? awaiter))
      (no-wake awaiter)

      (expired? awaiter now)
      (wake awaiter (assoc base :reason :timeout :deadline (:deadline awaiter)))

      (nil? event)
      (no-wake awaiter)

      (not (matches? sub event))
      (no-wake awaiter)

      (seen? awaiter sub event)
      (no-wake awaiter)

      :else
      ;; The payload is built from the awaiter as it stands: event-wake asks
      ;; whether this dedup value was already consumed, so marking it seen
      ;; first would answer yes and drop the wake.
      (let [payload (event-wake awaiter event base)
            a       (cond-> awaiter
                      (some? (dedup-value sub event))
                      (update :seen (fnil conj #{}) (dedup-value sub event)))]
        (wake a payload)))))

(m/=> step [:=> [:cat Awaiter [:maybe Event] :int] StepResult])

(defn arm
  "`awaiter` armed: a :pending awaiter becomes :armed, `run-id` is bound as
   the owner's :run-id when the owner is a :hive-agent-run and the id is
   given, and the wake recorded while pending is handed back.

   An awaiter that is not :pending is answered unchanged with no
   :pending-wake, so arm is idempotent: arming twice delivers nothing twice.
   Pure."
  [awaiter run-id]
  (if-not (pending? awaiter)
    {:awaiter awaiter :pending-wake nil}
    {:awaiter (cond-> (assoc awaiter :status :armed)
                (some? run-id) (assoc-in [:owner :run-id] (str run-id)))

     :pending-wake (:pending-wake awaiter)}))

(m/=> arm [:=> [:cat Awaiter [:maybe :any]] ArmResult])

(defn begin-wake
  "`awaiter` handed to its resumer: :waking, `payload` as its wake, one more
   attempt recorded. The registry persists this BEFORE it calls the resumer,
   which is what makes a crash mid-resume recoverable. Pure."
  [awaiter payload]
  (-> awaiter
      (assoc :status :waking :wake payload)
      (update :attempt (fnil inc 0))))

(m/=> begin-wake [:=> [:cat Awaiter [:maybe :map]] Awaiter])

(defn mark-woken
  "`awaiter` woken: its resume is done. Pure."
  [awaiter]
  (assoc awaiter :status :woken))

(m/=> mark-woken [:=> [:cat Awaiter] Awaiter])

(defn cancel
  "`awaiter` cancelled, when it is still live; otherwise unchanged. A
   :waking awaiter is cancelled too: a cancel outranks an in-flight resume,
   and the registry checks the status before it records the outcome. Pure."
  [awaiter]
  (if (live? awaiter) (assoc awaiter :status :cancelled) awaiter))

(m/=> cancel [:=> [:cat Awaiter] Awaiter])

(defn deferred?
  "True when a resumer answered that it could not resume now (heap pressure:
   {:success false :deferred true}), so the wake must be re-driven. A plain
   non-success is answered the same way by the caller. Pure."
  [answer]
  (boolean (and (map? answer)
                (false? (:success answer))
                (true? (:deferred answer)))))

(m/=> deferred? [:=> [:cat :any] :boolean])

(defn resume-prompt
  "The prompt the owner resumes with: the awaiter's continuation prompt
   followed by the wake payload as EDN, so the resumed run reads both what
   it planned to do and what woke it. Pure."
  [awaiter payload]
  (str (or (:prompt awaiter) "You were parked on an awaiter and it woke.")
       "\n\nWake payload (awaiter " (:id awaiter) "):\n"
       (pr-str payload)))

(def WakeEntry
  "One resume attempt as the registry's wake observer records it
   (ports/IWakeJournal): which awaiter, which owner kind, which attempt,
   when, and how it went. Plain EDN: keywords, strings, numbers, maps."
  [:map
   [:awaiter-id :string]
   [:owner-kind :keyword]
   [:attempt :int]
   [:at :int]
   [:ok? :boolean]
   [:reason {:optional true} [:maybe :keyword]]
   [:label {:optional true} [:maybe :string]]
   [:inbox {:optional true} [:maybe :string]]
   [:error {:optional true} [:maybe :string]]
   [:error-kind {:optional true} [:maybe :keyword]]
   [:deferred? {:optional true} :boolean]
   [:payload {:optional true} [:maybe :map]]
   [:value {:optional true} [:maybe :map]]])

(defn wake-entry
  "The WakeEntry for one resume attempt of `awaiter` (as it was handed to
   its resumer: :waking, its :wake payload and :attempt) whose outcome was
   `result` ({:ok? :value :error :data :deferred}), at `now`. Pure."
  [awaiter result now]
  (let [owner   (:owner awaiter)
        payload (:wake awaiter)
        kind    (get-in result [:data :error])]
    (cond-> {:awaiter-id (str (:id awaiter))
             :owner-kind (:kind owner)
             :attempt    (long (or (:attempt awaiter) 1))
             :at         (long now)
             :ok?        (boolean (:ok? result))
             :reason     (:reason payload)
             :payload    payload}
      (string? (:label owner))     (assoc :label (:label owner))
      (string? (:inbox owner))     (assoc :inbox (:inbox owner))
      (:error result)              (assoc :error (str (:error result)))
      (keyword? kind)              (assoc :error-kind kind)
      (:deferred result)           (assoc :deferred? true)
      (map? (:value result))       (assoc :value (:value result)))))

(m/=> wake-entry [:=> [:cat Awaiter :map :int] WakeEntry])

(m/=> new-awaiter [:=> [:cat :map [:maybe :int]] Awaiter])
(m/=> resume-prompt [:=> [:cat Awaiter [:maybe :map]] :string])
