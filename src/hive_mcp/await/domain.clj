(ns hive-mcp.await.domain
  "Wake bounded context, L0 values and L1 calculations: an Awaiter waits on
   one Subscription until a matching event or its deadline wakes it.

   Pure: no I/O, no atoms, no clock. The caller passes `now` (epoch ms).
   A deadline is a wake like any other, with :reason :timeout, so the one
   step function decides every wake and the boundary only executes it.

   Lifecycle: :armed -> :woken (an event or the deadline) | :cancelled.
   Only an :armed awaiter can wake, so a wake happens at most once."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; L0 - Vocabulary and schemas
;; =============================================================================

(def statuses
  "Every status an awaiter may have."
  [:armed :woken :cancelled])

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
   hive-mcp.await.ports/open-resumer); the other keys are the resumer's."
  [:map [:kind :keyword]])

(def Awaiter
  "One parked wait. :deadline is epoch ms or nil (wait forever). :prompt is
   the continuation the owner resumes with. :seen holds dedup values already
   consumed; :wake is the wake payload once woken."
  [:map
   [:id :string]
   [:owner Owner]
   [:subscription Subscription]
   [:deadline [:maybe :int]]
   [:prompt [:maybe :string]]
   [:status (into [:enum] statuses)]
   [:created {:optional true} [:maybe :int]]
   [:seen {:optional true} [:maybe [:set :any]]]
   [:wake {:optional true} [:maybe :map]]])

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

(defn new-awaiter
  "An :armed awaiter from a create request, stamped `now`. Pure."
  [{:keys [id owner subscription deadline prompt]} now]
  {:id           (str id)
   :owner        owner
   :subscription (merge {:filter nil :dedup-key nil} subscription)
   :deadline     deadline
   :prompt       prompt
   :status       :armed
   :created      now
   :seen         #{}
   :wake         nil})

(defn armed?
  "True when `awaiter` can still wake. Pure."
  [awaiter]
  (= :armed (:status awaiter)))

(defn expired?
  "True when `awaiter` has a deadline at or before `now`. Pure."
  [{:keys [deadline]} now]
  (boolean (and (some? deadline) (some? now) (<= deadline now))))

(defn matches?
  "True when `event` is of the subscription's :type and its :data agrees
   with every :filter entry. Pure."
  [{:keys [type filter]} event]
  (and (= type (:type event))
       (let [data (:data event)]
         (every? (fn [[k v]] (and (map? data) (= v (get data k)))) filter))))

(defn dedup-value
  "The value that identifies `event` under `subscription`, or nil when the
   subscription names no :dedup-key. Pure."
  [{:keys [dedup-key]} event]
  (when dedup-key
    (let [data (:data event)]
      (when (map? data) (get data dedup-key)))))

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

(defn step
  "Decide what `event` (nil for a bare clock tick) does to `awaiter` at
   `now`. Answers {:wake? :payload :awaiter}, the awaiter as it is after.

   Order of rules:
     1. not :armed            no wake, unchanged (a wake happens once);
     2. deadline reached      wake, payload {:reason :timeout};
     3. no event              no wake, unchanged;
     4. event does not match  no wake, unchanged;
     5. dedup value seen      no wake, unchanged;
     6. otherwise             wake, payload {:reason :event :event e}.
   A reached deadline outranks a late event: the wait already timed out.
   Pure."
  [awaiter event now]
  (let [base {:awaiter-id (:id awaiter) :at now}
        sub  (:subscription awaiter)]
    (cond
      (not (armed? awaiter))
      (no-wake awaiter)

      (expired? awaiter now)
      (wake awaiter (assoc base :reason :timeout :deadline (:deadline awaiter)))

      (nil? event)
      (no-wake awaiter)

      (not (matches? sub event))
      (no-wake awaiter)

      (contains? (or (:seen awaiter) #{}) (dedup-value sub event))
      (no-wake awaiter)

      :else
      (let [dv (dedup-value sub event)
            a  (cond-> awaiter (some? dv) (update :seen (fnil conj #{}) dv))]
        (wake a (assoc base :reason :event :event event))))))

(defn cancel
  "`awaiter` cancelled, when it is still :armed; otherwise unchanged. Pure."
  [awaiter]
  (if (armed? awaiter) (assoc awaiter :status :cancelled) awaiter))

(defn resume-prompt
  "The prompt the owner resumes with: the awaiter's continuation prompt
   followed by the wake payload as EDN, so the resumed run reads both what
   it planned to do and what woke it. Pure."
  [awaiter payload]
  (str (or (:prompt awaiter) "You were parked on an awaiter and it woke.")
       "\n\nWake payload (awaiter " (:id awaiter) "):\n"
       (pr-str payload)))

(m/=> new-awaiter [:=> [:cat :map [:maybe :int]] Awaiter])
(m/=> step [:=> [:cat Awaiter [:maybe Event] :int] StepResult])
(m/=> cancel [:=> [:cat Awaiter] Awaiter])
(m/=> resume-prompt [:=> [:cat Awaiter [:maybe :map]] :string])
