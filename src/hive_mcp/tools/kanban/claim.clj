(ns hive-mcp.tools.kanban.claim
  "Card claims: who is working a kanban card, and since when.

   A move into `doing` (inprogress) or an `agent spawn` with kanban_task_id
   records {:owner <agent/coordinator id> :since <timestamp>} under the
   card's content `:claim`. A second claim by an unrelated owner is refused,
   naming the holder and since, unless the caller passes steal_claim. A claim
   held by a dead owner is taken over silently. A move to `todo` or `done`
   clears the claim. Moving the card does not by itself stop a second
   coordinator from taking it (tool-friction 20261109113824-1b6c42b4: two
   coordinators spawned lings on one card).

   `decide` and `apply-claim` are pure. `holder-live?` and `same-lineage?`
   read the swarm registry and the OS; the `:kanban/claim-ctx` coeffect
   calls them once per event."
  (:require [clojure.string :as str]
            [hive-mcp.channel.audience :as audience]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Claim
  [:map [:owner :string] [:since {:optional true} [:maybe :string]]])

(def Verdict
  [:or
   [:enum :skip :take :keep :steal :reclaim]
   [:map [:refused [:= true]] [:holder :string] [:since [:maybe :string]]]])

(defn- present [s] (when (and (string? s) (not (str/blank? s))) s))

(defn owner-key
  "The identity an owner id is compared by. A coordinator lane that names a
   session reads as `coordinator:<session>`, so `coordinator:7-hive` and
   `coordinator:7` are one owner; every other id is itself."
  [id]
  (when-let [s (present (some-> id str))]
    (if-let [session (audience/coordinator-session s)]
      (str "coordinator:" session)
      s)))

(defn claim-of
  "The claim recorded on a card's `content`, or nil. Tolerates JSON-roundtripped
   string keys."
  [content]
  (let [c (when (map? content) (or (get content :claim) (get content "claim")))
        owner (when (map? c) (present (or (get c :owner) (get c "owner"))))]
    (when owner
      {:owner owner
       :since (let [s (or (get c :since) (get c "since"))] (when (string? s) s))})))

(defn decide
  "Pure: what a claim attempt by `owner` does to the card's current `claim`.

     :skip    no owner known: nothing recorded, nothing checked
     :take    no claim yet
     :keep    the owner (or its lineage) already holds it: since unchanged
     :steal   held by a live foreign owner, and steal? was asked
     :reclaim held by an owner that is dead
     {:refused true :holder :since}  held by a live foreign owner"
  [{:keys [claim owner steal? holder-live? same-lineage?]}]
  (let [owner (owner-key owner)
        holder (owner-key (:owner claim))]
    (cond
      (nil? owner)                         :skip
      (nil? holder)                        :take
      (or (= owner holder) same-lineage?)  :keep
      steal?                               :steal
      (false? holder-live?)                :reclaim
      :else {:refused true :holder (:owner claim) :since (:since claim)})))

(defn refused? [verdict] (and (map? verdict) (true? (:refused verdict))))

(defn refusal-message
  "The error text of a refused claim on `task-id`."
  [task-id {:keys [holder since]}]
  (str "Card " task-id " is claimed by " holder
       (when since (str " since " since))
       " and that owner is still live. Coordinate with it, or pass"
       " steal_claim=true to take the card over."))

(defn- strip-claim [content] (dissoc content :claim "claim"))

(defn apply-claim
  "Pure: `content` with the claim `verdict` applied for a move to `new-status`
   (nil for a claim with no status move). A move to todo or done clears the
   claim; :take/:steal/:reclaim record {:owner owner :since now}; every other
   verdict leaves the content as it is."
  [content new-status verdict owner now]
  (cond
    (#{"todo" "done"} new-status) (strip-claim content)
    (#{:take :steal :reclaim} verdict)
    (-> (strip-claim content) (assoc :claim {:owner owner :since now}))
    :else content))

(defn claims-on?
  "True when a move to `new-status` is a claim attempt."
  [new-status]
  (= "doing" new-status))

;; -----------------------------------------------------------------------------
;; Boundary reads (the :kanban/claim-ctx coeffect)
;; -----------------------------------------------------------------------------

(def ^:private dead-statuses #{:terminated :dead :killed :zombie})

(defn- get-slave [id]
  (try (when-let [f (requiring-resolve 'hive-mcp.swarm.datascript.queries/get-slave)]
         (f id))
       (catch Throwable _ nil)))

(defn- pid-alive? [pid]
  (try (let [^java.util.Optional h (java.lang.ProcessHandle/of (long pid))]
         (and (.isPresent h) (.isAlive ^java.lang.ProcessHandle (.get h))))
       (catch Throwable _ true)))

(defn holder-live?
  "Is claim holder `owner` still alive? A registered slave is live unless its
   status is dead. A `coordinator:<pid>` session is live while that process
   runs. Anything else cannot be checked and counts as live, so only an
   explicit steal takes it over."
  [owner]
  (if-let [slave (get-slave owner)]
    (not (contains? dead-statuses (:slave/status slave)))
    (if-let [pid (some->> (audience/coordinator-session (str owner))
                          (re-matches #"\d+")
                          parse-long)]
      (pid-alive? pid)
      true)))

(defn same-lineage?
  "Does `caller` work the card on behalf of claim holder `holder`? True when
   the caller is a slave spawned for `task-id` or spawned by the holder."
  [caller holder task-id]
  (boolean
   (when-let [slave (get-slave caller)]
     (or (= (str task-id) (str (:slave/kanban-task-id slave)))
         (let [h (owner-key holder)]
           (some #(= h (owner-key %)) [(:slave/parent slave) (:slave/spawned-by slave)]))))))

(defn claim-ctx
  "The claim reads an event needs: {:holder-live? :same-lineage?} for the
   claim on `entry`'s content against `owner`, or {} when there is nothing to
   check."
  [entry owner task-id]
  (let [claim (claim-of (:content entry))]
    (if (and claim (present owner) (not= (owner-key owner) (owner-key (:owner claim))))
      {:holder-live?  (holder-live? (:owner claim))
       :same-lineage? (same-lineage? owner (:owner claim) task-id)}
      {})))

(defn- claim-ctx-cofx
  "Coeffect `:kanban/claim-ctx`: the claim reads for the event in flight,
   after `:kanban/entry`. Only a claim attempt (a move into doing, or a
   `:kanban/claim` event) reads anything."
  [coeffects]
  (let [[id {:keys [task-id owner new-status]}] (:event coeffects)
        attempt? (or (= :kanban/claim id)
                     (claims-on? (get {"inprogress" "doing"} new-status new-status)))]
    (assoc coeffects :kanban/claim-ctx
           (if attempt?
             (claim-ctx (:kanban/entry coeffects) owner task-id)
             {}))))

(defn register!
  "Register the `:kanban/claim-ctx` coeffect and the `:kanban/claim-refused`
   effect. The refusal effect does nothing: the dispatch boundary reads it off
   the effect map and answers the error; it is registered only so the fx
   runner does not warn about an unknown effect. Key-addressed, so idempotent."
  []
  ((requiring-resolve 'hive.events.cofx/reg-cofx) :kanban/claim-ctx claim-ctx-cofx)
  ((requiring-resolve 'hive.events.fx/reg-fx) :kanban/claim-refused (fn [_] nil))
  true)

(m/=> register! [:=> [:cat] [:= true]])
(m/=> owner-key [:=> [:cat :any] [:maybe :string]])
(m/=> claim-of [:=> [:cat :any] [:maybe Claim]])
(m/=> decide [:=> [:cat :map] Verdict])
(m/=> refused? [:=> [:cat :any] :boolean])
(m/=> refusal-message [:=> [:cat :any :map] :string])
(m/=> apply-claim [:=> [:cat [:maybe :map] [:maybe :string] :any [:maybe :string] [:maybe :string]] [:maybe :map]])
(m/=> claims-on? [:=> [:cat [:maybe :string]] :boolean])
(m/=> holder-live? [:=> [:cat :any] :boolean])
(m/=> same-lineage? [:=> [:cat :any :any :any] :boolean])
(m/=> claim-ctx [:=> [:cat :any :any :any] :map])
