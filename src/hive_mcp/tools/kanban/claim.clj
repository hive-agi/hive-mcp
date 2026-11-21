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

   A coordinator id is `coordinator:<token>`, the token being a pid or a
   Claude Code session-id prefix. A session that moves to another process
   (resume, bg handoff) keeps its session id but not its pid, so two ids of
   one session are folded through the Claude Code session records
   (`same-session?`, kanban 20261111164432-2be7b1e6), and a coordinator's
   liveness is measured, never assumed (`token-live?`).

   `decide`, `apply-claim`, `token-session-ids`, `token-live?` and
   `same-session?` are pure. `holder-live?`, `same-lineage?` and `claim-ctx`
   read through `LivenessPorts` (default `default-ports`: swarm registry,
   OS, session records); the `:kanban/claim-ctx` coeffect calls them once
   per event."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [hive-mcp.channel.audience :as audience]
            [malli.core :as m]
            [hive-mcp.resilience.swallow :as swallow]))
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
  (swallow/attempt (when-let [f (requiring-resolve 'hive-mcp.swarm.datascript.queries/get-slave)]
         (f id))))

(defn- pid-alive? [pid]
  (try (let [^java.util.Optional h (java.lang.ProcessHandle/of (long pid))]
         (and (.isPresent h) (.isAlive ^java.lang.ProcessHandle (.get h))))
       (catch Throwable _ true)))

(def SessionRecord
  "One Claude Code session record (`~/.claude/sessions/<pid>.json`): the
   process now hosting a session, and the session's id."
  [:map [:pid :int] [:session-id :string]])

(def LivenessPorts
  "What the liveness reads go through. `:get-slave` reads the swarm registry,
   `:pid-alive?` the OS, `:session-records` the Claude Code session records
   (nil when this host keeps none, which makes a session token uncheckable)."
  [:map
   [:get-slave {:optional true} ifn?]
   [:pid-alive? {:optional true} ifn?]
   [:session-records {:optional true} ifn?]])

(defn- digits? [s] (boolean (and (string? s) (re-matches #"\d+" s))))

(defn- names-record?
  "Does session `token` name `record`? Any token names the sessions whose id
   it prefixes (bb-mcp stamps the first 8 hex digits of the Claude Code
   session id); an all-digit token also names the session its pid hosts,
   since a pid and a digit-only session prefix are spelled alike."
  [token {:keys [pid session-id]}]
  (or (str/starts-with? session-id token)
      (and (digits? token) (= (parse-long token) pid))))

(defn token-session-ids
  "Pure: the session ids coordinator session `token` names in `records`
   (see `names-record?`)."
  [token records]
  (let [t (present token)]
    (if (nil? t)
      #{}
      (into #{} (comp (filter #(names-record? t %)) (map :session-id)) records))))

(defn token-live?
  "Pure: is coordinator session `token` live? Live while a process hosting a
   session it names runs (`names-record?`). An all-digit token is also live
   while that pid runs. nil (unknown) when it is no pid and `records` is nil:
   the host keeps no session records, so nothing can be measured."
  [token records pid-alive?]
  (let [t (present token)]
    (cond
      (nil? t) nil
      (and (digits? t) (pid-alive? (parse-long t))) true
      (nil? records) (when (digits? t) false)
      :else (boolean (some #(and (names-record? t %) (pid-alive? (:pid %))) records)))))

(defn same-session?
  "Pure: do coordinator ids `a` and `b` name one Claude Code session? True
   when both carry a session token and the tokens resolve to a shared session
   id through `records`: `coordinator:2418990` (the pid its session ran
   under) and `coordinator:a2f8cfa6` (that session's id) are one owner while
   the record links them."
  [a b records]
  (let [ta (audience/coordinator-session (str a))
        tb (audience/coordinator-session (str b))]
    (boolean
     (and ta tb
          (or (= ta tb)
              (seq (set/intersection (token-session-ids ta records)
                                     (token-session-ids tb records))))))))

(def sessions-dir
  "Where Claude Code keeps one record per live session process."
  (str (System/getProperty "user.home") "/.claude/sessions"))

(defn read-session-records
  "The session records under `dir` as [{:pid :session-id}], or nil when the
   directory does not exist (a host that is no Claude Code). A record that
   does not parse is skipped."
  [dir]
  (let [d (io/file dir)]
    (when (.isDirectory d)
      (into []
            (keep (fn [^java.io.File f]
                    (when (str/ends-with? (.getName f) ".json")
                      (swallow/attempt (let [m (json/read-str (slurp f))
                                 pid (get m "pid")
                                 sid (present (get m "sessionId"))]
                             (when (and (int? pid) sid)
                               {:pid (long pid) :session-id sid}))))))
            (.listFiles d)))))

(defn default-ports
  "The live liveness ports: swarm registry, OS processes, Claude Code session
   records."
  []
  {:get-slave       get-slave
   :pid-alive?      pid-alive?
   :session-records #(read-session-records sessions-dir)})

(defn holder-live?
  "Is claim holder `owner` still alive? Measured through `ports`
   (`LivenessPorts`, default `default-ports`). A registered slave is live
   unless its status is dead. A coordinator session is live while a process
   hosting it runs (`token-live?`): `coordinator:<pid>` by that pid,
   `coordinator:<session-id prefix>` through the session records. Anything
   that cannot be measured counts as live, so only an explicit steal takes
   it over."
  ([owner] (holder-live? owner (default-ports)))
  ([owner {:keys [get-slave pid-alive? session-records]
           :or   {get-slave (constantly nil) pid-alive? (constantly true)}}]
   (if-let [slave (get-slave owner)]
     (not (contains? dead-statuses (:slave/status slave)))
     (let [token (audience/coordinator-session (str owner))
           live  (when token
                   (token-live? token (when session-records (session-records)) pid-alive?))]
       (if (nil? live) true live)))))

(defn same-lineage?
  "Does `caller` work the card on behalf of claim holder `holder`? True when
   the caller is a slave spawned for `task-id` or spawned by the holder.
   `get-slave` reads the swarm registry (default: the live one)."
  ([caller holder task-id] (same-lineage? caller holder task-id get-slave))
  ([caller holder task-id get-slave]
   (boolean
    (when-let [slave (get-slave caller)]
      (or (= (str task-id) (str (:slave/kanban-task-id slave)))
          (let [h (owner-key holder)]
            (some #(= h (owner-key %)) [(:slave/parent slave) (:slave/spawned-by slave)])))))))

(defn claim-ctx
  "The claim reads an event needs: {:holder-live? :same-lineage?} for the
   claim on `entry`'s content against `owner`, or {} when there is nothing to
   check. A caller whose coordinator id names the same Claude Code session as
   the holder's (`same-session?`: the session moved to another process, so
   its pid-shaped id changed) counts as the holder's lineage. Reads go
   through `ports` (`LivenessPorts`, default `default-ports`)."
  ([entry owner task-id] (claim-ctx entry owner task-id (default-ports)))
  ([entry owner task-id {:keys [get-slave session-records] :as ports}]
   (let [claim (claim-of (:content entry))]
     (if (and claim (present owner) (not= (owner-key owner) (owner-key (:owner claim))))
       (let [holder  (:owner claim)
             records (delay (when session-records (session-records)))
             same?   (and (audience/coordinator-session (str owner))
                          (audience/coordinator-session (str holder))
                          (same-session? owner holder @records))]
         {:holder-live?  (holder-live? holder ports)
          :same-lineage? (boolean (or same?
                                      (if get-slave
                                        (same-lineage? owner holder task-id get-slave)
                                        (same-lineage? owner holder task-id))))})
       {}))))

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
(m/=> token-session-ids [:=> [:cat :any [:maybe [:sequential SessionRecord]]] [:set :string]])
(m/=> token-live? [:=> [:cat :any [:maybe [:sequential SessionRecord]] ifn?] [:maybe :boolean]])
(m/=> same-session? [:=> [:cat :any :any [:maybe [:sequential SessionRecord]]] :boolean])
(m/=> read-session-records [:=> [:cat :string] [:maybe [:vector SessionRecord]]])
(m/=> default-ports [:=> [:cat] LivenessPorts])
(m/=> holder-live? [:function
                    [:=> [:cat :any] :boolean]
                    [:=> [:cat :any LivenessPorts] :boolean]])
(m/=> same-lineage? [:function
                     [:=> [:cat :any :any :any] :boolean]
                     [:=> [:cat :any :any :any ifn?] :boolean]])
(m/=> claim-ctx [:function
                 [:=> [:cat :any :any :any] :map]
                 [:=> [:cat :any :any :any LivenessPorts] :map]])
