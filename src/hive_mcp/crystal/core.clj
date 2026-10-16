(ns hive-mcp.crystal.core
  "Progressive crystallization of ephemeral knowledge.

   Public fns delegate to extension registry (:cc/*) with inline fallbacks."
  (:require [clojure.string :as str]
            [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.extensions.registry :as ext]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Delegate-or-fallback helper
;; =============================================================================

(defn- delegate
  "Try extension, fall back to inline fn."
  [ext-key fallback-fn args]
  (if-let [f (ext/get-extension ext-key)]
    (apply f args)
    (apply fallback-fn args)))

;; Forward declarations used in should-promote?
(declare scope-boost)

;; =============================================================================
;; Value-object schemas (contracts for the public fns below)
;; =============================================================================

(def Duration
  "A memory duration tier as returned by the promotion/demotion rules."
  [:enum :ephemeral :short :medium :long :permanent])

(def Instant
  "A java.time.Instant (session start/end)."
  [:fn {:gen/schema [:int {:min 0 :max 4000000000}]
        :gen/fmap (fn [s] (java.time.Instant/ofEpochSecond s))}
   #(instance? java.time.Instant %)])

(def RecallWeights
  "recall-context -> weight."
  [:map-of :keyword number?])

(def PromotionThresholds
  "`:from->to` -> score threshold."
  [:map-of :keyword number?])

(def Recall
  "One recall-history row. `:count` defaults to 1 when absent."
  [:map [:context {:optional true} :any] [:count {:optional true} number?]])

(def PromotionScore
  [:map [:score number?]
   [:breakdown [:vector [:map [:context :any] [:weight number?] [:count number?] [:contribution number?]]]]])

(def PromotableEntry
  [:map [:duration {:optional true} :any] [:recalls {:optional true} [:maybe [:sequential Recall]]]])

(def PromoteVerdict
  [:map [:promote? :boolean] [:current-score number?] [:base-score number?]
   [:behavioral-adjustment number?] [:scope-boost-override number?]
   [:threshold number?] [:next-duration [:maybe Duration]]])

(def DemoteVerdict
  [:map [:demote? :boolean] [:reason [:maybe [:= :behavioral-corrections]]]
   [:behavioral-adjustment number?] [:prev-duration [:maybe Duration]]])

(def SessionTag
  [:fn {:gen/schema :string :gen/fmap (fn [s] (str "session:" s))}
   #(and (string? %) (str/starts-with? % "session:"))])

(def SessionUidTag
  [:fn {:gen/schema :string :gen/fmap (fn [s] (str "session-uid:" s))}
   #(and (string? %) (str/starts-with? % "session-uid:"))])

(def SessionTiming
  [:map [:session-start [:maybe :string]] [:session-end :string] [:duration-minutes :int]])

(def DecayEntry
  [:maybe [:map [:access-count {:optional true} [:maybe number?]]
           [:last-accessed {:optional true} :any]
           [:duration {:optional true} [:maybe :string]]
           [:type {:optional true} [:maybe :string]]]])

(def ProgressNote
  [:map [:type [:= :note]] [:content :string] [:tags [:vector :string]] [:duration [:= :ephemeral]]])

(def SummaryNote
  [:map [:type [:= :note]] [:content :string] [:tags [:vector :string]] [:duration [:= :short]]])

;; =============================================================================
;; Crystal Config Registry (OCP: injectable domain constants)
;; =============================================================================

(defonce ^:private crystal-config-extensions (atom {}))

(defn register-crystal-config!
  "Register crystal domain configuration values.
   config-map: {:recall-weights {...} :promotion-thresholds {...}}
   Thread-safe, idempotent (merges with existing)."
  [config-map]
  (swap! crystal-config-extensions merge config-map))

(m/=> register-crystal-config! [:=> [:cat [:maybe [:map-of :keyword :any]]] [:map-of :keyword :any]])

(defn get-crystal-config
  "Look up a registered crystal config by key."
  [k]
  (get @crystal-config-extensions k))

(m/=> get-crystal-config [:=> [:cat :keyword] :any])

(defn get-crystal-config-or
  "Look up a registered crystal config by key, with default fallback."
  [k default]
  (get @crystal-config-extensions k default))

(m/=> get-crystal-config-or [:=> [:cat :keyword :any] :any])

;; =============================================================================
;; Recall Context Weights (defaults — overridable via crystal config registry)
;; =============================================================================

(def default-recall-weights
  "Default weights for different recall contexts.
   Higher = more meaningful signal for promotion.
   Can be overridden via register-crystal-config!"
  {:catchup-structural 0.1
   :wrap-structural 0.1
   :explicit-reference 1.0
   :cross-session 2.0
   :cross-project 3.0
   :user-feedback 5.0
   :behavioral-success 2.0
   :behavioral-failure 0.0
   :behavioral-correction -2.0})

(defn recall-weights
  "Weights for different recall contexts.
   Returns registered config if available, otherwise defaults."
  []
  (get-crystal-config-or :recall-weights default-recall-weights))

(m/=> recall-weights [:=> [:cat] RecallWeights])

(def default-promotion-thresholds
  "Default score thresholds for promotion between durations.
   Can be overridden via register-crystal-config!"
  {:ephemeral->short 5.0
   :short->medium 10.0
   :medium->long 15.0
   :long->permanent 25.0})

(defn promotion-thresholds
  "Score thresholds for promotion between durations.
   Returns registered config if available, otherwise defaults."
  []
  (get-crystal-config-or :promotion-thresholds default-promotion-thresholds))

(m/=> promotion-thresholds [:=> [:cat] PromotionThresholds])

;; =============================================================================
;; Score Calculation — delegates to :cc/promotion-score
;; =============================================================================

(defn- calculate-promotion-score-fallback [recalls]
  (let [rw (recall-weights)
        breakdown (for [{:keys [context count] :or {count 1}} recalls
                        :let [weight (get rw context 1.0)
                              contribution (* weight count)]]
                    {:context context :weight weight :count count :contribution contribution})
        total-score (reduce + 0.0 (map :contribution breakdown))]
    {:score total-score :breakdown (vec breakdown)}))

(defn calculate-promotion-score
  "Calculate promotion score from recall history."
  [recalls]
  (delegate :cc/promotion-score calculate-promotion-score-fallback [recalls]))

(m/=> calculate-promotion-score [:=> [:cat [:maybe [:sequential Recall]]] PromotionScore])

(defn current-duration->next
  "Map current duration to next tier."
  [duration]
  (case (keyword duration)
    :ephemeral :short
    :short :medium
    :medium :long
    :long :permanent
    :permanent :permanent
    (case (str duration)
      "ephemeral" :short
      "short-term" :medium
      "short" :medium
      "medium" :long
      "long-term" :permanent
      "long" :permanent
      "permanent" :permanent
      :medium)))

(m/=> current-duration->next [:=> [:cat :any] Duration])

(defn threshold-for-duration
  "Get promotion threshold for current duration."
  [duration]
  (let [pt (promotion-thresholds)]
    (case (keyword duration)
      :ephemeral (:ephemeral->short pt)
      :short (:short->medium pt)
      :short-term (:short->medium pt)
      :medium (:medium->long pt)
      :long (:long->permanent pt)
      :long-term (:long->permanent pt)
      :permanent Double/MAX_VALUE
      10.0)))

(m/=> threshold-for-duration [:=> [:cat :any] [:maybe number?]])

;; =============================================================================
;; Promotion / Demotion — delegates to :cc/should-promote, :cc/should-demote
;; =============================================================================

(defn- should-promote-fallback
  ([entry] (should-promote-fallback entry {}))
  ([{:keys [duration recalls] :as entry}
    {:keys [behavioral-adjustment scope-boost-override]
     :or {behavioral-adjustment 0.0 scope-boost-override 0.0}}]
   (let [{:keys [score]} (calculate-promotion-score recalls)
         xpoll-boost (if (zero? scope-boost-override) (scope-boost entry) scope-boost-override)
         adjusted-score (+ score behavioral-adjustment xpoll-boost)
         threshold (threshold-for-duration duration)
         should? (>= adjusted-score threshold)]
     {:promote? should?
      :current-score adjusted-score
      :base-score score
      :behavioral-adjustment behavioral-adjustment
      :scope-boost-override xpoll-boost
      :threshold threshold
      :next-duration (when should? (current-duration->next duration))})))

(defn should-promote?
  "Determine if a memory entry should be promoted."
  ([entry]
   (delegate :cc/should-promote should-promote-fallback [entry]))
  ([entry opts]
   (delegate :cc/should-promote should-promote-fallback [entry opts])))

(m/=> should-promote? [:function [:=> [:cat PromotableEntry] PromoteVerdict] [:=> [:cat PromotableEntry [:maybe [:map [:behavioral-adjustment {:optional true} number?] [:scope-boost-override {:optional true} number?]]]] PromoteVerdict]])

(defn- should-demote-fallback
  [{:keys [duration] :as _entry} behavioral-adjustment]
  (let [demote? (< behavioral-adjustment -3.0)
        prev-duration (case (keyword duration)
                        :permanent :long
                        :long :medium
                        :medium :short
                        :short :ephemeral
                        :ephemeral :ephemeral
                        :short)]
    {:demote? demote?
     :reason (when demote? :behavioral-corrections)
     :behavioral-adjustment behavioral-adjustment
     :prev-duration (when demote? prev-duration)}))

(defn should-demote?
  "Determine if a memory entry should be demoted."
  [{:keys [duration] :as entry} behavioral-adjustment]
  (delegate :cc/should-demote should-demote-fallback [entry behavioral-adjustment]))

(m/=> should-demote? [:=> [:cat [:maybe [:map [:duration {:optional true} :any]]] number?] DemoteVerdict])

;; =============================================================================
;; Session Tagging — delegates to :cc/session-id, :cc/session-tag, :cc/extract-session
;; =============================================================================

(defn- session-id-fallback []
  (let [now (java.time.LocalDateTime/now)
        fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd")]
    (.format now fmt)))

(defn session-id
  "Generate a session identifier for today."
  []
  (delegate :cc/session-id session-id-fallback []))

(m/=> session-id [:=> [:cat] [:string {:min 1}]])

(defn- session-tag-fallback
  ([] (str "session:" (session-id)))
  ([date-str] (str "session:" date-str)))

(defn session-tag
  "Create a session scope tag."
  ([] (delegate :cc/session-tag session-tag-fallback []))
  ([date-str] (delegate :cc/session-tag session-tag-fallback [date-str])))

(m/=> session-tag [:function [:=> [:cat] SessionTag] [:=> [:cat :any] SessionTag]])

;; session-id above is the calendar DATE, and it stays that way: `session:<date>`
;; is what every existing recall query matches on. It is NOT an identity — every
;; concurrent session on a box shares it — so anything that needs to know WHOSE
;; session a row belongs to uses the uid below instead. See
;; hive-mcp.session.identity for the ownership rules built on it.

(defn session-uid
  "The id of THIS session, distinct from every other session running today.
   Resolved by hive-mcp.session.current; nil if that namespace is unavailable,
   in which case callers simply omit the tag rather than inventing one."
  []
  (try
    (when-let [f (requiring-resolve 'hive-mcp.session.current/session-id)]
      (f))
    (catch Throwable _ nil)))

(m/=> session-uid [:=> [:cat] [:maybe :string]])

(defn session-uid-tag
  "`session-uid:<id>` for the current session, or nil. Carried ALONGSIDE
   session-tag, never instead of it, so existing date queries keep working."
  []
  (when-let [uid (session-uid)]
    (str "session-uid:" uid)))

(m/=> session-uid-tag [:=> [:cat] [:maybe SessionUidTag]])

(defn session-tags
  "The session tags a wrap-generated entry should carry: the date tag always,
   the uid tag when this session has an identity."
  []
  (into [(session-tag)] (remove nil?) [(session-uid-tag)]))

(m/=> session-tags [:=> [:cat] [:vector {:min 1 :max 2} :string]])

(defn- extract-session-fallback [tags]
  (some #(when (str/starts-with? % "session:") (subs % 8)) tags))

(defn extract-session-uid-from-tags
  "Pull the owning session's uid back out of an entry's tags. Returns nil for an
   entry written before uid tagging, which is exactly the 'unowned' case the
   ownership rules refuse to guess about."
  [tags]
  (some #(when (str/starts-with? % "session-uid:") (subs % 12)) tags))

(m/=> extract-session-uid-from-tags [:=> [:cat [:maybe [:or [:sequential :string] [:set :string]]]] [:maybe :string]])

(defn extract-session-from-tags
  "Extract session identifier from tags."
  [tags]
  (delegate :cc/extract-session extract-session-fallback [tags]))

(m/=> extract-session-from-tags [:=> [:cat [:maybe [:or [:sequential :string] [:set :string]]]] [:maybe :string]])

;; =============================================================================
;; Session Timestamp Tracking — delegates to :cc/record-start!, :cc/get-start, :cc/reset-start!
;; =============================================================================

;; Fallback atom for when addon is not loaded
(defonce ^{:private true
           :doc "Per-agent session start times (fallback when addon not loaded)."}
  session-start-tracker
  (atom {}))

(def ^:private default-agent-key "_global")

(defn- record-session-start-fallback
  ([] (record-session-start-fallback nil))
  ([agent-id]
   (let [k (or agent-id default-agent-key)
         now (java.time.Instant/now)]
     (swap! session-start-tracker
            (fn [m] (if (contains? m k) m (assoc m k now))))
     (get @session-start-tracker k))))

(defn record-session-start!
  "Record session start timestamp for an agent. Idempotent per agent-id."
  ([] (delegate :cc/record-start! record-session-start-fallback []))
  ([agent-id] (delegate :cc/record-start! record-session-start-fallback [agent-id])))

(m/=> record-session-start! [:function [:=> [:cat] Instant] [:=> [:cat [:maybe :string]] Instant]])

(defn- get-session-start-fallback
  ([] (get-session-start-fallback nil))
  ([agent-id]
   (let [k (or agent-id default-agent-key)]
     (or (get @session-start-tracker k)
         (when-not (= k default-agent-key)
           (get @session-start-tracker default-agent-key))))))

(defn get-session-start
  "Get recorded session start time for an agent."
  ([] (delegate :cc/get-start get-session-start-fallback []))
  ([agent-id] (delegate :cc/get-start get-session-start-fallback [agent-id])))

(m/=> get-session-start [:function [:=> [:cat] [:maybe Instant]] [:=> [:cat [:maybe :string]] [:maybe Instant]]])

(defn- reset-session-start-fallback
  ([] (reset! session-start-tracker {}))
  ([agent-id]
   (let [k (or agent-id default-agent-key)]
     (swap! session-start-tracker dissoc k))))

(defn reset-session-start!
  "Reset session start tracker for an agent."
  ([] (delegate :cc/reset-start! reset-session-start-fallback []))
  ([agent-id] (delegate :cc/reset-start! reset-session-start-fallback [agent-id])))

(m/=> reset-session-start! [:function [:=> [:cat] [:map-of :string Instant]] [:=> [:cat [:maybe :string]] [:map-of :string Instant]]])

(defn- session-timing-metadata-fallback [start-instant end-instant]
  (let [duration-minutes (if (and start-instant end-instant)
                           (.between java.time.temporal.ChronoUnit/MINUTES
                                     start-instant end-instant)
                           0)]
    {:session-start (some-> start-instant .toString)
     :session-end (.toString end-instant)
     :duration-minutes duration-minutes}))

(defn session-timing-metadata
  "Compute session timing metadata from start and end instants."
  [start-instant end-instant]
  (delegate :cc/timing-meta session-timing-metadata-fallback [start-instant end-instant]))

(m/=> session-timing-metadata [:=> [:cat [:maybe Instant] Instant] SessionTiming])

;; =============================================================================
;; Crystallization Rules (Pure Predicates) — kept inline (trivial, no IP)
;; =============================================================================

(defn mechanical-recall?
  "Is this recall context mechanical/structural (low signal)?"
  [context]
  (contains? #{:catchup-structural :wrap-structural} context))

(m/=> mechanical-recall? [:=> [:cat :any] :boolean])

(defn meaningful-recalls
  "Filter to only meaningful recalls."
  [recalls]
  (remove #(mechanical-recall? (:context %)) recalls))

(m/=> meaningful-recalls [:=> [:cat [:maybe [:sequential [:maybe :map]]]] [:sequential [:maybe :map]]])

(defn cross-boundary-recalls
  "Get recalls that cross session/project boundaries."
  [recalls]
  (filter #(contains? #{:cross-session :cross-project} (:context %)) recalls))

(m/=> cross-boundary-recalls [:=> [:cat [:maybe [:sequential [:maybe :map]]]] [:sequential [:maybe :map]]])

(defn has-user-endorsement?
  "Check if any recall has user feedback."
  [recalls]
  (some #(= :user-feedback (:context %)) recalls))

(m/=> has-user-endorsement? [:=> [:cat [:maybe [:sequential [:maybe :map]]]] [:maybe [:= true]]])

(defn behavioral-recall?
  "Is this recall context a behavioral signal?"
  [context]
  (contains? #{:behavioral-success :behavioral-failure :behavioral-correction} context))

(m/=> behavioral-recall? [:=> [:cat :any] :boolean])

(defn behavioral-recalls
  "Filter to only behavioral signal recalls."
  [recalls]
  (filter #(behavioral-recall? (:context %)) recalls))

(m/=> behavioral-recalls [:=> [:cat [:maybe [:sequential [:maybe :map]]]] [:sequential [:maybe :map]]])

(defn has-behavioral-signal?
  "Check if any recall has behavioral outcome data."
  [recalls]
  (some behavioral-recall? (map :context recalls)))

(m/=> has-behavioral-signal? [:=> [:cat [:maybe [:sequential [:maybe :map]]]] [:maybe [:= true]]])

;; =============================================================================
;; Staleness Decay — delegates to :cc/decay-candidate, :cc/decay-delta
;; =============================================================================

(defn days-since
  "Calculate days elapsed since a timestamp string."
  [timestamp-str]
  (when (and timestamp-str (not (str/blank? (str timestamp-str))))
    (rescue nil
            (let [then (java.time.ZonedDateTime/parse (str timestamp-str))
                  now (java.time.ZonedDateTime/now)]
              (.between java.time.temporal.ChronoUnit/DAYS then now)))))

(m/=> days-since [:=> [:cat :any] [:maybe :int]])

(defn- decay-candidate-fallback
  ([entry] (decay-candidate-fallback entry {}))
  ([{:keys [access-count duration type] :as _entry}
    {:keys [access-threshold] :or {access-threshold 3}}]
   (and (< (or access-count 0) access-threshold)
        (not= duration "permanent")
        (not= type "axiom"))))

(defn decay-candidate?
  "Check if entry should be considered for staleness decay."
  ([entry] (delegate :cc/decay-candidate decay-candidate-fallback [entry]))
  ([entry opts] (delegate :cc/decay-candidate decay-candidate-fallback [entry opts])))

(m/=> decay-candidate? [:function [:=> [:cat DecayEntry] :boolean] [:=> [:cat DecayEntry [:maybe [:map [:access-threshold {:optional true} number?]]]] :boolean]])

(def ^:private duration-decay-rate
  {"ephemeral" 2.0
   "short"     1.5
   "medium"    1.0
   "long"      0.5})

(defn- calculate-decay-delta-fallback
  ([entry] (calculate-decay-delta-fallback entry {}))
  ([{:keys [access-count last-accessed duration] :as _entry}
    {:keys [recency-days] :or {recency-days 7}}]
   (let [days-idle (or (days-since last-accessed) 30)
         access (max 1 (or access-count 0))]
     (if (< days-idle recency-days)
       0.0
       (let [time-factor (/ (double days-idle) 30.0)
             access-dampening (/ 1.0 (Math/log (+ access 2)))
             rate (get duration-decay-rate (or duration "medium") 1.0)]
         (* time-factor access-dampening rate))))))

(defn calculate-decay-delta
  "Calculate staleness-beta increase for a decay cycle."
  ([entry] (delegate :cc/decay-delta calculate-decay-delta-fallback [entry]))
  ([entry opts] (delegate :cc/decay-delta calculate-decay-delta-fallback [entry opts])))

(m/=> calculate-decay-delta [:function [:=> [:cat DecayEntry] [:double {:min 0.0}]] [:=> [:cat DecayEntry [:maybe [:map [:recency-days {:optional true} number?]]]] [:double {:min 0.0}]]])

;; =============================================================================
;; Scope Detection — already delegates via ext registry (unchanged)
;; =============================================================================

(defn extract-xpoll-projects
  "Extract distinct project IDs from scope tags."
  [entry]
  (let [tags (or (:tags entry) [])]
    (->> tags
         (filter #(str/starts-with? % "xpoll:project:"))
         (map #(subs % (count "xpoll:project:")))
         set)))

(m/=> extract-xpoll-projects [:=> [:cat [:maybe [:map [:tags {:optional true} [:maybe [:or [:sequential :string] [:set :string]]]]]]] [:set :string]])

(defn scope-count
  "Count distinct scope accesses for this entry."
  [entry]
  (count (extract-xpoll-projects entry)))

(m/=> scope-count [:=> [:cat [:maybe [:map [:tags {:optional true} [:maybe [:or [:sequential :string] [:set :string]]]]]]] [:int {:min 0}]])

(defn scope-boost
  "Compute promotion score boost from scope breadth.
   Delegates to extension if available. Returns 0.0 otherwise."
  [entry]
  (if-let [f (ext/get-extension :gx/score)]
    (f entry)
    0.0))

(m/=> scope-boost [:=> [:cat [:maybe :map]] number?])

(defn scope-eligible?
  "Predicate: is this entry eligible for scope-based auto-promotion?"
  ([entry] (scope-eligible? entry {}))
  ([entry opts]
   (if-let [f (ext/get-extension :gx/eligible?)]
     (f entry opts)
     false)))

(m/=> scope-eligible? [:function [:=> [:cat [:maybe :map]] :boolean] [:=> [:cat [:maybe :map] [:maybe :map]] :boolean]])

(defn scope-tiers
  "Compute tiers to promote based on scope breadth."
  [entry]
  (if-let [f (ext/get-extension :gx/tiers)]
    (f entry)
    0))

(m/=> scope-tiers [:=> [:cat [:maybe :map]] [:int {:min 0}]])

;; =============================================================================
;; Progress Note Generation — delegates to :cc/task-to-note, :cc/summarize-*
;; =============================================================================

(defn- task-to-progress-note-fallback
  [{:keys [title context priority started] :as task}]
  (let [completed-at (or (:completed-at task) (.toString (java.time.Instant/now)))
        duration-str (when started (str " (started: " started ")"))
        content (str "## Completed: " title "\n\n"
                     (when context (str context "\n\n"))
                     "Priority: " (or priority "medium")
                     duration-str "\nCompleted: " completed-at)]
    {:type :note
     :content content
     :tags (into (session-tags)
                 ["session-progress" "completed-task"
                  (str "priority-" (or priority "medium"))])
     :duration :ephemeral}))

(defn task-to-progress-note
  "Convert a completed kanban task to a progress note."
  [task]
  (delegate :cc/task-to-note task-to-progress-note-fallback [task]))

(m/=> task-to-progress-note [:=> [:cat [:map [:title {:optional true} :any] [:context {:optional true} [:maybe :string]] [:priority {:optional true} [:maybe :string]] [:started {:optional true} :any] [:completed-at {:optional true} :any]]] ProgressNote])

(defn- extract-content-summary [content]
  (cond
    (nil? content) "(no content)"
    (string? content) (or (first (str/split-lines content)) "(empty)")
    (map? content) (or (:title content) (:task-type content) (str (keys content)))
    :else (str content)))

(defn- summarize-session-progress-fallback [notes git-commits & [harvested]]
  (let [notes (->> (or notes []) (filter map?))
        git-commits (or git-commits [])
        notes-with-content (->> notes
                                (filter #(let [c (:content %)
                                               t (:title %)]
                                           (or (and (some? c)
                                                    (if (string? c) (not (str/blank? c)) true))
                                               (and (some? t)
                                                    (if (string? t) (not (str/blank? t)) true))))))
        task-count (count (filter #(some #{"completed-task"} (:tags %)) notes-with-content))
        session (session-id)
        note-summaries (->> notes-with-content
                            (map #(extract-content-summary (or (:content %) %)))
                            (remove #(contains? #{"(no content)" "(empty)"} %))
                            (map #(str "- " %))
                            (str/join "\n"))
        commit-summaries (->> git-commits (map #(str "- " %)) (str/join "\n"))
        ;; New: KG edge and kanban movement counts from harvested data
        kg-edge-count (get-in harvested [:summary :kg-edge-count] 0)
        kanban-mvs-count (get-in harvested [:summary :kanban-movement-count] 0)
        has-content? (or (seq notes-with-content) (seq git-commits)
                         (pos? kg-edge-count) (pos? kanban-mvs-count))]
    (when has-content?
      {:type :note
       :content (str "## Session Summary: " session "\n\n"
                     "### Completed Tasks: " task-count "\n" note-summaries
                     "\n\n### Commits: " (count git-commits) "\n" commit-summaries
                     (when (pos? kg-edge-count)
                       (str "\n\n### KG Connections: " kg-edge-count))
                     (when (pos? kanban-mvs-count)
                       (str "\n\n### Kanban Movements: " kanban-mvs-count)))
       :tags (into (session-tags) ["session-summary" "wrap-generated"])
       :duration :short})))

(defn summarize-session-progress
  "Summarize multiple progress notes into a session summary.
   Optional harvested map threads full harvest context (incl. :hivemind-messages)
   to extensions registered under :cc/summarize-progress."
  ([notes git-commits] (summarize-session-progress notes git-commits nil))
  ([notes git-commits harvested]
   (delegate :cc/summarize-progress summarize-session-progress-fallback [notes git-commits harvested])))

(m/=> summarize-session-progress [:function [:=> [:cat [:maybe [:sequential :any]] [:maybe [:sequential :any]]] [:maybe SummaryNote]] [:=> [:cat [:maybe [:sequential :any]] [:maybe [:sequential :any]] [:maybe [:map [:summary {:optional true} [:maybe [:map [:kg-edge-count {:optional true} number?] [:kanban-movement-count {:optional true} number?]]]]]]] [:maybe SummaryNote]]])

(defn- summarize-memory-activity-fallback [{:keys [created accessed]} & [_harvested]]
  (when (pos? (+ (or created 0) (or accessed 0)))
    {:type :note
     :content (str "## Session Summary: " (session-id) "\n\n"
                   "### Memory Activity\n"
                   "- Memories created: " (or created 0) "\n"
                   "- Memories accessed: " (or accessed 0) "\n")
     :tags (into (session-tags) ["session-summary" "wrap-generated" "coordinator"])
     :duration :short}))

(defn summarize-memory-activity
  "Produce a session summary from memory activity alone.
   Optional harvested map threads full harvest context (incl. :hivemind-messages)
   to extensions registered under :cc/summarize-memory."
  ([activity] (summarize-memory-activity activity nil))
  ([activity harvested]
   (delegate :cc/summarize-memory summarize-memory-activity-fallback [activity harvested])))

(m/=> summarize-memory-activity [:function [:=> [:cat [:maybe [:map [:created {:optional true} [:maybe number?]] [:accessed {:optional true} [:maybe number?]]]]] [:maybe SummaryNote]] [:=> [:cat [:maybe [:map [:created {:optional true} [:maybe number?]] [:accessed {:optional true} [:maybe number?]]]] :any] [:maybe SummaryNote]]])

(defn meaningful-harvest?
  "True iff the harvest shows write-shaped work: progress notes, completed
   tasks, commits, kg-edges, kanban moves, or memories created. Excludes
   accessed-count (reads)."
  [harvested]
  (let [{:keys [progress-count task-count commit-count
                kg-edge-count kanban-movement-count created-count]}
        (:summary harvested)]
    (boolean (some pos? [(or progress-count 0)
                         (or task-count 0)
                         (or commit-count 0)
                         (or kg-edge-count 0)
                         (or kanban-movement-count 0)
                         (or created-count 0)]))))

(m/=> meaningful-harvest? [:=> [:cat [:maybe [:map [:summary {:optional true} [:maybe [:map [:progress-count {:optional true} [:maybe number?]] [:task-count {:optional true} [:maybe number?]] [:commit-count {:optional true} [:maybe number?]] [:kg-edge-count {:optional true} [:maybe number?]] [:kanban-movement-count {:optional true} [:maybe number?]] [:created-count {:optional true} [:maybe number?]]]]]]]] :boolean])

(comment
  (calculate-promotion-score
   [{:context :explicit-reference :count 2}
    {:context :cross-session :count 1}
    {:context :catchup-structural :count 5}])

  (should-promote? {:duration :ephemeral
                    :recalls [{:context :explicit-reference :count 3}
                              {:context :cross-session :count 1}]}))
