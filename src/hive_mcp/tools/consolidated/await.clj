(ns hive-mcp.tools.consolidated.await
  "`await` verbs over hive-mcp.await.registry: create, arm, list, cancel,
   fire.

   Mounted as the `swarm await` subdomain. A ling parks by creating an
   awaiter (source, filter, deadline, continuation prompt, owner) and then
   ending its run :parked; the awaiter stays :pending until the park is
   observed, which is what stops a wake from resuming a run that is still
   turning. `arm` does that promotion by hand (hive-agent's park path calls
   the registry directly), `fire` injects a :manual event, the reference
   source."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-mcp.await.domain :as domain]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.resumers.hive-agent-run]
            [hive-mcp.await.resumers.session]
            [hive-mcp.await.sources.manual :as manual]
            [hive-mcp.await.sources.poll :as poll]
            [hive-mcp.await.sources.sql-predicate]
            [hive-mcp.await.sources.k8s-log-regex]
            [hive-mcp.await.sources.prom-predicate]
            [hive-mcp.channel.audience :as audience]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.tools.core :as tc :refer [mcp-json mcp-error]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- current-registry
  "The registry the verbs act on. A var so tests can point it at a temp one."
  []
  (registry/default-registry))

(defn- blank->nil [s] (when-not (str/blank? (str s)) (str s)))

(m/=> blank->nil [:=> [:cat :any] [:maybe :string]])

(defn- ->kw
  "A keyword from a keyword or a string (a leading colon is dropped)."
  [x]
  (cond (keyword? x) x
        (blank->nil x) (keyword (let [s (str x)] (if (str/starts-with? s ":") (subs s 1) s)))
        :else nil))

(m/=> ->kw [:=> [:cat :any] [:maybe :keyword]])

(defn- ->long [x]
  (cond (integer? x) (long x)
        (string? x)  (parse-long (str/trim x))
        :else        nil))

(m/=> ->long [:=> [:cat :any] [:maybe :int]])

(defn- ->bool [x]
  (cond (boolean? x) x
        (string? x)   ({"true" true "false" false} (str/lower-case (str/trim x)))
        :else         nil))

(defn- keyword-keys
  "`m` with keyword keys (JSON objects may arrive with string keys)."
  [m]
  (when (map? m) (into {} (map (fn [[k v]] [(->kw k) v])) m)))

(m/=> keyword-keys [:=> [:cat :any] [:maybe [:map-of [:maybe :keyword] :any]]])

(def ^:private owner-string-keys
  "Flat owner params that are strings, and the Owner key each one lands on."
  {:run_id :run-id :ling_id :ling-id :cwd :cwd :model :model :provider :provider
   :parent :parent :spawned_by :spawned-by :project_id :project-id
   :spawn_mode :spawn-mode :kanban_task_id :kanban-task-id
   :inbox :inbox :label :label :session :session})

(defn- flat-owner
  "The owner map a flat `owner_kind` plus its sibling params describe, or nil
   when no `owner_kind` was given. Carries the successor-inheritance keys
   (review B5) when the caller names them."
  [params]
  (when-let [k (->kw (:owner_kind params))]
    (let [base (reduce-kv (fn [acc flat k'] 
                            (if-let [v (blank->nil (get params flat))]
                              (assoc acc k' (str v))
                              acc))
                          {} owner-string-keys)]
      (cond-> (assoc base :kind k)
        (some? (:sandbox params))       (assoc :sandbox (->bool (:sandbox params)))
        (some? (:max_budget_usd params)) (assoc :max-budget-usd (double (:max_budget_usd params)))
        (some? (:token_budget params))  (assoc :token-budget (->long (:token_budget params)))
        (some? (:time_budget_ms params)) (assoc :time-budget-ms (->long (:time_budget_ms params)))
        (keyword-keys (:grant params))  (assoc :grant (keyword-keys (:grant params)))
        (coll? (:presets params))       (assoc :presets (mapv str (:presets params)))
        (keyword-keys (:turn_budget params)) (assoc :turn-budget (keyword-keys (:turn_budget params)))))))

(defn default-filter
  "The subscription filter an await create gets: the caller's `flt` when it
   names one; otherwise, for a :manual source whose `owner` names a ling,
   {:to <ling-id>}. That addresses a broadcast `await fire` (one naming no
   awaiter id) to that ling only; before, an unfiltered manual awaiter
   matched every broadcast, and one coordinator's fire woke every other
   coordinator's lings (friction 20261122150418-7fef4903). An awaiter left
   with no filter hears only a fire that names its id. Pure."
  [source flt owner]
  (or flt
      (when-let [ling (and (= :manual source) (blank->nil (:ling-id owner)))]
        {:to ling})))

(m/=> default-filter [:=> [:cat [:maybe :keyword] [:maybe :map] [:maybe :map]] [:maybe :map]])

(defn session-inbox
  "The inbox a :session owner's wake goes to when the caller names none, as
   a Result: (ok id) or (err :await/bad-request {:message s}). Pure.

   1. a coordinator caller that names a session (`coordinator:<session>`)
      gets its own inbox, `coordinator-<session>` (the id `inbox auto-arm!`
      binds);
   2. a bare coordinator caller (`coordinator`, no session token) is
      REFUSED: `env-inbox` is the server process's HIVE_INBOX_ID, shared by
      every coordinator on that server, so falling back to it could hand
      the wake to another coordinator (review item 4);
   3. a caller outside the coordinator lane (a script, a peer) falls back
      to `env-inbox`, when set and valid."
  [caller env-inbox]
  (let [session (audience/coordinator-session caller)
        coord?  (and (some? caller) (audience/coordinator-reader? caller))
        env     (blank->nil env-inbox)]
    (cond
      session
      (if-let [id (inbox/safe-encode-inbox-id (str "coordinator:" session))]
        (r/ok id)
        (r/err :await/bad-request {:message (str "await create: session " (pr-str session)
                                                 " has no valid inbox id; pass `inbox`")}))

      coord?
      (r/err :await/bad-request
             {:message (str "await create: owner_kind=session from a coordinator caller with no"
                            " session token (" (pr-str (str caller)) "); pass `inbox` explicitly."
                            " HIVE_INBOX_ID is not used: it is shared by every coordinator on"
                            " this server.")})

      (and env (inbox/safe-encode-inbox-id env))
      (r/ok (inbox/safe-encode-inbox-id env))

      :else
      (r/err :await/bad-request
             {:message "await create: owner_kind=session needs `inbox` (no caller session and no HIVE_INBOX_ID)"}))))

(m/=> session-inbox [:=> [:cat :any [:maybe :string]] :map])

(defn complete-owner
  "`owner` completed, as a Result. A :session owner's :inbox is the given
   one canonicalised (`coordinator:777` -> `coordinator-777`), refused when
   it cannot be encoded to a valid inbox id (review B2), else filled by
   session-inbox; :session is filled from the caller's session token. Other
   kinds are answered unchanged. Pure."
  [owner caller env-inbox]
  (if (not= :session (:kind owner))
    (r/ok owner)
    (let [given (blank->nil (:inbox owner))
          ib    (if given
                  (if-let [id (inbox/safe-encode-inbox-id (str/trim given))]
                    (r/ok id)
                    (r/err :await/bad-request
                           {:message (str "await create: inbox " (pr-str given) " is not a valid"
                                          " inbox id (1-64 letters, digits, '-' or '_'; a ':' is"
                                          " written '-')")}))
                  (session-inbox caller env-inbox))
          sess  (audience/coordinator-session caller)]
      (r/let-ok [id ib]
        (r/ok (cond-> (assoc owner :inbox id)
                (and (nil? (:session owner)) sess) (assoc :session sess)))))))

(m/=> complete-owner [:=> [:cat [:maybe :map] :any [:maybe :string]] :map])


(defn create-request
  "Pure: the registry create request from MCP `params`, as a Result.

   Owner: `owner` (an object with `kind`), else `owner_kind` plus the flat
   `run_id`, `ling_id`, `cwd`, `model`, `provider`, `parent`, `spawned_by`,
   `project_id`, `spawn_mode`, `kanban_task_id`, `sandbox`, `grant`,
   `presets`, `turn_budget`, `token_budget`, `time_budget_ms`,
   `max_budget_usd` (the authority a successor inherits), and for the
   :session/:journal kinds `inbox`, `label`, `session`.
   Subscription: `source` (alias `type`, default manual), `filter` (object;
   a manual awaiter whose owner names a ling defaults to {:to <ling-id>},
   see default-filter), `dedup_key`. Deadline: `deadline` (epoch ms) or
   `timeout_ms`. `prompt` is the continuation the owner resumes with; `id`
   is optional.

   `run_id` is NOT required: a :hive-agent-run owner binds it when it parks
   (arm). A :session owner's inbox is completed by complete-owner;
   `env-inbox` is the server's HIVE_INBOX_ID, passed in by the handler and
   never read from the wire (review item 5).
   Answers (ok request) or (err :await/bad-request {:message s})."
  ([params] (create-request params nil))
  ([params env-inbox]
   (let [owner (or (some-> (keyword-keys (:owner params)) (update :kind ->kw))
                   (flat-owner params))
         owner (some-> owner (dissoc nil) (update-keys
                                           (fn [k] (or ({:run_id :run-id :ling_id :ling-id
                                                         :project_id :project-id
                                                         :spawned_by :spawned-by
                                                         :spawn_mode :spawn-mode
                                                         :kanban_task_id :kanban-task-id} k) k))))
         owner* (when (:kind owner) (complete-owner owner (:_caller_id params) env-inbox))
         owner  (if (r/ok? owner*) (:ok owner*) owner)
         source (or (->kw (:source params)) (->kw (:type params)) :manual)
         flt    (default-filter source (not-empty (keyword-keys (:filter params))) owner)
         dedup  (->kw (:dedup_key params))
         deadline (->long (:deadline params))
         timeout  (->long (:timeout_ms params))
         poll-ms  (->long (:poll_ms params))]
     (cond
       (nil? (:kind owner))
       (r/err :await/bad-request {:message "await create: pass `owner` {kind ...} or `owner_kind`"})

       (r/err? owner*)
       owner*

       (and (some? (:timeout_ms params)) (not (and timeout (pos? timeout))))
       (r/err :await/bad-request {:message "await create: timeout_ms must be a positive integer"})

       (and (some? (:poll_ms params)) (not (and poll-ms (pos? poll-ms))))
       (r/err :await/bad-request {:message "await create: poll_ms must be a positive integer"})

       (and (some? (:spec params)) (not (map? (:spec params))))
       (r/err :await/bad-request {:message "await create: spec must be an object"})

       :else
       (r/ok (cond-> {:owner owner
                      :subscription (cond-> {:type source :filter flt :dedup-key dedup}
                                      (map? (:spec params)) (assoc :spec (poll/kw-keys (:spec params)))
                                      poll-ms (assoc :poll-ms poll-ms))
                      :prompt (blank->nil (:prompt params))}
               (blank->nil (:id params)) (assoc :id (str (:id params)))
               deadline (assoc :deadline deadline)
               (and (nil? deadline) timeout) (assoc :timeout-ms timeout)))))))

(m/=> create-request [:function [:=> [:cat [:maybe :map]] :map]
                      [:=> [:cat [:maybe :map] [:maybe :string]] :map]])

(defn- run-verb
  "Run (f) for `verb`, turning a thrown ex-info or an err Result into an MCP
   error that names the verb and the :error keyword; an ok Result or a plain
   value answers as JSON."
  [verb f]
  (try
    (let [v (f)]
      (cond
        (r/err? v) (mcp-error (str "await " verb ": " (or (:message v) (pr-str (:error v)))
                                   " [" (:error v) "]"))
        (r/ok? v)  (mcp-json (:ok v))
        :else      (mcp-json v)))
    (catch clojure.lang.ExceptionInfo e
      (mcp-error (str "await " verb ": " (.getMessage e)
                      (when-let [k (:error (ex-data e))] (str " [" k "]")))))
    (catch Throwable e
      (mcp-error (str "await " verb " failed: " (.getMessage e))))))

(m/=> run-verb [:=> [:cat :string ifn?] tc/McpResponse])

(defn handle-create
  "Create a :pending awaiter (see create-request for the params). Answers
   the awaiter and the instruction to end the run parked: the awaiter arms
   itself when the park is observed, and a wake that arrives before then is
   recorded, not lost."
  [params]
  (run-verb "create"
            #(r/let-ok [req (create-request params (System/getenv "HIVE_INBOX_ID"))]
               (let [a (registry/create! (current-registry) req)]
                 (r/ok {:awaiter a
                        :next (case (get-in a [:owner :kind])
                                :session (str "Awaiter " (:id a) " is armed. Its wake is sent to inbox "
                                              (get-in a [:owner :inbox]) " (swarm inbox read inbox="
                                              (get-in a [:owner :inbox]) ") and journaled; keep working. Woken means stored in the inbox"
                                              " stream, not read: arm that inbox (swarm inbox arm) to receive it.")
                                :journal (str "Awaiter " (:id a) " is armed. Its wake is only journaled;"
                                              " read it with swarm await list.")
                                (str "Awaiter " (:id a) " is pending. End your run now"
                                     " (parked) with awaiter_id=" (:id a) "; it arms when the"
                                     " park is observed and the wake resumes the run."))})))))

(m/=> handle-create [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-arm
  "Arm the :pending awaiter `id`, binding `run_id` as the parked run's
   chat-point id, and fire the wake it recorded while pending. Answers the
   awaiter and whether a recorded wake fired. Idempotent.

   hive-agent's park path calls the registry directly; this verb is the same
   promotion for a caller outside that path (a script, a test, a peer)."
  [params]
  (run-verb "arm"
            (fn []
              (let [reg (current-registry)
                    id  (blank->nil (:id params))
                    run (blank->nil (:run_id params))]
                (if-not id
                  (r/err :await/bad-request {:message "pass `id`"})
                  (if-let [res (registry/arm! reg id run)]
                    (r/ok {:awaiter (:awaiter res) :fired? (:fired? res)
                           :pending-wake (:pending-wake res)})
                    (r/err :await/unknown-awaiter {:message (str "no awaiter " id)})))))))

(m/=> handle-arm [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-list
  "Every awaiter (or only those with `status` pending|armed|waking|woken|
   cancelled), plus the registered sources and owner kinds, and the resume
   result of each woken or waking one."
  [params]
  (run-verb "list"
            (fn []
              (let [reg    (current-registry)
                    status (->kw (:status params))
                    as     (cond->> (registry/awaiters reg)
                             status (filter #(= status (:status %))))]
                {:awaiters (mapv (fn [a]
                                   (cond-> a
                                     (contains? #{:woken :waking} (:status a))
                                     (assoc :resume (registry/resume-result reg (:id a)))))
                                 as)
                 :count    (count as)
                 :sources  (ports/known-sources)
                 :resumers (ports/known-resumers)
                 :statuses domain/statuses}))))

(m/=> handle-list [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-cancel
  "Cancel the live awaiter `id`; its source and deadline are released and
   its owner is never resumed."
  [params]
  (run-verb "cancel"
            (fn []
              (if-let [id (blank->nil (:id params))]
                (if-let [a (registry/cancel! (current-registry) id)]
                  (r/ok {:awaiter a})
                  (r/err :await/unknown-awaiter {:message (str "no awaiter " id)}))
                (r/err :await/bad-request {:message "pass `id`"})))))

(m/=> handle-cancel [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn addressed-data
  "The data a fire delivers. A fire that names awaiter `target` (looked up,
   or nil) is addressed to it, so a missing :to is filled from the target's
   filter :to: naming the id is enough to reach an awaiter whose filter was
   defaulted to {:to <ling-id>}. Every other key still has to match. Pure."
  [data target]
  (let [to (get-in target [:subscription :filter :to])]
    (if (and (some? to) (not (contains? data :to)))
      (assoc data :to to)
      data)))

(m/=> addressed-data [:=> [:cat :map [:maybe :map]] :map])

(defn handle-fire
  "Inject one :manual event with `data` (an object, default {}) into the
   awaiter `id` (its filter :to is filled in when `data` names none, see
   addressed-data), or, when no id is given, into every listening :manual
   awaiter that has a filter; each awaiter's filter and dedup decide whether
   it wakes. An awaiter with no filter hears only a fire naming its id. Answers
   the ids offered the event and the ids that woke, with their resume
   results. An :armed awaiter wakes; a :pending one RECORDS the wake."
  [params]
  (run-verb "fire"
            (fn []
              (let [reg     (current-registry)
                    id      (blank->nil (:id params))
                    data    (addressed-data (or (keyword-keys (:data params)) {})
                                            (when id (registry/lookup reg id)))
                    before  (set (map :id (filter domain/live? (registry/awaiters reg))))
                    offered (manual/fire! data id)
                    status  (fn [wid] (:status (registry/lookup reg wid)))
                    woke    (filterv #(contains? #{:woken :waking} (status %)) offered)]
                {:offered offered
                 :woke    (mapv (fn [w] {:id w :resume (registry/resume-result reg w)})
                                (filter before woke))
                 :pending (mapv (fn [w] {:id w :pending-wake (:pending-wake (registry/lookup reg w))})
                                (filter #(= :pending (status %)) offered))}))))

(m/=> handle-fire [:=> [:cat [:maybe :map]] tc/McpResponse])

(def handlers
  {:create #'handle-create
   :arm    #'handle-arm
   :list   #'handle-list
   :cancel #'handle-cancel
   :fire   #'handle-fire})

(def tool-def
  {:name "await"
   :description "Park on an event instead of polling: create (a PENDING awaiter: source, filter, deadline, continuation prompt, owner; then end your run parked with awaiter_id — it arms when the park is observed and a wake arriving before then is recorded, not lost), arm (id, run_id?: promote a pending awaiter now, the same promotion hive-agent's park performs), list (awaiters, sources, owner kinds), cancel (id), fire (inject a manual event: id?, data; without id only FILTERED awaiters hear it, so address a ling with data.to=<ling_id>)."
   :inputSchema {:type "object"
                 :properties {"owner"      {:type "object" :description "[await create] Who resumes on the wake: {kind, ...}; kind hive-agent-run takes cwd plus the authority to inherit"}
                              "owner_kind" {:type "string" :description "[await create] Flat alias for owner.kind: hive-agent-run (a parked ling, resumed by a successor), session (an app coordinator session: the wake is sent to its hive inbox and journaled; armed at create), journal (nobody resumed, the wake is only journaled; armed at create)"}
                              "inbox"      {:type "string" :description "[await create] session owner: the inbox the wake is sent to (default: the calling coordinator session's own inbox, else HIVE_INBOX_ID)"}
                              "label"      {:type "string" :description "[await create] session/journal owner: a label kept on the wake and its journal entry"}
                              "session"    {:type "string" :description "[await create] session owner: the coordinator session token the awaiter belongs to (default: the calling coordinator's session); recorded on the owner, does not route the wake"}
                              "run_id"     {:type "string" :description "[await create/arm] Chat-point run id the hive-agent-run resumer continues (bound at arm when the run parks)"}
                              "ling_id"    {:type "string" :description "[await create] The parked ling's id (also names the successor)"}
                              "cwd"        {:type "string" :description "[await create] Absolute directory the parked run lives in"}
                              "parent"     {:type "string" :description "[await create] Spawned-by parent of the successor"}
                              "spawned_by" {:type "string" :description "[await create] Caller of the parked ling, replayed on the successor"}
                              "project_id" {:type "string" :description "[await create] Project scope of the successor"}
                              "kanban_task_id" {:type "string" :description "[await create] Kanban card the successor keeps"}
                              "sandbox"    {:type "boolean" :description "[await create] Run the successor sandboxed (inherit the parked ling's value)"}
                              "grant"      {:type "object" :description "[await create] Grant of the successor (inherit the parked ling's attenuated grant)"}
                              "presets"    {:type "array" :items {:type "string"} :description "[await create] Presets of the successor"}
                              "turn_budget" {:type "object" :description "[await create] Turn lease of the successor"}
                              "token_budget" {:type "integer" :description "[await create] Context-reconstruction budget of the successor"}
                              "time_budget_ms" {:type "integer" :description "[await create] Wall-clock budget of the successor"}
                              "max_budget_usd" {:type "number" :description "[await create] Spend cap of the successor"}
                              "source"     {:type "string" :description "[await create] Event source type: manual (default), sql-predicate (poll a read-only Postgres SELECT: spec {connection_ref pass:<path>|env:<NAME>, query with $1.., params [..], predicate {kind exists|count|scalar, op, value, column?}, row_cap?, statement_timeout_ms?}), k8s-log-regex (poll kubectl logs: spec {namespace, pod|selector, container?, context?, regex, lookback_ms?, tail?}), prom-predicate (poll PromQL: spec {query, op, threshold, match any|all, url?}). Polling sources REQUIRE a deadline or timeout_ms"}
                              "spec"       {:type "object" :description "[await create] Source-specific spec of a polling source (see source)"}
                              "poll_ms"    {:type "integer" :description "[await create] Poll interval of a polling source (minimum: sql-predicate 30000, k8s-log-regex and prom-predicate 10000)"}
                              "filter"     {:type "object" :description "[await create] Every key must equal the event data's value. Default for a manual awaiter naming ling_id: {to: <ling_id>}; an awaiter with no filter wakes only on a fire naming its id"}
                              "dedup_key"  {:type "string" :description "[await create] Event data key whose value never wakes twice"}
                              "timeout_ms" {:type "integer" :description "[await create] Wake with reason timeout after this long"}
                              "deadline"   {:type "integer" :description "[await create] Absolute deadline, epoch ms"}
                              "data"       {:type "object" :description "[await fire] The manual event's data"}}}})

(def tools [tool-def])
