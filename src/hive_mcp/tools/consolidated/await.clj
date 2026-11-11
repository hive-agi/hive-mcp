(ns hive-mcp.tools.consolidated.await
  "`await` verbs over hive-mcp.await.registry: create, list, cancel, fire.

   Mounted as the `swarm await` subdomain. A ling parks by creating an
   awaiter (source, filter, deadline, continuation prompt) and then ending
   its run; the awaiter's owner resumer continues the run on the wake.
   `fire` injects a :manual event, the reference source."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-mcp.await.domain :as domain]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.resumers.hive-agent-run]
            [hive-mcp.await.sources.manual :as manual]
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

(defn- ->kw
  "A keyword from a keyword or a string (a leading colon is dropped)."
  [x]
  (cond (keyword? x) x
        (blank->nil x) (keyword (let [s (str x)] (if (str/starts-with? s ":") (subs s 1) s)))
        :else nil))

(defn- ->long [x]
  (cond (integer? x) (long x)
        (string? x)  (parse-long (str/trim x))
        :else        nil))

(defn- keyword-keys
  "`m` with keyword keys (JSON objects may arrive with string keys)."
  [m]
  (when (map? m) (into {} (map (fn [[k v]] [(->kw k) v])) m)))

(defn create-request
  "Pure: the registry create request from MCP `params`, as a Result.
   Owner: `owner` (an object with `kind`), else `owner_kind` plus the flat
   `run_id`, `ling_id`, `cwd`, `model`, `provider`. Subscription: `source`
   (alias `type`, default manual), `filter` (object), `dedup_key`.
   Deadline: `deadline` (epoch ms) or `timeout_ms`. `prompt` is the
   continuation the owner resumes with; `id` is optional.
   Answers (ok request) or (err :await/bad-request {:message s})."
  [params]
  (let [owner (or (some-> (keyword-keys (:owner params)) (update :kind ->kw))
                  (when-let [k (->kw (:owner_kind params))]
                    (cond-> {:kind k}
                      (blank->nil (:run_id params))   (assoc :run-id (str (:run_id params)))
                      (blank->nil (:ling_id params))  (assoc :ling-id (str (:ling_id params)))
                      (blank->nil (:cwd params))      (assoc :cwd (str (:cwd params)))
                      (blank->nil (:model params))    (assoc :model (str (:model params)))
                      (blank->nil (:provider params)) (assoc :provider (str (:provider params))))))
        owner (some-> owner (dissoc nil) (update-keys (fn [k] (or ({:run_id :run-id :ling_id :ling-id} k) k))))
        source (or (->kw (:source params)) (->kw (:type params)) :manual)
        flt    (keyword-keys (:filter params))
        dedup  (->kw (:dedup_key params))
        deadline (->long (:deadline params))
        timeout  (->long (:timeout_ms params))]
    (cond
      (nil? (:kind owner))
      (r/err :await/bad-request {:message "await create: pass `owner` {kind ...} or `owner_kind`"})

      (and (some? (:timeout_ms params)) (not (and timeout (pos? timeout))))
      (r/err :await/bad-request {:message "await create: timeout_ms must be a positive integer"})

      :else
      (r/ok (cond-> {:owner owner
                     :subscription {:type source :filter flt :dedup-key dedup}
                     :prompt (blank->nil (:prompt params))}
              (blank->nil (:id params)) (assoc :id (str (:id params)))
              deadline (assoc :deadline deadline)
              (and (nil? deadline) timeout) (assoc :timeout-ms timeout))))))

(m/=> create-request [:=> [:cat [:maybe :map]] :map])

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

(defn handle-create
  "Create and arm an awaiter (see create-request for the params). Answers
   the armed awaiter and the instruction to end the run parked."
  [params]
  (run-verb "create"
            #(r/let-ok [req (create-request params)]
               (let [a (registry/create! (current-registry) req)]
                 (r/ok {:awaiter a
                        :next (str "Awaiter " (:id a) " is armed. End your run now (parked);"
                                   " the wake resumes it with your prompt and the wake payload.")})))))

(m/=> handle-create [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-list
  "Every awaiter (or only those with `status` armed|woken|cancelled), plus
   the registered sources and owner kinds, and the resume result of each
   woken one."
  [params]
  (run-verb "list"
            (fn []
              (let [reg    (current-registry)
                    status (->kw (:status params))
                    as     (cond->> (registry/awaiters reg)
                             status (filter #(= status (:status %))))]
                {:awaiters (mapv (fn [a]
                                   (cond-> a
                                     (= :woken (:status a))
                                     (assoc :resume (registry/resume-result reg (:id a)))))
                                 as)
                 :count    (count as)
                 :sources  (ports/known-sources)
                 :resumers (ports/known-resumers)
                 :statuses domain/statuses}))))

(m/=> handle-list [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-cancel
  "Cancel the armed awaiter `id`; its source and deadline are released and
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

(defn handle-fire
  "Inject one :manual event with `data` (an object, default {}) into the
   awaiter `id`, or into every listening :manual awaiter when no id is
   given; each awaiter's filter and dedup decide whether it wakes. Answers
   the ids offered the event and the ids that woke, with their resume
   results."
  [params]
  (run-verb "fire"
            (fn []
              (let [reg     (current-registry)
                    data    (or (keyword-keys (:data params)) {})
                    id      (blank->nil (:id params))
                    before  (set (map :id (filter domain/armed? (registry/awaiters reg))))
                    offered (manual/fire! data id)
                    woke    (filterv #(= :woken (:status (registry/lookup reg %))) offered)]
                {:offered offered
                 :woke    (mapv (fn [w] {:id w :resume (registry/resume-result reg w)})
                                (filter before woke))}))))

(m/=> handle-fire [:=> [:cat [:maybe :map]] tc/McpResponse])

(def handlers
  {:create #'handle-create
   :list   #'handle-list
   :cancel #'handle-cancel
   :fire   #'handle-fire})

(def tool-def
  {:name "await"
   :description "Park on an event instead of polling: create (arm an awaiter: source, filter, deadline, continuation prompt, owner; then end your run parked), list (awaiters, sources, owner kinds), cancel (id), fire (inject a manual event: id?, data)."
   :inputSchema {:type "object"
                 :properties {"owner"      {:type "object" :description "[await create] Who resumes on the wake: {kind, ...}; kind hive-agent-run takes run_id (chat-point run) and cwd"}
                              "owner_kind" {:type "string" :description "[await create] Flat alias for owner.kind (hive-agent-run)"}
                              "run_id"     {:type "string" :description "[await create] Chat-point run id the hive-agent-run resumer continues"}
                              "ling_id"    {:type "string" :description "[await create] The parked ling's id"}
                              "source"     {:type "string" :description "[await create] Event source type (default manual)"}
                              "filter"     {:type "object" :description "[await create] Every key must equal the event data's value"}
                              "dedup_key"  {:type "string" :description "[await create] Event data key whose value never wakes twice"}
                              "timeout_ms" {:type "integer" :description "[await create] Wake with reason timeout after this long"}
                              "deadline"   {:type "integer" :description "[await create] Absolute deadline, epoch ms"}
                              "data"       {:type "object" :description "[await fire] The manual event's data"}}}})

(def tools [tool-def])
