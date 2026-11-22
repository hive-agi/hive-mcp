(ns hive-mcp.tools.consolidated.inbox
  "`inbox` verbs over hive-mcp.nats.inbox: arm, send, read, status.

   Mounted as the `swarm inbox` subdomain. The inbox id is the stable
   coordinator name the caller passes as `inbox` (alias `id`); the
   HIVE_INBOX_ID environment variable is the default when neither is given.
   Every verb answers through the hive NATS client's live connection and
   refuses plainly, with the bus status, when there is none."
  (:require [clojure.string :as str]
            [hive-mcp.channel.audience :as audience]
            [hive-mcp.nats.client :as client]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.nats.inbox-stream :as inbox-stream]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [malli.core :as m]
            [hive-mcp.nats.sense-republish :as sense-republish]
            [hive-mcp.nats.sense-adapters :as sense-adapters]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- connection
  "The live jnats Connection. A var so tests can point it at a scratch broker."
  []
  (client/current-connection))

(defn- env-inbox [] (System/getenv "HIVE_INBOX_ID"))

(defn- blank->nil [s] (when-not (str/blank? (str s)) (str s)))

(m/=> blank->nil [:=> [:cat :any] [:maybe :string]])

(defn resolve-inbox-id
  "Pure over its inputs: the inbox a call addresses. `inbox`, then `id`, then
   the HIVE_INBOX_ID default."
  [params default]
  (or (blank->nil (:inbox params)) (blank->nil (:id params)) (blank->nil default)))

(m/=> resolve-inbox-id [:=> [:cat [:maybe :map] [:maybe :string]] [:maybe :string]])

(defn- ->int [x]
  (cond (integer? x) x
        (and (string? x) (re-matches #"\d+" x)) (Long/parseLong x)
        :else nil))

(defn- with-conn
  "Run (f conn) when the bus is connected; translate ex-info into an MCP
   error carrying its :error keyword."
  [verb f]
  (if-let [conn (connection)]
    (try (mcp-json (f conn))
         (catch clojure.lang.ExceptionInfo e
           (mcp-error (str "inbox " verb ": " (.getMessage e)
                           (when-let [k (:error (ex-data e))] (str " [" k "]")))))
         (catch Exception e
           (mcp-error (str "inbox " verb " failed: " (.getMessage e)))))
    (mcp-error (str "inbox " verb ": no connected event bus ("
                    (:summary (eb/backbone-status))
                    "). See `events bus-status`."))))

(defn- need-inbox
  "Resolve the addressed inbox, encode it to its canonical id
   (`coordinator:134462` -> `coordinator-134462`) and call (f id). An id that
   is no valid inbox id after encoding is refused with :inbox/invalid-id."
  [verb params f]
  (if-let [raw (resolve-inbox-id params (env-inbox))]
    (try (f (inbox/encode-inbox-id raw))
         (catch clojure.lang.ExceptionInfo e
           (if (= :inbox/invalid-id (:error (ex-data e)))
             (mcp-error (str "inbox " verb ": " (.getMessage e) " [:inbox/invalid-id]"))
             (throw e))))
    (mcp-error (str "inbox " verb ": pass `inbox` (your stable coordinator name)"
                    " or set HIVE_INBOX_ID"))))

(defn auto-arm!
  "Bind a coordinator caller to its own inbox without an explicit `inbox arm`.
   `caller` is the transport's caller id; only a coordinator lane that names a
   session (`coordinator:<pid>`) is armed, under its encoded id
   (`coordinator-<pid>`). Arms the durable, binds the caller so the senses of
   its lings are republished there, and starts the sense adapters. Never
   throws: -> {:armed? true :inbox id} | {:armed? false :reason kw}."
  [caller]
  (try
    (let [session (audience/coordinator-session caller)
          id      (when session (inbox/safe-encode-inbox-id (str "coordinator:" session)))
          conn    (when id (connection))]
      (cond
        (nil? id)   {:armed? false :reason :not-a-coordinator-session}
        (nil? conn) {:armed? false :reason :no-bus :inbox id}
        :else
        (do (inbox/arm! conn id)
            (sense-republish/bind-inbox! caller id)
            (sense-adapters/start!)
            {:armed? true :inbox id})))
    (catch Throwable t
      {:armed? false :reason :arm-failed :error (.getMessage t)})))

(m/=> auto-arm! [:=> [:cat :any] :map])

(defn- channel-port
  "The :9999 channel server's port when it runs, else nil. Resolved late so
   this ns does not load the HTTP server."
  []
  (when-let [st (try (requiring-resolve 'hive-mcp.channel.websocket/status)
                     (catch Throwable _ nil))]
    (when (:running? (st)) (:port (st)))))

(defn monitor-for
  "The Monitor spec of inbox `id` (hive-mcp.nats.inbox-stream/monitor-spec)
   on channel `port`, minting its token; {:available? false :reason ..}
   when no channel server runs."
  [id port]
  (if port
    (assoc (inbox-stream/monitor-spec {:port port :id id
                                       :token (inbox-stream/issue-token! id)})
           :available? true)
    {:available? false
     :reason "no channel server (services :channel) in this JVM: use `inbox read`"}))

(m/=> monitor-for [:=> [:cat :string [:maybe :int]] :map])

(defn handle-arm
  "Create or keep the durable for an inbox. Re-arm keeps the cursor. The
   caller (`_caller_id`) is bound to the inbox, so the terminal and attention
   senses of the lings it spawns are republished there
   (hive-mcp.nats.sense-republish)."
  [params]
  (need-inbox "arm" params
              (fn [id]
                (with-conn "arm"
                  (fn [conn]
                    (let [armed  (inbox/arm! conn id)
                          caller (blank->nil (:_caller_id params))]
                      (when caller (sense-republish/bind-inbox! caller id))
                      (sense-adapters/start!)
                      (cond-> (assoc armed
                                     :monitor (monitor-for id (channel-port))
                                     :how-to-receive
                                     (str "Push: start a Monitor with :monitor :command (one SSE"
                                          " `data:` JSON line per message, acked after it is written)."
                                          " Pull: inbox read inbox=" id " (wait_ms long-polls)."))
                        caller (assoc :senses-from-lings-of
                                      (sense-republish/binding-key caller)))))))))

(defn handle-send
  "Publish `body` (alias `message`) to inbox `to`."
  [{:keys [to body message from meta] :as params}]
  (let [to   (blank->nil to)
        body (if (some? body) body message)]
    (cond
      (nil? to)   (mcp-error "inbox send: `to` (target inbox id) is required")
      (nil? body) (mcp-error "inbox send: `body` (or `message`) is required")
      :else
      (with-conn "send"
        (fn [conn]
          (inbox/send! conn (cond-> {:to to :body body
                                     :from (or (blank->nil from)
                                               (resolve-inbox-id params (env-inbox)))}
                              (map? meta) (assoc :meta meta)
                              (blank->nil (:msg_id params)) (assoc :id (:msg_id params)))))))))

(defn handle-read
  "Pull and ack up to `n` messages; `wait_ms` long-polls for the first one."
  [params]
  (need-inbox "read" params
              (fn [id]
                (with-conn "read"
                  #(inbox/read! % id {:n (->int (or (:n params) (:limit params)))
                                      :wait-ms (->int (or (:wait_ms params) (:wait-ms params)))})))))

(defn handle-status
  "One inbox's cursor, or every armed inbox when none is named, with the
   sense republisher's state (bindings, counts, degraded?)."
  [params]
  (let [id (resolve-inbox-id params (env-inbox))]
    (with-conn "status" #(cond-> (assoc (inbox/status % id)
                                        :sense-republish (sense-republish/status))
                           (inbox/safe-encode-inbox-id id)
                           (assoc :reader (inbox-stream/stream-status
                                           (inbox/safe-encode-inbox-id id)))))))

(def ^:private Reply hive-mcp.tools.core/McpResponse)
(m/=> handle-arm    [:=> [:cat [:maybe :map]] Reply])
(m/=> handle-send   [:=> [:cat [:maybe :map]] Reply])
(m/=> handle-read   [:=> [:cat [:maybe :map]] Reply])
(m/=> handle-status [:=> [:cat [:maybe :map]] Reply])

(def handlers
  {:arm    #'handle-arm
   :send   #'handle-send
   :read   #'handle-read
   :status #'handle-status})

(def tool-def
  {:name "inbox"
   :description "Session inbox on HIVE_INBOX (JetStream): arm (ensure your durable; re-arm keeps the cursor), send (to=<inbox> body), read (pull+ack, n, wait_ms), status (cursor, pending)."
   :inputSchema {:type "object"
                 :properties {"inbox"   {:type "string" :description "[arm/read/status] Your stable inbox id (coordinator name). Default: HIVE_INBOX_ID"}
                              "to"      {:type "string" :description "[send] Target inbox id"}
                              "body"    {:description "[send] Message body (any JSON value)"}
                              "from"    {:type "string" :description "[send] Sender inbox id (default: inbox)"}
                              "meta"    {:type "object" :description "[send] Extra metadata"}
                              "msg_id"  {:type "string" :description "[send] Idempotency id; a resend inside 2 min is stored once"}
                              "n"       {:type "integer" :description "[read] Max messages (default 10, max 256)"}
                              "wait_ms" {:type "integer" :description "[read] Long-poll for the first message, ms (max 30000)"}}}})

(def tools [tool-def])
