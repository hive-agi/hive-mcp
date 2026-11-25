(ns hive-mcp.nats.inbox-stream
  "Push endpoint for session inboxes: GET /inbox/<id>.sse?token=<t> on the
   :9999 channel server, one Server-Sent Events stream per inbox, for Claude
   Code's Monitor tool and a plugin monitor (card 20261116182711-093a70b4,
   design 20261102174419-54683d6d addendum (a)).

   - Loopback only (the remote address and any Origin), and a per-inbox
     token (Q6) minted by `inbox arm`. The :9999 firehose leaked other
     sessions; this is scoped to ONE inbox.
   - hive pulls the inbox's durable (hive-mcp.nats.inbox/fetch!), writes one
     `data:` line per message and acks ONLY after the write was taken by the
     connection. A failed or timed-out write naks the message and every
     later one of the batch, so a reconnect gets them again, in order, with
     no duplicate of what was acked.
   - A comment heartbeat after every empty poll keeps the connection alive
     and finds dead readers. Frames are capped at 1 MiB.
   - Reader liveness per inbox (`stream-status`): readers now, last write,
     and a re-arm `reminder` once an inbox whose Monitor was issued has had
     no reader for more than 60 s (Q9).

   Strata (CPPB):
     Value objects  Monitor, StreamStatus
     Pure           parse-route, parse-query, loopback?, token=, frame,
                    heartbeat, monitor-spec, reminder
     Ports          source: (fn [inbox {:keys [n wait-ms]}]) -> [Fetched]
                    (hive-mcp.nats.inbox/Fetched); default the hive NATS
                    client's live connection
     Boundary       token registry (issue-token!), reader registry,
                    configure!, handler, stream-status"
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.nats.client :as client]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.resilience.swallow :as swallow]
            [malli.core :as m]
            [manifold.stream :as s]
            [taoensso.timbre :as log])
  (:import [java.net InetAddress URLDecoder]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest SecureRandom]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value objects
;; =============================================================================

(def max-frame-bytes
  "The largest `data:` payload written, in UTF-8 bytes (1 MiB)."
  (* 1024 1024))

(def no-reader-reminder-ms
  "An issued Monitor with no reader for this long earns a reminder."
  60000)

(def Monitor
  "What `inbox arm` hands a coordinator to start its Monitor."
  [:map
   [:url :string]
   [:command :string]
   [:plugin-command :string]
   [:timeout_ms :int]
   [:description :string]])

(def StreamStatus
  [:map
   [:inbox :string]
   [:readers :int]
   [:monitor-issued? :boolean]
   [:delivered {:optional true} :int]
   [:connected-at {:optional true} [:maybe :int]]
   [:last-write-at {:optional true} [:maybe :int]]
   [:last-left-at {:optional true} [:maybe :int]]
   [:no-reader-ms {:optional true} [:maybe :int]]
   [:reminder {:optional true} [:maybe :string]]])

;; =============================================================================
;; Pure
;; =============================================================================

(def ^:private route-re #"^/inbox/([^/]+)\.sse$")

(defn parse-route
  "The inbox id a request path addresses (`/inbox/<id>.sse`), or nil."
  [uri]
  (when (string? uri)
    (second (re-matches route-re uri))))

(m/=> parse-route [:=> [:cat :any] [:maybe :string]])

(defn parse-query
  "A query string as {name value}, URL-decoded; nil -> {}."
  [qs]
  (if (str/blank? qs)
    {}
    (into {}
          (keep (fn [pair]
                  (let [[k v] (str/split pair #"=" 2)]
                    (when-not (str/blank? k)
                      [(URLDecoder/decode ^String k StandardCharsets/UTF_8)
                       (URLDecoder/decode ^String (or v "") StandardCharsets/UTF_8)]))))
          (str/split qs #"&"))))

(m/=> parse-query [:=> [:cat [:maybe :string]] [:map-of :string :string]])

(defn loopback?
  "Is `addr` (an IP literal, as a ring :remote-addr) a loopback address?
   nil or a non-literal is not."
  [addr]
  (boolean
   (when (and (string? addr) (re-matches #"[0-9A-Fa-f:.]+(%[0-9A-Za-z]+)?" addr))
     (swallow/attempt (.isLoopbackAddress (InetAddress/getByName ^String addr))))))

(m/=> loopback? [:=> [:cat :any] :boolean])

(defn- loopback-origin? [origin]
  (or (nil? origin)
      (boolean (re-matches #"^https?://(localhost|127\.0\.0\.1|\[::1\])(:\d+)?/?$" origin))))

(defn token=
  "Constant-time equality of two token strings; false when either is nil."
  [a b]
  (boolean
   (and (string? a) (string? b)
        (MessageDigest/isEqual (.getBytes ^String a StandardCharsets/UTF_8)
                               (.getBytes ^String b StandardCharsets/UTF_8)))))

(m/=> token= [:=> [:cat :any :any] :boolean])

(defn- utf8-len [^String s] (alength (.getBytes s StandardCharsets/UTF_8)))

(defn frame
  "One SSE event for delivered message `msg` (hive-mcp.nats.inbox/Delivered):
   `id: <seq>`, `event: inbox`, and ONE `data:` line of JSON. A payload over
   `max-frame-bytes` has its body replaced by a prefix of the body's JSON
   and `truncated: true`, so a frame never exceeds the cap."
  ([msg] (frame msg max-frame-bytes))
  ([msg cap]
   (let [data (json/write-str msg)
         data (if (<= (utf8-len data) cap)
                data
                (let [body  (json/write-str (:body msg))
                      room  (max 0 (- cap 512))
                      cut   (subs body 0 (min (count body) (quot room 4)))]
                  (json/write-str (assoc msg :body cut :truncated true
                                         :original-bytes (utf8-len data)))))]
     (str "id: " (:seq msg) "\nevent: inbox\ndata: " data "\n\n"))))

(m/=> frame [:function
             [:=> [:cat :map] :string]
             [:=> [:cat :map pos-int?] :string]])

(defn heartbeat
  "An SSE comment line: ignored by readers, proves the connection is live."
  [now]
  (str ": hb " now "\n\n"))

(m/=> heartbeat [:=> [:cat :int] :string])

(defn monitor-spec
  "The Monitor a coordinator starts for inbox `id` on channel `port` with
   `token`: a one-shot curl for Claude Code's Monitor tool, and a
   reconnecting loop for a plugin monitor (monitors.json)."
  [{:keys [port id token]}]
  (let [url (str "http://127.0.0.1:" port "/inbox/" id ".sse?token=" token)
        cmd (str "curl -sN --fail '" url "'")]
    {:url            url
     :command        cmd
     :plugin-command (str "sh -c 'while :; do " (str/replace cmd "'" "") "; sleep 2; done'")
     :timeout_ms     3600000
     :description    (str "hive inbox " id ": one `data:` JSON line per message"
                          " (acked after it is written); `: hb` lines are heartbeats")}))

(m/=> monitor-spec [:=> [:cat [:map [:port :int] [:id :string] [:token :string]]] Monitor])

(defn reminder
  "The re-arm reminder for `entry` at `now`, or nil: only when a Monitor was
   issued, no reader is connected, and none has been for longer than
   `no-reader-reminder-ms` (since it left, else since the issue)."
  [id {:keys [issued-at readers last-left-at]} now]
  (when (and issued-at (zero? (or readers 0)))
    (let [since (- now (or last-left-at issued-at))]
      (when (> since no-reader-reminder-ms)
        (str "inbox " id " has had no Monitor reader for " (quot since 1000) " s: "
             "start the Monitor from `inbox arm`'s :monitor spec (re-arm after a "
             "hive-mcp restart), or pull with `inbox read`.")))))

(m/=> reminder [:=> [:cat :string :map :int] [:maybe :string]])

;; =============================================================================
;; Boundary: tokens and readers
;; =============================================================================

(defonce ^:private registry* (atom {}))

(defonce ^:private rng (SecureRandom.))

(defn- new-token []
  (let [b (byte-array 24)]
    (.nextBytes ^SecureRandom rng b)
    (apply str (map #(format "%02x" (bit-and % 0xff)) b))))

(defn issue-token!
  "The token of inbox `id`, minted on first use and kept for the life of
   the process (a hive-mcp restart mints a new one: re-arm). Marks the
   inbox's Monitor as issued."
  [id]
  (let [id (inbox/encode-inbox-id id)]
    (-> (swap! registry* update id
               (fn [e] (cond-> (or e {:readers 0 :delivered 0})
                         (nil? (:token e)) (assoc :token (new-token))
                         true (assoc :issued-at (or (:issued-at e) (System/currentTimeMillis))))))
        (get-in [id :token]))))

(m/=> issue-token! [:=> [:cat :string] :string])

(defn forget-tokens!
  "Drop every token and reader record (tests)."
  []
  (reset! registry* {})
  nil)

(m/=> forget-tokens! [:=> [:cat] :nil])

(defn stream-status
  "Reader liveness of inbox `id` at `now` (default: the clock)."
  ([id] (stream-status id (System/currentTimeMillis)))
  ([id now]
   (let [id (inbox/encode-inbox-id id)
         e  (get @registry* id)]
     (cond-> {:inbox id :readers (or (:readers e) 0) :monitor-issued? (boolean (:token e))}
       e (assoc :delivered (or (:delivered e) 0)
                :connected-at (:connected-at e)
                :last-write-at (:last-write-at e)
                :last-left-at (:last-left-at e)
                :no-reader-ms (when (and (:issued-at e) (zero? (or (:readers e) 0)))
                                (- now (or (:last-left-at e) (:issued-at e))))
                :reminder (reminder id e now))))))

(m/=> stream-status [:function
                     [:=> [:cat :string] StreamStatus]
                     [:=> [:cat :string :int] StreamStatus]])

(defn- reader-joined! [id]
  (swap! registry* update id #(-> (or % {:readers 0 :delivered 0})
                                  (update :readers (fnil inc 0))
                                  (assoc :connected-at (System/currentTimeMillis)))))

(defn- reader-left! [id]
  (swap! registry* update id #(-> %
                                  (update :readers (fn [n] (max 0 (dec (or n 1)))))
                                  (assoc :last-left-at (System/currentTimeMillis)))))

(defn- wrote! [id]
  (swap! registry* update id #(-> % (update :delivered (fnil inc 0))
                                  (assoc :last-write-at (System/currentTimeMillis)))))

;; =============================================================================
;; Boundary: the source port and configuration
;; =============================================================================

(defn nats-source
  "The default source: `inbox/fetch!` over the hive client's live
   connection. No connection throws :inbox/no-bus (the stream heartbeats and
   retries)."
  [id opts]
  (if-let [conn (client/current-connection)]
    (inbox/fetch! conn id opts)
    (throw (ex-info "no connected event bus" {:error :inbox/no-bus}))))

(m/=> nats-source [:=> [:cat :string :map] [:vector inbox/Fetched]])

(def ^:private default-config
  {:source           #'nats-source
   :batch            16
   :wait-ms          15000
   :retry-ms         2000
   :write-timeout-ms 30000})

(defonce ^:private config* (atom default-config))

(defn configure!
  "Merge `opts` into the stream configuration (:source :batch :wait-ms
   :retry-ms :write-timeout-ms); nil restores the defaults. Returns it."
  [opts]
  (if (nil? opts)
    (reset! config* default-config)
    (swap! config* merge opts)))

(m/=> configure! [:=> [:cat [:maybe :map]] :map])

;; =============================================================================
;; Boundary: the stream
;; =============================================================================

(defn- put-frame!
  "Write `text` to `out`; true only when the connection took it in time."
  [out text timeout-ms]
  (true? (swallow/attempt @(s/try-put! out text timeout-ms ::timeout))))

(defn- nak-all! [fetched]
  (doseq [{:keys [nak!]} fetched] (swallow/attempt (nak!))))

(defn- pump!
  "Feed inbox `id` into `out` until the reader goes away."
  [id out {:keys [source batch wait-ms retry-ms write-timeout-ms]}]
  (try
    (loop []
      (when-not (s/closed? out)
        (let [fetched (try (source id {:n batch :wait-ms wait-ms})
                           (catch Throwable t
                             (log/debug "[inbox-stream]" id "fetch failed:" (.getMessage t))
                             (Thread/sleep (long retry-ms))
                             ::failed))]
          (cond
            (or (= ::failed fetched) (empty? fetched))
            (when (put-frame! out (heartbeat (System/currentTimeMillis)) write-timeout-ms)
              (recur))

            :else
            (let [left (loop [[f & more :as todo] fetched]
                         (cond
                           (nil? f) nil
                           (put-frame! out (frame (:message f)) write-timeout-ms)
                           (do (when (swallow/attempt ((:ack! f)))
                                 (wrote! id))
                               (recur more))
                           :else todo))]
              (if left
                (nak-all! left)
                (recur)))))))
    (catch Throwable t
      (log/warn "[inbox-stream]" id "stream ended:" (.getMessage t)))
    (finally
      (s/close! out)
      (reader-left! id))))

(defn- refuse [status msg]
  {:status status :headers {"Content-Type" "text/plain"} :body msg})

(defn authorize
  "Pure over the registry: may `req` open the stream of the inbox its path
   names? -> {:ok? true :inbox id} | {:ok? false :response ring-response}."
  [req]
  (let [raw (parse-route (:uri req))
        id  (some-> raw inbox/safe-encode-inbox-id)
        tok (get (parse-query (:query-string req)) "token")
        want (get-in @registry* [id :token])]
    (cond
      (not= :get (:request-method req))           {:ok? false :response (refuse 405 "GET only")}
      (nil? id)                                   {:ok? false :response (refuse 404 "no such inbox route")}
      (not (loopback? (:remote-addr req)))        {:ok? false :response (refuse 403 "loopback only")}
      (not (loopback-origin? (get-in req [:headers "origin"])))
      {:ok? false :response (refuse 403 "loopback Origin only")}
      (not (token= tok want))                     {:ok? false :response (refuse 401 "missing or wrong token: use `inbox arm`'s :monitor spec")}
      :else                                       {:ok? true :inbox id})))

(m/=> authorize [:=> [:cat :map] [:map [:ok? :boolean]]])

(defn handler
  "The ring handler for GET /inbox/<id>.sse. Answers a refusal, or a 200
   whose body is a manifold stream fed by a reader thread."
  [req]
  (let [{:keys [ok? inbox response]} (authorize req)]
    (if-not ok?
      response
      (let [out (s/stream)
            cfg @config*]
        (reader-joined! inbox)
        (s/put! out (str "retry: 2000\n: connected inbox=" inbox "\n\n"))
        (doto (Thread. ^Runnable (fn [] (pump! inbox out cfg))
                       (str "hive-inbox-sse-" inbox))
          (.setDaemon true)
          (.start))
        {:status  200
         :headers {"Content-Type"  "text/event-stream; charset=utf-8"
                   "Cache-Control" "no-cache"
                   "Connection"    "keep-alive"
                   "X-Accel-Buffering" "no"}
         :body    out}))))

(m/=> handler [:=> [:cat :map] :map])

(defn route?
  "Does `uri` belong to this endpoint?"
  [uri]
  (and (string? uri) (str/starts-with? uri "/inbox/")))

(m/=> route? [:=> [:cat :any] :boolean])
