(ns hive-mcp.nats.inbox
  "Per-coordinator session inboxes on the HIVE_INBOX JetStream stream.

   An inbox id is a STABLE name the operator assigns (decision
   20261102194055-2dba76dd, Q8), never the MCP session id. Inbox <id> is the
   subject hive.v1.inbox.<id>, read through ONE durable pull consumer
   inbox-<id> with explicit acks. The cursor lives in the broker, so a reader
   that goes away and comes back (re-arm) gets exactly what it has not yet
   acked, in stream order.

   Strata (CPPB):
     Value objects  InboxId, Envelope, SendReceipt, ReadResult, InboxStatus
     Pure           valid-inbox-id?, subject, durable, envelope, decode
     Boundary       arm!, send!, read!, status (each takes a jnats Connection,
                    so tests drive a throwaway broker and the tool passes the
                    hive client's live one)"
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.nats.streams :as streams]
            [malli.core :as m]
            [hive-mcp.resilience.swallow :as swallow])
  (:import [io.nats.client Connection JetStreamApiException PublishOptions
            FetchConsumeOptions Message]
           [io.nats.client.api ConsumerConfiguration AckPolicy DeliverPolicy
            ConsumerInfo]
           [java.time Duration]
           [java.util UUID]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value objects
;; =============================================================================

(def ^:private id-re #"^[A-Za-z0-9_-]{1,64}$")

(def InboxId [:re id-re])

(def Envelope
  [:map
   [:id :string]
   [:to InboxId]
   [:from {:optional true} [:maybe :string]]
   [:body :any]
   [:meta {:optional true} [:maybe :map]]
   [:ts :int]])

(def SendReceipt
  [:map [:id :string] [:to :string] [:subject :string] [:seq :int] [:duplicate? :boolean]])

(def Delivered
  [:map [:seq :int] [:id {:optional true} [:maybe :string]]
   [:from {:optional true} [:maybe :string]] [:body :any]
   [:meta {:optional true} [:maybe :map]] [:ts {:optional true} [:maybe :int]]])

(def ReadResult
  [:map [:inbox :string] [:messages [:vector Delivered]] [:pending :int]])

(def InboxStatus
  [:map [:inbox :string] [:armed? :boolean]
   [:pending {:optional true} :int] [:ack-pending {:optional true} :int]
   [:delivered-seq {:optional true} :int]])

;; =============================================================================
;; Pure
;; =============================================================================

(def stream-name (:name streams/inbox-stream))

(defn valid-inbox-id?
  "True for an id usable as one NATS subject token and a durable name:
   1-64 chars of letters, digits, '-' and '_'."
  [id]
  (boolean (and (string? id) (re-matches id-re id))))

(m/=> valid-inbox-id? [:=> [:cat :any] :boolean])

(defn subject [id] (str "hive.v1.inbox." id))
(m/=> subject [:=> [:cat InboxId] :string])

(defn durable [id] (str "inbox-" id))
(m/=> durable [:=> [:cat InboxId] :string])

(defn envelope
  "Pure: the message as stored. `id` doubles as Nats-Msg-Id, so a retried
   send inside the stream's duplicate window is stored once."
  [{:keys [id to from body meta ts]}]
  (cond-> {:id (or id (str (UUID/randomUUID)))
           :to to
           :body body
           :ts (or ts (System/currentTimeMillis))}
    from (assoc :from from)
    meta (assoc :meta meta)))

(m/=> envelope [:=> [:cat [:map [:to InboxId]]] Envelope])

(defn decode
  "Pure: stored bytes as a delivery. A payload that is not an envelope is
   delivered as its raw text under :body, never dropped."
  [^bytes data seq]
  (let [s (String. data "UTF-8")
        e (swallow/attempt-ex (json/read-str s :key-fn keyword))]
    (if (and (map? e) (contains? e :body))
      (assoc (select-keys e [:id :from :body :meta :ts]) :seq seq)
      {:seq seq :body s})))

(m/=> decode [:=> [:cat bytes? :int] Delivered])

(defn encode-inbox-id
  "Pure: the canonical inbox id for `id`. The coordinator lane spells its
   session `coordinator:<pid>` or `coordinator:<first 8 hex of the session
   id>`; ':' is no subject-token character, so every ':' becomes '-'
   (`coordinator:134462` -> `coordinator-134462`, `coordinator:72216f39` ->
   `coordinator-72216f39`). A valid id
   is returned unchanged, so encoding is idempotent. `valid-inbox-id?` stays
   the one strict grammar: an id that still fails it after encoding throws
   :inbox/invalid-id naming the given and the encoded form."
  [id]
  (let [encoded (if (string? id) (str/replace id ":" "-") id)]
    (if (valid-inbox-id? encoded)
      encoded
      (throw (ex-info (str "invalid inbox id " (pr-str id)
                           (when (and (string? id) (not= id encoded))
                             (str " (encoded " (pr-str encoded) ")"))
                           ": use 1-64 chars of letters, digits, '-' or '_'"
                           " (a ':' is written '-')")
                      {:error :inbox/invalid-id :inbox id :encoded encoded})))))

(m/=> encode-inbox-id [:=> [:cat :any] :string])

(defn decode-inbox-id
  "Pure: the coordinator-lane spelling of a canonical id. Reverses
   `encode-inbox-id` for `coordinator-<session>` where the session token is
   one bb-mcp stamps: decimal digits (a pid, `coordinator-134462` ->
   `coordinator:134462`) or exactly eight lowercase hex digits (a Claude Code
   session-id prefix, `coordinator-a2f8cfa6` -> `coordinator:a2f8cfa6`,
   kanban 20261111164432-2be7b1e6). Every other id is returned as given,
   because a '-' elsewhere is a plain dash."
  [id]
  (let [prefix "coordinator-"
        token  (when (and (string? id) (str/starts-with? id prefix))
                 (subs id (count prefix)))]
    (if (and (seq token)
             (or (every? #(Character/isDigit ^char %) token)
                 (re-matches #"[0-9a-f]{8}" token)))
      (str "coordinator:" token)
      id)))

(m/=> decode-inbox-id [:=> [:cat :any] :any])

(defn safe-encode-inbox-id
  "`encode-inbox-id`, or nil when `id` cannot be made a valid inbox id."
  [id]
  (swallow/attempt-ex (encode-inbox-id id)))

(m/=> safe-encode-inbox-id [:=> [:cat :any] [:maybe :string]])

;; =============================================================================
;; Boundary
;; =============================================================================

(def ^:private consumer-not-found 10014)
(def ^:private stream-not-found 10059)

(defn- not-found? [^JetStreamApiException e]
  (contains? #{consumer-not-found stream-not-found} (.getApiErrorCode e)))

(defn- consumer-info ^ConsumerInfo [^Connection conn id]
  (try (.getConsumerInfo (.jetStreamManagement conn) stream-name (durable id))
       (catch JetStreamApiException e (if (not-found? e) nil (throw e)))))

(defn- ensure-stream! [^Connection conn]
  (let [jsm (.jetStreamManagement conn)]
    (when-not (try (.getStreamInfo jsm stream-name) true
                   (catch JetStreamApiException e
                     (if (not-found? e) false (throw e))))
      (let [r (streams/ensure-stream! conn streams/inbox-stream)]
        (when (= :failed (:action r))
          (throw (ex-info (str "HIVE_INBOX could not be ensured: " (:error r))
                          {:error :inbox/stream-failed :report r})))))))

(defn- info->status [id ^ConsumerInfo ci]
  (if ci
    {:inbox id :armed? true
     :pending (int (.getNumPending ci))
     :ack-pending (int (.getNumAckPending ci))
     :delivered-seq (int (.getStreamSequence (.getDelivered ci)))}
    {:inbox id :armed? false}))

(defn arm!
  "Ensure HIVE_INBOX and the durable for inbox `id`. Idempotent: re-arming
   keeps the broker's cursor, so nothing acked is replayed and nothing unacked
   is lost. A first arm delivers what was sent to the inbox before it existed
   (still inside the stream's 7 day max-age)."
  [^Connection conn id]
  (let [id       (encode-inbox-id id)
        _        (ensure-stream! conn)
        existed? (some? (consumer-info conn id))
        cfg (-> (ConsumerConfiguration/builder)
                (.durable (durable id))
                (.filterSubject (subject id))
                (.ackPolicy AckPolicy/Explicit)
                (.deliverPolicy DeliverPolicy/All)
                (.ackWait (Duration/ofSeconds 30))
                (.build))]
    (when-not existed?
      (.addOrUpdateConsumer (.jetStreamManagement conn) stream-name cfg))
    (assoc (info->status id (consumer-info conn id))
           :created? (not existed?)
           :subject (subject id)
           :durable (durable id))))

(m/=> arm! [:=> [:cat :any :string] [:map [:inbox :string] [:armed? :boolean]]])

(defn disarm!
  "Delete the durable of inbox `id`, if any. Messages already in the stream
   stay until max-age, but nothing reads them: a later arm! would replay
   them, so an id is disarmed only when its reader is gone for good (a killed
   ling). Answers {:inbox id :armed? false :deleted? bool}."
  [^Connection conn id]
  (let [id (encode-inbox-id id)
        deleted? (boolean
                  (when (consumer-info conn id)
                    (try (.deleteConsumer (.jetStreamManagement conn) stream-name (durable id))
                         (catch JetStreamApiException e
                           (if (not-found? e) false (throw e))))))]
    {:inbox id :armed? false :deleted? deleted?}))

(m/=> disarm! [:=> [:cat :any :string] [:map [:inbox :string] [:armed? :boolean]
                                        [:deleted? :boolean]]])

(defn send!
  "Publish one message to inbox `to` with a broker ack. Returns the stream
   seq. The inbox need not be armed yet: the stream keeps the message."
  [^Connection conn {:keys [to] :as msg}]
  (let [to  (encode-inbox-id to)
        msg (assoc msg :to to)
        _   (ensure-stream! conn)
        env (envelope msg)
        pa  (.publish (.jetStream conn) ^String (subject to)
                      (.getBytes ^String (json/write-str env) "UTF-8")
                      (-> (PublishOptions/builder) (.messageId (:id env)) (.build)))]
    {:id (:id env) :to to :subject (subject to)
     :seq (.getSeqno pa) :duplicate? (.isDuplicate pa)}))

(m/=> send! [:=> [:cat :any [:map [:to :string]]] SendReceipt])

(defn read!
  "Pull up to `n` (default 10, max 256) messages from armed inbox `id` and
   ack each one as it is returned. `wait-ms` (default 0) waits that long for a
   first message when none is pending. Throws :inbox/not-armed."
  ([conn id] (read! conn id nil))
  ([^Connection conn raw-id {:keys [n wait-ms]}]
   (let [id   (encode-inbox-id raw-id)
         _    (when-not (consumer-info conn id)
                (throw (ex-info (str "inbox " id " is not armed: run `inbox arm` first")
                                {:error :inbox/not-armed :inbox id})))
         n    (-> (or n 10) long (max 1) (min 256))
         wait (-> (or wait-ms 0) long (max 0) (min 30000))
         b    (-> (FetchConsumeOptions/builder) (.maxMessages (int n)))
         opts (.build (if (pos? wait) (.noWaitExpiresIn b wait) (.noWait b)))
         cc   (.getConsumerContext conn stream-name (durable id))
         fr   (.fetch cc opts)
         msgs (loop [acc []]
                (if-let [^Message msg (.nextMessage fr)]
                  (let [d (decode (.getData msg) (.streamSequence (.metaData msg)))]
                    (.ackSync msg (Duration/ofSeconds 5))
                    (recur (conj acc d)))
                  acc))]
     {:inbox id :messages msgs
      :pending (or (:pending (info->status id (consumer-info conn id))) 0)})))

(def Fetched
  "One message pulled but NOT acked: the caller acks after it has handed the
   message on (hive-mcp.nats.inbox-stream acks after the SSE write)."
  [:map [:message Delivered] [:ack! ifn?] [:nak! ifn?]])

(defn fetch!
  "Pull up to `n` (default 10, max 256) messages from armed inbox `id`
   WITHOUT acking them. Each answer is a `Fetched`: call :ack! once the
   message is delivered, :nak! to have the broker redeliver it now. One left
   alone is redelivered after the durable's 30 s ack wait. `wait-ms` (default
   0) waits that long for a first message. Throws :inbox/not-armed."
  ([conn id] (fetch! conn id nil))
  ([^Connection conn raw-id {:keys [n wait-ms]}]
   (let [id   (encode-inbox-id raw-id)
         _    (when-not (consumer-info conn id)
                (throw (ex-info (str "inbox " id " is not armed: run `inbox arm` first")
                                {:error :inbox/not-armed :inbox id})))
         n    (-> (or n 10) long (max 1) (min 256))
         wait (-> (or wait-ms 0) long (max 0) (min 30000))
         b    (-> (FetchConsumeOptions/builder) (.maxMessages (int n)))
         opts (.build (if (pos? wait) (.noWaitExpiresIn b wait) (.noWait b)))
         cc   (.getConsumerContext conn stream-name (durable id))
         fr   (.fetch cc opts)]
     (loop [acc []]
       (if-let [^Message msg (.nextMessage fr)]
         (recur (conj acc {:message (decode (.getData msg) (.streamSequence (.metaData msg)))
                           :ack!    (fn [] (.ackSync msg (Duration/ofSeconds 5)) true)
                           :nak!    (fn [] (.nak msg) true)}))
         acc)))))

(m/=> fetch! [:function
              [:=> [:cat :any :string] [:vector Fetched]]
              [:=> [:cat :any :string [:maybe :map]] [:vector Fetched]]])

(m/=> read! [:function
             [:=> [:cat :any :string] ReadResult]
             [:=> [:cat :any :string [:maybe :map]] ReadResult]])

(defn status
  "Status of inbox `id`, or of every armed inbox when `id` is nil."
  [^Connection conn id]
  (if id
    (let [id (encode-inbox-id id)]
      (info->status id (consumer-info conn id)))
    (let [jsm (.jetStreamManagement conn)
          names (try (vec (.getConsumerNames jsm stream-name))
                     (catch JetStreamApiException e (if (not-found? e) [] (throw e))))]
      {:inboxes (->> names
                     (keep #(when (str/starts-with? % "inbox-") (subs % 6)))
                     sort
                     (mapv #(info->status % (consumer-info conn %))))})))

(m/=> status [:=> [:cat :any [:maybe :string]] :map])
