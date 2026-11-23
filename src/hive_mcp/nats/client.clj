(ns hive-mcp.nats.client
  "NATS client wrapper for push-based agent notifications, enabling real-time
   communication in the Hive system. Manages connection lifecycle, publish,
   subscribe, and health checks.

   Implements a graceful degradation model: when NATS is unavailable, the system
   automatically falls back to polling-based collect mechanisms, ensuring
   reliability without fatal errors.

   Depends on the Java NATS client (io.nats.client) for low-level NATS protocol
   handling. State is managed via atoms: connection (io.nats.client.Connection),
   dispatcher (async message handling), and subscriptions (subject-to-Subscription
   tracking for cleanup)."

  (:require [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [malli.core :as m]
            [clojure.string :as str]
            [hive-mcp.resilience.swallow :as swallow]
            [hive-ttracking.tracecontext :as tc]
            [hive-mcp.context.request :as ctx])
  (:import [io.nats.client Nats Options$Builder Connection$Status
            MessageHandler ErrorListener JetStreamSubscription Message Connection
            Dispatcher]
           [io.nats.client.impl ErrorListenerLoggerImpl Headers]
           [java.time Duration]
           [io.nats.client ConnectionListener]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; State
;; =============================================================================

;; Atom holding the io.nats.client.Connection instance, or nil if disconnected.
(defonce ^:private connection
  (atom nil))

;; Atom holding the Dispatcher for asynchronous message handling, or nil if disconnected.
(defonce ^:private dispatcher
  (atom nil))

;; Atom mapping subject strings to Subscription objects for cleanup during disconnect.
(defonce ^:private subscriptions
  (atom {}))  ;; subject -> Subscription

;; =============================================================================
;; Connection Lifecycle
;; =============================================================================

(defn connected?
  "Return true if NATS connection is active and connected."
  []
  (when-let [^Connection c @connection]
    (= (.getStatus c) Connection$Status/CONNECTED)))

(defn current-connection
  "The live jnats Connection when CONNECTED, else nil. For callers that need
   the JetStream API (hive-mcp.nats.inbox); everything else goes through
   publish!/subscribe!."
  []
  (when (connected?) @connection))

(m/=> current-connection [:=> [:cat] :any])

(defonce ^:private conn-state
  (atom {:status :stopped}))

(defonce ^:private epoch-counter (atom 0))

(defonce ^:private on-connect-hooks (atom {}))

(defn redact-url
  "Pure: a NATS URL with any user:pass@ credentials removed."
  [url]
  (some-> url str (clojure.string/replace #"//[^/@]*@" "//")))

(m/=> redact-url [:=> [:cat [:maybe :string]] [:maybe :string]])

(defn add-on-connect-hook!
  "Run `(f jnats-connection)` on every CONNECTED/RECONNECTED event, boot
   included, and at once when the client is already connected, so the order
   of registration against connect does not matter. Idempotent per key. A
   throwing hook is logged and isolated."
  [key f]
  (swap! on-connect-hooks assoc key f)
  (when-let [conn (and (connected?) @connection)]
    (try (f conn)
         (catch Throwable t
           (log/warn "[NATS] on-connect hook" key "failed:" (.getMessage t)))))
  key)

(m/=> add-on-connect-hook! [:=> [:cat :any ifn?] :any])

(defn remove-on-connect-hook!
  "Remove the on-connect hook under `key`. Safe when absent."
  [key]
  (swap! on-connect-hooks dissoc key)
  nil)

(m/=> remove-on-connect-hook! [:=> [:cat :any] :nil])

(defn- run-on-connect-hooks! [conn]
  (doseq [[k f] @on-connect-hooks]
    (try (f conn)
         (catch Throwable t
           (log/warn "[NATS] on-connect hook" k "failed:" (.getMessage t))))))

(defn- adopt-connection!
  "First CONNECTED: publish the Connection and a dispatcher. Reconnects
   reuse both (jnats heals the same Connection and its subscriptions)."
  [^Connection conn]
  (when-not (identical? conn @connection)
    (reset! dispatcher (.createDispatcher conn))
    (reset! connection conn)))

(defn- connection-listener
  "Listener bound to one start! epoch. Once stop!/start! moves the epoch on,
   any event from the old attempt closes that attempt's Connection: an async
   connect that never succeeded is not reachable through `connection`, so
   this is the only place it can be stopped."
  [epoch]
  (reify ConnectionListener
    (connectionEvent [_ conn ev]
      (if (not= epoch @epoch-counter)
        (when (not= "CLOSED" (.name ev))
          (future (swallow/attempt-ex (.close conn))))
        (case (.name ev)
          ("CONNECTED" "RECONNECTED")
          (do (adopt-connection! conn)
              (swap! conn-state assoc :status :connected :error nil
                     :since (System/currentTimeMillis))
              (swap! conn-state update :connects (fnil inc 0))
              (log/info "[NATS]" (.getEvent ev) (:url @conn-state))
              (run-on-connect-hooks! conn))
          "DISCONNECTED"
          (swap! conn-state assoc :status :reconnecting
                 :since (System/currentTimeMillis))
          "CLOSED"
          (swap! conn-state assoc :status :closed)
          nil)))))

(defn- stop-connection!
  "Close the current connection (if any) and invalidate its listener."
  []
  (swap! epoch-counter inc)
  (when-let [^Connection conn @connection]
    (swallow/attempt-ex (.close conn)))
  (reset! connection nil)
  (reset! dispatcher nil)
  (reset! subscriptions {}))

(def ClientStatus
  [:map
   [:status [:enum :stopped :connecting :connected :reconnecting :closed :failed]]
   [:connected? :boolean]
   [:url {:optional true} [:maybe :string]]
   [:since {:optional true} [:maybe :int]]
   [:connects {:optional true} :int]
   [:error {:optional true} [:maybe :string]]])

(defn status
  "Client connection state, measured from the live Connection when present.
   The URL is reported without credentials."
  []
  (let [s @conn-state
        up? (boolean (connected?))]
    (cond-> (assoc (select-keys s [:status :url :since :connects :error])
                   :connected? up?)
      (and up? (not= :connected (:status s))) (assoc :status :connected))))

(m/=> status [:=> [:cat] ClientStatus])

(def heartbeat-alarm-window-ms
  "At most one heartbeatAlarm line per consumer in this window."
  60000)

(defonce ^:private heartbeat-alarms
  ;; consumer name -> {:last-logged ms :suppressed n}
  (atom {}))

(defn heartbeat-alarm-log!
  "Rate-limit decision for one heartbeatAlarm of CONSUMER at NOW: answers
   {:log? true :suppressed n} (log it, naming the N alarms dropped since the
   last line) at most once per WINDOW-MS per consumer, else {:log? false}
   and the alarm is counted."
  ([consumer now] (heartbeat-alarm-log! consumer now heartbeat-alarm-window-ms))
  ([consumer now window-ms]
   (let [k (str consumer)
         [before _] (swap-vals! heartbeat-alarms update k
                                (fn [{:keys [last-logged suppressed] :or {suppressed 0} :as s}]
                                  (if (or (nil? last-logged) (>= (- now last-logged) window-ms))
                                    {:last-logged now :suppressed 0}
                                    (assoc s :suppressed (inc suppressed)))))
         {:keys [last-logged suppressed] :or {suppressed 0}} (get before k)]
     (if (or (nil? last-logged) (>= (- now last-logged) window-ms))
       {:log? true :suppressed suppressed}
       {:log? false}))))

(m/=> heartbeat-alarm-log! [:function
                            [:=> [:cat :any :int] [:map [:log? :boolean]]]
                            [:=> [:cat :any :int :int] [:map [:log? :boolean]]]])

(defn error-listener
  "The connection's jnats ErrorListener: jnats' own logger for everything,
   except heartbeatAlarm, which is logged at most once per consumer per
   `heartbeat-alarm-window-ms` as a timbre warning (with the count it
   dropped). A pull consumer on a loaded host (one per ling inbox) can miss
   its idle heartbeat on every fetch, and the default listener printed one
   SEVERE line per ling per fetch."
  ^ErrorListener []
  (let [base (ErrorListenerLoggerImpl.)]
    (reify ErrorListener
      (errorOccurred [_ conn error] (.errorOccurred base conn error))
      (exceptionOccurred [_ conn exp] (.exceptionOccurred base conn exp))
      (slowConsumerDetected [_ conn consumer] (.slowConsumerDetected base conn consumer))
      (messageDiscarded [_ conn msg] (.messageDiscarded base conn msg))
      (unhandledStatus [_ conn sub status] (.unhandledStatus base conn sub status))
      (pullStatusWarning [_ conn sub status] (.pullStatusWarning base conn sub status))
      (pullStatusError [_ conn sub status] (.pullStatusError base conn sub status))
      (socketWriteTimeout [_ conn] (.socketWriteTimeout base conn))
      (heartbeatAlarm [_ _conn sub last-stream-seq last-consumer-seq]
        (let [consumer (swallow/attempt (some-> ^JetStreamSubscription sub (.getConsumerName)))
              {:keys [log? suppressed]} (heartbeat-alarm-log! consumer
                                                              (System/currentTimeMillis))]
          (when log?
            (log/warn "[NATS] heartbeatAlarm" {:consumer consumer
                                               :last-stream-seq last-stream-seq
                                               :last-consumer-seq last-consumer-seq
                                               :suppressed-since-last suppressed})))))))

(m/=> error-listener [:=> [:cat] [:fn #(instance? ErrorListener %)]])

(defn start!
  "Connect to NATS without blocking boot, and keep trying forever.

   Options:
     :url                - NATS server URL (default: nats://localhost:4222)
     :connection-timeout - per-attempt timeout in ms (default: 5000)
     :max-reconnects     - reconnect attempts (default: -1, forever)
     :reconnect-wait     - wait between attempts in ms (default: 1000)

   Returns at once. A broker that is down now is joined when it comes up;
   the connection, dispatcher and every on-connect hook arrive through the
   jnats ConnectionListener, the same path a later reconnect takes."
  [{:keys [url connection-timeout max-reconnects reconnect-wait]
    :or {url "nats://localhost:4222"
         connection-timeout 5000
         max-reconnects -1
         reconnect-wait 1000}}]
  (stop-connection!)
  (let [epoch (swap! epoch-counter inc)]
    (swap! conn-state assoc :url (redact-url url) :status :connecting
           :since (System/currentTimeMillis))
    (try
      (let [opts (-> (Options$Builder.)
                     (.server url)
                     (.connectionTimeout (Duration/ofMillis connection-timeout))
                     (.maxReconnects max-reconnects)
                     (.reconnectWait (Duration/ofMillis reconnect-wait))
                     (.connectionName "hive-mcp")
                     (.connectionListener (connection-listener epoch))
                     (.errorListener (error-listener))
                     (.build))]
        (Nats/connectAsynchronously opts true)
        (log/info "[NATS] Connecting (async, retry forever) to" (redact-url url)))
      (catch Exception e
        (swap! conn-state assoc :status :failed :error (.getMessage e))
        (log/warn "[NATS] Connection setup failed (non-fatal):" (.getMessage e))))
    nil))

(defn stop!
  "Disconnect from NATS server. Safe to call when not connected."
  []
  (let [had? (some? @connection)]
    (stop-connection!)
    (swap! conn-state assoc :status :stopped :since (System/currentTimeMillis))
    (when had? (log/info "[NATS] Disconnected"))
    nil))

;; =============================================================================
;; Connection Watches
;; =============================================================================

(defn add-connection-watch!
  "Register `f` under `key`; it is called as (f old-conn new-conn) whenever the
   connection OBJECT is replaced, that is, on start! and on stop!.

   A jnats reconnect does NOT fire here: `max-reconnects -1` means jnats heals
   the same Connection in place and re-establishes its own subscriptions, so
   nothing needs re-arming. What does fire is a stop!/start! cycle, and that one
   is destructive: stop! drops the dispatcher and clears `subscriptions`, so
   every subscription armed before the cycle is silently gone afterwards while
   `connected?` reports true again. A subscriber that owns a long-lived
   subscription (the progress NATS->ws bridge) watches this to re-subscribe.

   Idempotent per key: re-registering a key replaces its callback."
  [key f]
  (add-watch connection key
             (fn [_ _ old new]
               (when (not= old new)
                 (try (f old new)
                      (catch Exception e
                        (log/warn "[NATS] Connection watch" key "failed:"
                                  (.getMessage e)))))))
  key)

(defn remove-connection-watch!
  "Remove the connection watch registered under `key`. Safe when absent."
  [key]
  (remove-watch connection key)
  nil)

;; =============================================================================
;; Payload Sanitization
;; =============================================================================

(def ^:const max-payload-depth
  "Maximum nesting depth allowed in a published payload before clamping.
   clojure.data.json walks payloads recursively; an unbounded deep nest
   (e.g. a shout whose :data carries a quoted ratom graph or a cyclic
   structure) exhausts the JVM stack as a StackOverflowError that takes
   down the publisher thread. 32 is well below the safe Clojure recursion
   ceiling and deeper than any legitimate shout payload."
  32)

(defn clamp-depth
  "Walk DATA replacing collections nested deeper than `max-payload-depth`
   with a marker string. Cycle-safe insofar as a depth bound guarantees
   termination regardless of input shape (self-referential maps will
   bottom out at the cap rather than recurse forever)."
  ([data] (clamp-depth data max-payload-depth))
  ([data depth]
   (cond
     (or (nil? data) (not (coll? data))) data
     (zero? depth)
     (cond
       (map? data)    "<truncated:map>"
       (vector? data) "<truncated:vec>"
       (set? data)    "<truncated:set>"
       :else          "<truncated:seq>")
     (map? data)
     (persistent!
      (reduce-kv (fn [m k v] (assoc! m k (clamp-depth v (dec depth))))
                 (transient {}) data))
     (vector? data)
     (mapv #(clamp-depth % (dec depth)) data)
     (set? data)
     (into #{} (map #(clamp-depth % (dec depth))) data)
     :else
     (doall (map #(clamp-depth % (dec depth)) data)))))

;; =============================================================================
;; Trace context (STD-04 O1: W3C traceparent / tracestate as NATS headers)
;; =============================================================================

(defonce ^:private trace-random
  ;; One SecureRandom for every fresh span id this namespace draws.
  (delay (tc/secure-random-fn)))

(defn bound-trace
  "The W3C trace context bound in the request context of the calling thread
   (:trace of hive-mcp.context.request/*request-ctx*), or nil."
  []
  (:trace (ctx/request-ctx)))

(defn outbound-trace
  "Context an outbound message carries: a child of CTX (default the bound
   one) with the same trace-id, flags and tracestate and a fresh parent-id.
   nil when CTX is nil or invalid, so nothing is invented outside a trace."
  ([] (outbound-trace (bound-trace)))
  ([trace-ctx]
   (when trace-ctx
     (tc/child trace-ctx (tc/new-span-id @trace-random)))))

(defn trace-headers
  "jnats Headers holding `traceparent` (and `tracestate` when non-empty) for
   a valid TRACE-CTX; nil for nil or an invalid context."
  ^Headers [trace-ctx]
  (when-let [hs (tc/->headers trace-ctx)]
    (let [h (Headers.)]
      (doseq [[k v] hs]
        (.add h ^String k ^"[Ljava.lang.String;" (into-array String [v])))
      h)))

(defn inbound-trace
  "Trace context carried by the headers of an inbound jnats MSG, or nil when
   it has no headers or a missing or malformed traceparent (an invalid
   tracestate is dropped, it never invalidates the traceparent)."
  [^Message msg]
  (when (.hasHeaders msg)
    (let [^Headers h (.getHeaders msg)]
      (tc/parse (.getFirst h "traceparent") (.getFirst h "tracestate")))))

(defn- safe-outbound-headers
  "Headers for an outbound message of the bound trace (a child of it), or
   nil when no trace is bound or building them fails in any way. Its own
   guarded step: a trace failure never reaches the publish."
  ^Headers []
  (try (trace-headers (outbound-trace))
       (catch Throwable e
         (log/debug "[NATS] trace header build failed, publishing plain:" (.getMessage e))
         nil)))

(defn- publish-traced!
  "Publish BYTES on SUBJECT through CONN, with headers H when non-nil. When
   the headers publish throws (a build problem, or jnats refusing headers,
   e.g. \"Headers are not supported by the server\"), the message is sent
   again WITHOUT headers: a trace never costs a message. A failure of the
   plain publish itself propagates to the caller as before."
  [^Connection conn ^String subject ^bytes bytes ^Headers h]
  (if h
    (try (.publish conn subject h bytes)
         (catch Exception e
           (log/debug "[NATS] headers refused on" subject ", publishing plain:" (.getMessage e))
           (.publish conn subject bytes)))
    (.publish conn subject bytes)))

(defn- call-with-trace
  "Run (F) with TRACE-CTX bound as :trace in the request context; plain (F)
   when TRACE-CTX is nil. Inbound carrier rules: a valid `traceparent` header
   (plus `tracestate`, dropped when invalid) binds; anything else binds
   nothing. Reading the headers is guarded: a failure there runs (F) plain."
  [trace-ctx f]
  (if trace-ctx
    (let [bound (assoc (ctx/request-ctx) :trace trace-ctx)]
      (ctx/with-request-context bound (f)))
    (f)))

(defn- safe-inbound-trace
  "`inbound-trace` of MSG, or nil when reading it fails."
  [msg]
  (try (inbound-trace msg) (catch Throwable _ nil)))

;; =============================================================================
;; Publish / Subscribe
;; =============================================================================

(defn publish!
  "Publish JSON message to subject. Clamps nesting depth at
   `max-payload-depth` to prevent StackOverflow from deep/cyclic payloads,
   then serializes to JSON with UTF-8 encoding. No-op if disconnected.
   Serialization failures and StackOverflow are logged and swallowed —
   one bad shout must not crash the publisher."
  [subject data]
  (when-let [^Connection conn @connection]
    (when (connected?)
      (try
        (let [safe (clamp-depth data)
              bytes (.getBytes (json/write-str safe) "UTF-8")]
          (publish-traced! conn subject bytes (safe-outbound-headers)))
        (catch StackOverflowError _e
          (log/warn "[NATS] StackOverflow serializing payload for" subject
                    "— dropped (depth cap insufficient or cycle in scalar)"))
        (catch Exception e
          (log/warn "[NATS] Publish failed on" subject ":" (.getMessage e)))))))

(defn subscribe!
  "Subscribe to subject with handler fn. Handler receives parsed JSON map.
   Implements MessageHandler via reify, deserializing incoming message data
   from JSON using clojure.data.json with keyword keys. Logs warnings on
   handler exceptions. Tracks subscription in the subscriptions atom for cleanup."
  [subject handler-fn]
  (when-let [^Dispatcher disp @dispatcher]
    (let [sub (.subscribe disp ^String subject ^MessageHandler
                          (reify MessageHandler
                            (onMessage [_ msg]
                              (try
                                (let [data (json/read-str (String. (.getData msg) "UTF-8") :key-fn keyword)]
                                  (call-with-trace (safe-inbound-trace msg)
                                                   #(handler-fn data)))
                                (catch Exception e
                                  (log/warn "[NATS] Handler error on" subject ":" (.getMessage e)))))))]
      (swap! subscriptions assoc subject sub)
      sub)))

(defn unsubscribe!
  "Unsubscribe from a subject. Safe to call when not subscribed."
  [subject]
  (when-let [_sub (get @subscriptions subject)]
    (when-let [^Dispatcher disp @dispatcher]
      (.unsubscribe disp ^String subject))
    (swap! subscriptions dissoc subject)))
;; =============================================================================
;; Function Contracts (m16 coverage batch)
;; =============================================================================

;; Connection lifecycle boundary fns; graceful-degradation model means most
;; returns are nil or the jnats object (opaque Java shapes — :any).

(m/=> connected?
      [:=> [:cat]
       :any])

(m/=> start!
      [:=> [:cat :map]
       :any])

(m/=> stop!
      [:=> [:cat]
       :any])

(m/=> add-connection-watch!
      [:=> [:cat :any ifn?]
       :any])

(m/=> remove-connection-watch!
      [:=> [:cat :any]
       :nil])

(m/=> clamp-depth
      [:=> [:cat :any [:* :any]] :any])

(m/=> publish!
      [:=> [:cat :string :any]
       :any])

(m/=> subscribe!
      [:=> [:cat :string ifn?]
       :any])

(m/=> unsubscribe!
      [:=> [:cat :string]
       :any])
