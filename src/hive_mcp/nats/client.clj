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
            [clojure.string :as str])
  (:import [io.nats.client Nats Options$Builder Connection$Status
            MessageHandler]
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
  (some-> @connection (.getStatus) (= Connection$Status/CONNECTED)))

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
  [conn]
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
          (future (try (.close conn) (catch Exception _))))
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
  (when-let [conn @connection]
    (try (.close conn) (catch Exception _)))
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
;; Publish / Subscribe
;; =============================================================================

(defn publish!
  "Publish JSON message to subject. Clamps nesting depth at
   `max-payload-depth` to prevent StackOverflow from deep/cyclic payloads,
   then serializes to JSON with UTF-8 encoding. No-op if disconnected.
   Serialization failures and StackOverflow are logged and swallowed —
   one bad shout must not crash the publisher."
  [subject data]
  (when-let [conn @connection]
    (when (connected?)
      (try
        (let [safe (clamp-depth data)
              bytes (.getBytes (json/write-str safe) "UTF-8")]
          (.publish conn subject bytes))
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
  (when-let [disp @dispatcher]
    (let [sub (.subscribe disp subject
                          (reify MessageHandler
                            (onMessage [_ msg]
                              (try
                                (let [data (json/read-str (String. (.getData msg) "UTF-8") :key-fn keyword)]
                                  (handler-fn data))
                                (catch Exception e
                                  (log/warn "[NATS] Handler error on" subject ":" (.getMessage e)))))))]
      (swap! subscriptions assoc subject sub)
      sub)))

(defn unsubscribe!
  "Unsubscribe from a subject. Safe to call when not subscribed."
  [subject]
  (when-let [_sub (get @subscriptions subject)]
    (when-let [disp @dispatcher]
      (.unsubscribe disp subject))
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
