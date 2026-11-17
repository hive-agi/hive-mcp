(ns hive-mcp.nats.sense-adapters
  "Adapters for hive-mcp.nats.sense-republish's ports, and the host wiring.

     SixthSenseFeed      ISenseFeed over hive-agent's sixth-sense push
                         (hive-agent.sixth-sense.api/listen!). hive-agent is
                         not a compile dependency: the fns are resolved at
                         call time, and a JVM without it has no feed.
     NatsInboxPublisher  IInboxPublisher over a jnats Connection supplier
                         (default: the hive client's live one) and
                         hive-mcp.nats.inbox/send!.
     bounded-executor    the republisher's :execute: one daemon thread and a
                         bounded queue, so the sense collector never waits
                         on NATS and a stalled bus cannot grow memory.

   `start!` builds the process republisher from these and installs it; it is
   idempotent and safe to call again (boot, every NATS connect)."
  (:require [hive-mcp.nats.client :as client]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.nats.sense-republish :as sr]
            [taoensso.timbre :as log]
            [malli.core :as m])
  (:import [java.util.concurrent ThreadPoolExecutor TimeUnit
            ArrayBlockingQueue ThreadFactory RejectedExecutionException]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Feed: hive-agent sixth-sense
;; =============================================================================

(defn host-sense-port
  "hive-agent's sixth-sense push as {:listen! f :unlisten! f}, or nil when
   hive-agent is not in this JVM."
  []
  (try
    (let [l (requiring-resolve 'hive-agent.sixth-sense.api/listen!)
          u (requiring-resolve 'hive-agent.sixth-sense.api/unlisten!)]
      (when (and l u) {:listen! l :unlisten! u}))
    (catch Throwable _ nil)))

(m/=> host-sense-port [:=> [:cat] [:maybe [:map [:listen! ifn?] [:unlisten! ifn?]]]])

(defrecord SixthSenseFeed [port-fn]
  sr/ISenseFeed
  (feed-id [_] :sixth-sense)
  (subscribe! [_ key f]
    (if-let [{:keys [listen!]} (port-fn)]
      (do (listen! key f) true)
      false))
  (unsubscribe! [_ key]
    (when-let [{:keys [unlisten!]} (port-fn)]
      (unlisten! key))
    nil))

(defn sixth-sense-feed
  "The sixth-sense ISenseFeed. `port-fn` (default `host-sense-port`) answers
   {:listen! :unlisten!} or nil."
  ([] (sixth-sense-feed host-sense-port))
  ([port-fn] (->SixthSenseFeed port-fn)))

(m/=> sixth-sense-feed [:function
                        [:=> [:cat] [:fn #(satisfies? sr/ISenseFeed %)]]
                        [:=> [:cat ifn?] [:fn #(satisfies? sr/ISenseFeed %)]]])

;; =============================================================================
;; Publisher: HIVE_INBOX over jnats
;; =============================================================================

(defrecord NatsInboxPublisher [conn-fn]
  sr/IInboxPublisher
  (publisher-available? [_] (some? (conn-fn)))
  (publish-message! [_ message]
    (if-let [conn (conn-fn)]
      (inbox/send! conn message)
      (throw (ex-info "no connected event bus" {:error :inbox/no-bus})))))

(defn nats-inbox-publisher
  "The HIVE_INBOX IInboxPublisher. `conn-fn` (default: the hive client's
   live connection) answers a jnats Connection or nil."
  ([] (nats-inbox-publisher client/current-connection))
  ([conn-fn] (->NatsInboxPublisher conn-fn)))

(m/=> nats-inbox-publisher [:function
                            [:=> [:cat] [:fn #(satisfies? sr/IInboxPublisher %)]]
                            [:=> [:cat ifn?] [:fn #(satisfies? sr/IInboxPublisher %)]]])

;; =============================================================================
;; Executor
;; =============================================================================

(defn bounded-executor
  "An :execute fn for a republisher: runs thunks in order on ONE daemon
   thread with a queue of `capacity` (default 1024). Answers false when the
   queue is full, so the caller counts a drop instead of waiting."
  ([] (bounded-executor 1024))
  ([capacity]
   (let [tf (reify ThreadFactory
              (newThread [_ r]
                (doto (Thread. ^Runnable r "hive-sense-republish")
                  (.setDaemon true))))
         ex (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS
                                 (ArrayBlockingQueue. (int capacity)) tf)]
     (fn [thunk]
       (try (.execute ex ^Runnable thunk) true
            (catch RejectedExecutionException _ false))))))

(m/=> bounded-executor [:function
                        [:=> [:cat] ifn?]
                        [:=> [:cat pos-int?] ifn?]])

;; =============================================================================
;; Host wiring
;; =============================================================================

(defonce ^:private executor* (delay (bounded-executor)))

(defn start!
  "Install the process republisher (sixth-sense -> HIVE_INBOX) when none is
   installed. Idempotent; call at boot and on every NATS connect: a JVM where
   hive-agent mounts later is picked up then. Never throws."
  []
  (try
    (if (sr/installed?)
      {:installed? true :feed :sixth-sense}
      (let [r   (sr/republisher (sixth-sense-feed) (nats-inbox-publisher)
                                {:execute @executor*})
            res (sr/install! r)]
        (when (:installed? res)
          (log/info "[sense-republish] senses republish into parent inboxes"))
        res))
    (catch Throwable t
      (log/warn "[sense-republish] start failed (non-fatal):" (.getMessage t))
      {:installed? false :feed :sixth-sense})))

(m/=> start! [:=> [:cat] [:map [:installed? :boolean] [:feed :any]]])
