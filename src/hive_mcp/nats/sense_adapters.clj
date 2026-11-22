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
            [malli.core :as m]
            [hive-mcp.extensions.soft :as ext-soft]
            [hive-mcp.resilience.swallow :as swallow])
  (:import [java.util.concurrent ThreadPoolExecutor TimeUnit
            ArrayBlockingQueue ThreadFactory RejectedExecutionException]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Feed: hive-agent sixth-sense
;; =============================================================================

(def drain-consumer
  "The sense-log consumer id the replay drain reads as.

   A STRING, not a keyword, on purpose: hive-agent persists consumer cursors
   keyed by a string (hive-agent.sixth-sense.store/cursors-value? requires
   one), so a keyword id would be dropped by `restore` and the cursor would
   not survive a re-init. A cursor that survives is the whole point of the
   replay lane, so the id is part of the contract, not a detail."
  "inbox-republish")

(def SensePort
  "The host sense port: where sixth-sense pushes from, and where it drains.
     :listen!   (fn [key f])   register a push wake, `f` called per sense
     :unlisten! (fn [key])     remove that wake
     :drain!    (fn [{:keys [consumer cursor limit receptor]}] -> batch)
               consume-once read of the sense log, one cursor per consumer
               id; the batch is {:senses [Sense] :cursor n :epoch id ...}

   `:drain!` is optional. A hive-agent predating `sense-port` has none, and
   then nothing can be replayed: a sense that arrived while no listener was
   attached is simply not heard (see `drain-feed`)."
  [:map
   [:listen! ifn?]
   [:unlisten! ifn?]
   [:drain! {:optional true} [:maybe ifn?]]])

(defn sense-port?
  "Does `port` answer the host sense port contract? Shape only: nothing is
   invoked, so asking cannot consume a sense from the live log. Behaviour is
   the conformance suite's job (hive-mcp.nats.sense-replay-conformance-test)."
  [port]
  (boolean (and (map? port) (m/validate SensePort port))))

(m/=> sense-port? [:=> [:cat :any] :boolean])

(defn resolve-drain
  "hive-agent's consume-once sense drain, or nil when this JVM has no
   hive-agent (or one too old to expose `sense-port`).

   Resolved rather than required, like every other hive-agent touchpoint: the
   host is loaded into this image, not compiled against. The 0-arity
   `sense-port` re-resolves the process sensorium on each call, so the fn
   built here survives a sensorium remount — resolve it once, call it for the
   life of the process."
  []
  (when-let [sp (ext-soft/resolve-soft 'hive-agent.sixth-sense.api/sense-port)]
    (swallow/attempt (:drain! (sp)))))

(m/=> resolve-drain [:=> [:cat] [:maybe ifn?]])

(defn host-sense-port
  "hive-agent's sixth-sense as the host sense port (see `SensePort`), or nil
   when hive-agent is not in this JVM.

   `:drain!` is the replay lane's first slice. Push alone cannot deliver a
   sense that arrived while no listener was attached (the republisher was not
   installed yet, or the parent was unbound), nor one the client skipped; the
   sense log can, because it outlives both. Push is then only a WAKE and the
   drain is the read path. A hive-agent without `sense-port` yields a port
   with no `:drain!`, which is a working push-only port, never an error."
  []
  (let [l (ext-soft/resolve-soft 'hive-agent.sixth-sense.api/listen!)
        u (ext-soft/resolve-soft 'hive-agent.sixth-sense.api/unlisten!)]
    (when (and l u)
      (if-let [d (resolve-drain)]
        {:listen! l :unlisten! u :drain! d}
        {:listen! l :unlisten! u}))))

(m/=> host-sense-port [:=> [:cat] [:maybe SensePort]])

;; =============================================================================
;; The replay read path
;; =============================================================================

(def DrainResult
  "What one replay drain answered.
     :ok?      did the port answer at all (false: no drain, or it threw)
     :senses   the senses this consumer had not been handed, oldest first
     :cursor   where the next drain resumes (advanced past :senses)
     :epoch    the log the cursor belongs to; a DIFFERENT epoch next time
               means the log was reset and the cursor is meaningless
     :more?    a limit left senses for the next drain
     :hidden   how many senses in the span the receptor filtered out
     :orphans  how many of those name no parent (no reader will hear them)
     :consumer the sense-log consumer id this drain was charged to
     :reason   why it did not answer: :no-drain | :drain-failed"
  [:map
   [:ok? :boolean]
   [:consumer :string]
   [:senses [:vector :map]]
   [:cursor {:optional true} [:maybe :int]]
   [:epoch {:optional true} [:maybe [:or :string :keyword]]]
   [:more? {:optional true} :boolean]
   [:hidden {:optional true} :int]
   [:orphans {:optional true} :int]
   [:reason {:optional true} :keyword]
   [:error {:optional true} [:maybe :string]]])

(defn drain-senses
  "One consume-once replay read of the host sense log, through `port`.

   This is the read path the replay loop calls when a sense wakes it (the
   wake itself comes from :listen!); it is the whole reason `host-sense-port`
   resolves `:drain!`. Push alone loses a sense that arrived while nothing was
   listening; the log does not, because the consumer cursor advanced only by a
   drain is what says the sense was READ.

   Never throws: a port with no drain answers {:ok? false :reason :no-drain},
   and a drain that throws answers :reason :drain-failed with :error. Both
   carry the consumer id, because the caller's one decision is whether to
   retry, and a failed drain must NOT be treated as an empty one."
  ([port] (drain-senses port nil))
  ([port {:keys [cursor limit receptor consumer]}]
   (let [consumer (or consumer drain-consumer)]
     (if-let [drain! (:drain! port)]
       (try
         (let [b    (drain! (cond-> {:consumer consumer}
                              (some? cursor) (assoc :cursor cursor)
                              (some? limit) (assoc :limit limit)
                              (some? receptor) (assoc :receptor receptor)))
               ok!  (some? b)]
           (if ok!
             (cond-> {:ok? true :consumer consumer
                      :senses (vec (:senses b))}
               (some? (:cursor b)) (assoc :cursor (:cursor b))
               (some? (:epoch b)) (assoc :epoch (:epoch b))
               (some? (:more? b)) (assoc :more? (boolean (:more? b)))
               (some? (:hidden b)) (assoc :hidden (int (:hidden b)))
               (some? (:orphans b)) (assoc :orphans (int (:orphans b))))
             {:ok? false :consumer consumer :senses [] :reason :drain-failed
              :error "the port's :drain! answered nil"}))
         (catch Throwable t
           {:ok? false :consumer consumer :senses [] :reason :drain-failed
            :error (or (.getMessage t) (str (class t)))}))
       {:ok? false :consumer consumer :senses [] :reason :no-drain}))))

(m/=> drain-senses [:function
                    [:=> [:cat :any] DrainResult]
                    [:=> [:cat :any [:maybe :map]] DrainResult]])

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
