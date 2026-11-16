(ns hive-mcp.transport.events
  "The event backbone as a TransportChain (port :events, default chain
   [:nats :local-channel]).

   ChainBackbone is an IEventBackbone facade: every protocol call runs
   through hive-mcp.transport.registry/call!, so the sticky active adapter,
   retry, degradation, the probe loop and the switch announcement all come
   from the chain. Adapters are open (registry/open-adapter on
   [:events <adapter>]); a Kafka backbone is one more defmethod.

   Adapters
     :nats           publish/subscribe over hive-mcp.nats.client. Every op
                     checks the live connection first, so a dropped broker is
                     an :unavailable outcome, never an assumed-connected call.
     :local-channel  the in-process fan-out. Shouts are fanned out to local
                     channels by the publisher before it reaches the
                     backbone (hivemind.messaging/route-shout!), so a publish
                     here is a recorded no-op and a subscribe is accepted
                     without a handle. It answers connected? false: events
                     stay in this JVM, and that is what status reports.

   Named slots: slot :default is port :events; any other slot S is port
   :events/S, configured under [:transports :events/S].

   A switch of the :events port is announced twice: the registry's own sink
   (hivemind shout) and a deliberate shout that sixth-sense perceives as a
   sense, so a coordinator hears the bus degrade."
  (:require [hive-dsl.result :as r]
            [hive-mcp.nats.client :as client]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-mcp.transport.registry :as reg]
            [malli.core :as m]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Ports and slots
;; =============================================================================

(defn- slot->port
  "Chain port of backbone slot `slot` (nil or :default is :events)."
  [slot]
  (if (or (nil? slot) (= :default slot))
    :events
    (keyword "events" (name slot))))

;; =============================================================================
;; Adapters
;; =============================================================================

(defn- unavailable
  [message]
  (r/err :events/unavailable {:error/kind :unavailable :message message}))

(defn- nats-call
  "One op against the live NATS connection; unavailable when it is down."
  [{:keys [op subject payload handler]}]
  (if-not (client/connected?)
    (unavailable "nats not connected")
    (case op
      :connected?   (r/ok true)
      :publish      (do (client/publish! subject payload) (r/ok nil))
      :subscribe    (if-let [h (client/subscribe! subject handler)]
                      (r/ok h)
                      (unavailable "nats subscribe refused: no dispatcher"))
      :unsubscribe  (do (client/unsubscribe! subject) (r/ok nil))
      (r/err :events/unknown-op {:error/kind :addressing :op op}))))

(defn- nats-probe
  []
  (if (client/connected?)
    (r/ok :connected)
    (unavailable "nats not connected")))

(defn- local-call
  "The in-process channel: always available, never a bus."
  [{:keys [op]}]
  (case op
    :connected?  (r/ok false)
    :publish     (r/ok nil)
    :subscribe   (r/ok nil)
    :unsubscribe (r/ok nil)
    (r/err :events/unknown-op {:error/kind :addressing :op op})))

(defmethod reg/open-adapter [:events :nats] [_ _]
  {:call #'nats-call :probe #'nats-probe})

(defmethod reg/open-adapter [:events :local-channel] [_ _]
  {:call #'local-call :probe (fn [] (r/ok :local))})

;; =============================================================================
;; Facade
;; =============================================================================

(defn- value-of
  [out]
  (when (r/ok? out) (get-in out [:ok :value])))

(defn- port-view
  [port]
  (get-in (reg/status) [:ports port]))

(defn- degraded-view?
  "True when the port is not served by the head of its chain."
  [view]
  (not= :primary (:resolved-by view)))

(defn- heal!
  "A sticky :local-channel must not shadow a NATS that is already back: an
   on-connect hook that subscribes would otherwise land on the fallback and
   get no handle. When the broker is connected and :nats is not active,
   probe now, ignoring the interval."
  [port]
  (when (and (client/connected?)
             (not= :nats (:active (port-view port))))
    (r/rescue-log "events heal" nil (reg/probe-port! port {:force? true}))))

(defn- via-chain!
  [port op]
  (heal! port)
  (reg/call! port op))

(defrecord ChainBackbone [port]
  eb/IEventBackbone
  (backbone-id [_]
    (or (:active (port-view port)) :events-chain))

  (connected? [_]
    (true? (value-of (via-chain! port {:op :connected?}))))

  (publish! [_ subject payload]
    (via-chain! port {:op :publish :subject subject :payload payload})
    nil)

  (subscribe! [_ subject handler-fn]
    (value-of (via-chain! port {:op :subscribe :subject subject :handler handler-fn})))

  (unsubscribe! [_ subject]
    (via-chain! port {:op :unsubscribe :subject subject})
    nil)

  eb/IChainStatus
  (chain-status [_]
    (let [view (port-view port)]
      (assoc view :port port :degraded? (degraded-view? view)))))

(defn chain-backbone
  "The chain-backed IEventBackbone of slot `slot` (default :default)."
  ([] (chain-backbone :default))
  ([slot] (->ChainBackbone (slot->port slot))))

(m/=> chain-backbone [:function
                      [:=> [:cat] :any]
                      [:=> [:cat [:maybe :keyword]] :any]])

;; =============================================================================
;; Switch announcement
;; =============================================================================

(defn- sense-message
  [{:keys [from to reason]}]
  (str "event bus " (name reason) ": " (or from "none") " -> " (or to "none")
       (when (not= :nats to) " (NATS unavailable, events stay in-process)")))

(defn- emit-sense!
  "A deliberate shout, which sixth-sense perceives as a sense."
  [sw]
  (when-let [shout! (requiring-resolve 'hive-mcp.hivemind.messaging/shout!)]
    (shout! "transport" :progress
            {:message     (sense-message sw)
             :deliberate? true
             :data        (select-keys sw [:port :from :to :reason])})))

(defn- announce-switch!
  [prev sw]
  (when prev (prev sw))
  (when (= :events (:port sw))
    (log/warn (sense-message sw))
    (r/rescue-log "events switch sense" nil (#'emit-sense! sw))))

(defonce ^:private installed? (atom false))

(defn install!
  "Wrap the registry's switch sink so an :events switch also emits a sense.
   Idempotent. Answers true when it wrapped, false when already wrapped."
  []
  (if (compare-and-set! installed? false true)
    (let [prev (reg/set-switch-sink! nil)]
      (reg/set-switch-sink! (fn [sw] (announce-switch! prev sw)))
      true)
    false))

(m/=> install! [:=> [:cat] :boolean])

(defn start!
  "Boot path: bind the registry, wrap the switch sink, start the probe loops
   and install the chain backbone in the event-backbone slot. Never blocks on
   the broker; a NATS that is down serves :local-channel until a probe finds
   it back."
  []
  (install!)
  (reg/install!)
  (eb/set-backbone! (chain-backbone))
  nil)

(m/=> start! [:=> [:cat] :nil])
