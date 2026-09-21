;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.adapters.messaging
  "The host-side adapter for hive-spi.swarm.ports.messaging.

   Implements the messaging port protocols by DELEGATING to the existing
   hive-mcp channel/delivery vars: no behaviour is invented here, this
   namespace is pure plumbing over channel.core, channel.*, channel.a2a
   and protocols.delivery-channel.

   Delivery is PLATFORM-AGNOSTIC at the port: this adapter implements
   IConversationStore (the read side) and the other messaging protocols,
   while OUTBOUND delivery to any given platform is a registered
   IInboxSink (see hive-mcp.swarm.adapters.inbox.piggyback and its
   siblings). Nothing here knows which platforms exist.

   Install once from addon init with `install!`; the swarm slice then
   reaches the fabric only through hive-spi.swarm.ports.messaging and
   never requires a hive-mcp namespace directly."
  (:require [hive-spi.swarm.ports.messaging :as spi]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.channel.websocket :as ws]
            [hive-mcp.channel.audience :as audience]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.channel.a2a :as a2a]
            [hive-mcp.channel.broadcast-policy :as policy]
            [hive-mcp.channel.broadcast-ledger :as ledger]
            [hive-mcp.channel.payload-ref :as payload-ref]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.protocols.delivery-channel :as dc]
            [taoensso.timbre :as log]))

;;; ============================================================================
;;; The adapter: every method delegates to an existing hive-mcp var
;;; ============================================================================

(defn- emit-leg!
  "Run one transport's emit THUNK. A throw is logged under TRANSPORT and
   dropped, so the remaining legs still run and the port never throws."
  [transport thunk]
  (try
    (thunk)
    (catch Exception e
      (log/warn "frontend push:" transport "leg failed:" (.getMessage e))))
  nil)

(defn make-adapter
  "The messaging port implementation backed by the live hive-mcp fabric."
  []
  (reify

    spi/IEventBus
    (publish! [_ event]
      (channel/publish! event))
    (subscribe! [_ event-type]
      (channel/subscribe! event-type))
    (unsubscribe! [_ event-type ch]
      (channel/unsubscribe! event-type ch))

    spi/IFrontendPush
    (broadcast! [_ msg]
      (channel/broadcast! msg))
      ;; TODO: IFrontendPush covers both transports: also mirror MSG to
      ;; hive-mcp.channel.websocket/broadcast! once the swarm call sites
      ;; that conflate the two surfaces are migrated. emit! already does.
    (emit! [_ event-type data]
      ;; One verb, both transports: emit-event! covers the channel socket
      ;; and the local bus, ws/emit! the browser clients. Each leg is
      ;; guarded on its own, so a failing transport neither throws through
      ;; the port nor silences the other one: UI clients are spectators.
      (emit-leg! :channel #(channel/emit-event! event-type data))
      (emit-leg! :websocket #(ws/emit! event-type data))
      nil)
    (frontend-status [_]
      {:channel-connected? (boolean (channel/server-connected?))
       :ws-connected? (boolean (ws/connected?))
       :ws-clients (long (or (ws/client-count) 0))})

    spi/IAudience
    (coordinator-reader? [_ reader-id]
      (audience/coordinator-reader? reader-id))
    (coordinator-session [_ id]
      (audience/coordinator-session id))

    spi/IConversationStore
    (register-message-source! [_ source-fn]
      (piggyback/register-message-source! source-fn))
    (fetch-conversation [_ context-id opts]
      (let [{:keys [limit] :or {limit 100}} (or opts {})]
        (or (piggyback/fetch-conversation context-id :limit limit) [])))
    (new-conversation-id [_]
      (a2a/new-context-id))

    spi/IBroadcastGovernance
    (apply-policy [_ msg opts]
      (policy/apply-policy msg opts))
    (spent-recently [_ project-id]
      (ledger/spent-recently project-id))
    (spent-recently [_ project-id now]
      (ledger/spent-recently project-id now))
    (record-broadcast! [_ project-id]
      (ledger/record-broadcast! project-id))
    (record-broadcast! [_ project-id now]
      (ledger/record-broadcast! project-id now))

    spi/IContextStore
    (elide! [_ s put-fn]
      (payload-ref/elide! s put-fn))
    (elide! [_ s cap put-fn]
      (payload-ref/elide! s cap put-fn))
    (context-put! [_ data opts]
      (let [{:keys [tags ttl-ms]} (or opts {})]
        (context-store/context-put! data :tags (or tags #{}) :ttl-ms ttl-ms)))

    spi/IDeliveryFanout
    (fanout! [_ payload]
      (dc/fanout! payload))
    (delivery-channels [_]
      (mapv (fn [ch]
              {:id (dc/channel-id ch)
               :available? (boolean (dc/available? ch))})
            (dc/get-channels)))))

(defn install!
  "Install the hive-mcp-backed adapter as the active messaging port.
   Call once from addon init, inside the host JVM. Returns the adapter."
  []
  (spi/set-messaging! (make-adapter)))
