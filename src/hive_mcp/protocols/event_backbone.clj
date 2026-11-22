(ns hive-mcp.protocols.event-backbone
  "Protocol definitions for event backbone (pub/sub) backends.

   Abstracts NATS/Redis/Kafka behind an IEventBackbone protocol so that
   domain code never depends on a specific messaging system directly.

   Pattern: follows protocols/editor.clj (active atom + NoopBackbone fallback).
   Headless mode is a first-class citizen — NoopBackbone degrades gracefully."
  (:require [hive-mcp.protocols.registry :as reg]
            [malli.core :as m]
            [hive-mcp.resilience.swallow :as swallow]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; IEventBackbone Protocol
;;; ============================================================================

(defprotocol IEventBackbone
  "Backend protocol for event pub/sub backbone.

   Implementations:
   - NatsBackbone   (nats/backbone.clj)
   - NoopBackbone   (this file, headless fallback)
   - [future: RedisBackbone, KafkaBackbone, ...]"

  (backbone-id [this]
    "Return keyword identifying this backbone backend.
     Examples: :nats, :noop, :redis")

  (connected? [this]
    "Return true if the backbone is reachable and operational.")

  (publish! [this subject payload]
    "Publish a message to a subject/topic.
     payload is a Clojure map — serialization is the backend's responsibility.
     No-op if disconnected. Returns nil.")

  (subscribe! [this subject handler-fn]
    "Subscribe to a subject/topic with a handler function.
     handler-fn receives a deserialized Clojure map.
     Returns an opaque subscription handle, or nil if disconnected.")

  (unsubscribe! [this subject]
    "Unsubscribe from a subject/topic. Safe to call when not subscribed."))

(defprotocol IChainStatus
  "Optional: a backbone served by a TransportChain reports the chain's state
   (chain, active adapter, per-adapter health, :degraded?) so backbone-status
   can show a degraded bus instead of a bare connected flag."
  (chain-status [this]
    "Map with :port :chain :active :resolved-by :adapters :switches :degraded?."))

;;; ============================================================================
;;; Active Backbone Management
;;; ============================================================================

(declare ->NoopBackbone)

(defonce ^:private slot
  (reg/single-slot {:validate #(satisfies? IEventBackbone %)
                    :on-empty #(->NoopBackbone)}))

(defn set-backbone!
  "Set the active event backbone implementation."
  [backbone]
  (reg/install! slot backbone))

(defn backbone-set?
  "Check if a backbone has been explicitly configured."
  []
  (reg/present? slot))

(defn get-backbone
  "Get the active backbone. Returns NoopBackbone when none is configured
   (headless mode is a first-class citizen)."
  []
  (reg/current slot))

(defn clear-backbone!
  "Clear the active backbone."
  []
  (reg/clear! slot))

(defn add-backbone-watch!
  "Run `(f old new)` when the active backbone changes (e.g. Noop->Nats)."
  [key f]
  (reg/watch-slot! slot key f))

(defn remove-backbone-watch!
  "Remove the backbone watch registered under `key`. Safe when absent."
  [key]
  (reg/unwatch-slot! slot key))

;;; ============================================================================
;;; NoopBackbone (Headless Fallback)
;;; ============================================================================

(defrecord NoopBackbone []
  IEventBackbone
  (backbone-id [_this] :noop)
  (connected? [_this] false)
  (publish! [_this _subject _payload] nil)
  (subscribe! [_this _subject _handler-fn] nil)
  (unsubscribe! [_this _subject] nil))

(defn noop-backbone
  "Create a no-op backbone fallback."
  []
  (->NoopBackbone))

(def BackboneStatus
  [:map
   [:backbone-id :keyword]
   [:installed? :boolean]
   [:connected? :boolean]
   [:working? :boolean]
   [:degraded? {:optional true} :boolean]
   [:chain {:optional true} :map]
   [:summary :string]])

(defn backbone-status
  "Which backbone is active and whether it actually works. :working? is true
   only for an installed, non-noop backbone that reports connected? now, so
   neither a NoopBackbone nor a disconnected NatsBackbone passes as a bus.
   A chain-backed backbone also reports :chain (its IChainStatus view) and
   :degraded?: true when the preferred adapter does not serve the port, so a
   bus running on its fallback reads as degraded, never as healthy."
  []
  (let [bb   (get-backbone)
        id   (backbone-id bb)
        inst (and (backbone-set?) (not= :noop id))
        up?  (boolean (swallow/attempt-ex (connected? bb)))
        ok?  (and inst up?)
        view (when (and inst (satisfies? IChainStatus bb))
               (swallow/attempt-ex (chain-status bb)))
        deg? (boolean (:degraded? view))]
    (cond-> {:backbone-id id
             :installed?  (boolean inst)
             :connected?  up?
             :working?    (boolean ok?)
             :summary     (cond
                            (not inst) "no event bus: NoopBackbone, events stay in-process"
                            (and view deg?)
                            (str "event bus DEGRADED: serving on " (name (or (:active view) :none))
                                 " (preferred " (name (first (:chain view))) " unavailable)")
                            ok?        (str (name id) " backbone connected")
                            :else      (str (name id) " backbone installed but NOT connected"))}
      view (assoc :chain view :degraded? deg?))))

(m/=> backbone-status [:=> [:cat] BackboneStatus])

(def Backbone
  "Any value satisfying IEventBackbone; generates NoopBackbones."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (->NoopBackbone))}
   #(satisfies? IEventBackbone %)])

;; The input is loose: the slot's :validate assert rejects a non-backbone.
(m/=> set-backbone! [:=> [:cat :any] Backbone])

(m/=> backbone-set? [:=> [:cat] :boolean])

(m/=> get-backbone [:=> [:cat] Backbone])

(m/=> clear-backbone! [:=> [:cat] :nil])

(m/=> add-backbone-watch! [:=> [:cat :any ifn?] reg/SlotValue])

(m/=> remove-backbone-watch! [:=> [:cat :any] reg/SlotValue])

(m/=> noop-backbone [:=> [:cat] Backbone])
