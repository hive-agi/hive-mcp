(ns hive-mcp.nats.bus
  "The hive event bus as one unit: backbone + NATS client + owned streams +
   subscriptions, reached the same way at boot and on every reconnect.

   Strata (CPPB):
     Value objects  BusStatus
     Boundary       connect!  (install backbone, register on-connect hooks,
                               start the async client; returns at once)
                    bus-status (fold backbone, client, last stream report)

   Seam for the next lanes (sense republish, inbox): add an on-connect hook
   with hive-mcp.nats.client/add-on-connect-hook!; it runs on the first
   connect and after every reconnect."
  (:require [hive-mcp.nats.client :as client]
            [hive-mcp.nats.streams :as streams]
            [hive-mcp.nats.backbone :as nats-bb]
            [hive-mcp.protocols.event-backbone :as eb]
            [taoensso.timbre :as log]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private last-stream-report (atom nil))

(def BusStatus
  [:map
   [:working? :boolean]
   [:backbone eb/BackboneStatus]
   [:nats client/ClientStatus]
   [:streams [:map
              [:declared [:vector :string]]
              [:last-ensure [:maybe [:vector streams/EnsureReport]]]]]])

(defn bus-status
  "Plain answer to: is there a working event bus, and which one."
  []
  (let [bb (eb/backbone-status)]
    {:working? (:working? bb)
     :backbone bb
     :nats     (client/status)
     :streams  {:declared    (mapv :name streams/default-streams)
                :last-ensure @last-stream-report}}))

(m/=> bus-status [:=> [:cat] BusStatus])

(defn- ensure-streams-hook [conn]
  (reset! last-stream-report (streams/ensure-streams! conn streams/default-streams)))

(defn once-per-connection
  "Wrap `f` so it runs once per jnats Connection OBJECT. A jnats reconnect
   heals the same Connection and re-arms its subscriptions, so subscribing
   again there would double every handler; a new Connection (stop!/start!)
   starts empty and gets `f` again."
  [f]
  (let [seen (atom nil)]
    (fn [conn]
      (when-not (identical? conn @seen)
        (reset! seen conn)
        (f conn)))))

(m/=> once-per-connection [:=> [:cat ifn?] ifn?])

(defn connect!
  "Boot path. Installs NatsBackbone, registers the stream hook and each
   `on-connect` fn (key -> (fn [conn])), then starts the async client.
   Everything that needs a live connection runs from the hooks, so boot
   with the broker down and a later reconnect converge on the same state.
   Never blocks on the broker; never throws."
  [nats-config on-connect]
  (eb/set-backbone! (nats-bb/create-backbone))
  (client/add-on-connect-hook! ::streams #'ensure-streams-hook)
  (doseq [[k f] on-connect] (client/add-on-connect-hook! k f))
  (client/start! nats-config)
  (log/info "[bus] NatsBackbone installed; client connecting asynchronously")
  nil)

(m/=> connect! [:=> [:cat :map [:maybe [:map-of :any ifn?]]] :nil])
