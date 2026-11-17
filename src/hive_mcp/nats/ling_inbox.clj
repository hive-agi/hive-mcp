(ns hive-mcp.nats.ling-inbox
  "The NATS inbox of a headless ling: hive.v1.inbox.<ling-id> on HIVE_INBOX,
   read by the durable inbox-<ling-id>.

   hive-mcp owns the inbox's life: `arm!` at spawn, `disarm!` on kill. The
   READER is the ling's loop: hive-agent attaches a consumer per run
   (hive-agent.loop.nats-inbox) and feeds the loop's own inbox with what it
   reads. `consumer-attached?` asks it, by soft resolution, so core never
   names hive-agent at compile time; with no hive-agent the answer is false
   and the :delivery chain keeps the in-process route.

   `route` is the ling-lane health view: NATS up, inbox armed and consumer
   attached are three separate facts, so 'the bus is up' is never read as
   'this ling can be reached over it'.

   Every effect is fail-soft (a NATS outage never fails a spawn or a kill)
   and every port is dynamic so tests drive it with no broker."
  (:require [hive-dsl.result :as r]
            [hive-mcp.extensions.soft :as soft]
            [hive-mcp.nats.client :as client]
            [hive-mcp.nats.inbox :as inbox]
            [malli.core :as m]
            [taoensso.timbre :as log])
  (:import [io.nats.client Connection PurgeOptions]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Ports
;; =============================================================================

(def ^:dynamic *connection*
  "0-arg port: the live NATS connection, or nil."
  (fn [] (client/current-connection)))

(defn- default-consumer-attached?
  [ling-id]
  (if-let [f (soft/resolve-soft 'hive-agent.loop.nats-inbox/attached?)]
    (boolean (r/rescue false (f ling-id)))
    false))

(def ^:dynamic *consumer-attached?*
  "1-arg port (ling id): true while the ling's loop reads its NATS inbox."
  default-consumer-attached?)

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn- purge-subject!
  "Drop what an earlier ling of the same name left on the subject, so a
   fresh durable (DeliverPolicy/All) never replays it to the new ling."
  [^Connection conn id]
  (.purgeStream (.jetStreamManagement conn) inbox/stream-name
                (-> (PurgeOptions/builder) (.subject (inbox/subject id)) (.build))))

(defn arm!
  "Arm ling `ling-id`'s inbox (idempotent). A first arm purges the subject
   first. Answers the inbox status map, or nil with no bus or a bad id."
  [ling-id]
  (when-let [id (inbox/safe-encode-inbox-id ling-id)]
    (when-let [conn (*connection*)]
      (r/rescue-log (str "arm ling inbox " id) nil
        (when-not (:armed? (inbox/status conn id))
          (r/rescue-log (str "purge ling inbox " id) nil (purge-subject! conn id)))
        (let [res (inbox/arm! conn id)]
          (log/info "Ling NATS inbox armed" {:ling-id ling-id :subject (:subject res)})
          res)))))

(defn disarm!
  "Disarm ling `ling-id`'s inbox: delete its durable and purge its subject.
   Answers the disarm map, or nil with no bus or a bad id."
  [ling-id]
  (when-let [id (inbox/safe-encode-inbox-id ling-id)]
    (when-let [conn (*connection*)]
      (r/rescue-log (str "disarm ling inbox " id) nil
        (let [res (inbox/disarm! conn id)]
          (r/rescue-log (str "purge ling inbox " id) nil (purge-subject! conn id))
          (log/info "Ling NATS inbox disarmed" {:ling-id ling-id :deleted? (:deleted? res)})
          res)))))

(m/=> arm! [:=> [:cat :any] [:maybe :map]])
(m/=> disarm! [:=> [:cat :any] [:maybe :map]])

;; =============================================================================
;; Route view
;; =============================================================================

(def Route
  [:map
   [:ling-id :any]
   [:nats-up? :boolean]
   [:armed? :boolean]
   [:consumer-attached? :boolean]
   [:reachable? :boolean]])

(defn route
  "Can ling `ling-id` be reached over NATS? Three separate facts: the bus is
   up, the ling's inbox is armed, its loop's consumer is attached. Only all
   three make it :reachable?. Never throws."
  [ling-id]
  (let [conn     (*connection*)
        id       (inbox/safe-encode-inbox-id ling-id)
        armed?   (boolean (and conn id
                               (r/rescue false (:armed? (inbox/status conn id)))))
        attached (boolean (and id (*consumer-attached?* ling-id)))]
    {:ling-id            ling-id
     :nats-up?           (some? conn)
     :armed?             armed?
     :consumer-attached? attached
     :reachable?         (and (some? conn) armed? attached)}))

(m/=> route [:=> [:cat :any] Route])
