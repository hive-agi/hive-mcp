(ns hive-mcp.await.resumers.session
  "Owner kinds for a waiter that is NOT a hive-agent run: :session and
   :journal.

   An app coordinator (a Claude Code session) cannot be resumed by spawning
   a ling, and before this the only resumer was :hive-agent-run, so its
   `swarm await create` was refused with :await/unknown-resumer and its wait
   fell back to a sleep timer that left no record (card RUN-LEDGER-1,
   friction 20261125100548-20ddd01b).

   :session  {:kind :session :inbox <inbox id> :session? s :label? s}
             The wake is a message sent to the session's hive inbox
             (`swarm inbox read` sees it), and it is journaled.
   :journal  {:kind :journal :label? s}
             Nobody is resumed: the wake is only journaled. Use it for a
             wait whose outcome is read later, e.g. by a run ledger.

   Both kinds never park, so create arms them at once (ports/arm-on-create?).
   The source side is left alone: any open-source (:manual today, the
   RUN-LEDGER-4 sources later) wakes these owners like any other.

   Ports (DIP): the inbox and the journal are protocols, IWakeInbox and
   IWakeJournal. *inbox* and *journal* hold the adapters and are read when
   the resumer is OPENED, i.e. once per awaiter, so a caller can bind a
   double around create! and the resume thread uses the same one. The
   defaults are the NATS inbox (resolved late, so loading this namespace
   does not load the bus) and an EDN-lines file next to the registry.

   A delivery that cannot reach the bus throws, so the registry keeps the
   wake :waking and re-drives it (at-least-once). The inbox message id is
   <awaiter id>-<attempt>: within the stream's duplicate window a re-sent
   attempt is stored once."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-mcp.await.ports :as ports]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Values
;; =============================================================================

(def SessionOwner
  "A :session owner: the inbox its wake is sent to."
  [:map
   [:kind [:= :session]]
   [:inbox [:string {:min 1}]]
   [:session {:optional true} [:maybe :string]]
   [:label {:optional true} [:maybe :string]]])

(def JournalOwner
  "A :journal owner: nobody to resume, only a label for the journal."
  [:map
   [:kind [:= :journal]]
   [:label {:optional true} [:maybe :string]]])

(def WakeRequest
  "What the registry hands a resumer."
  [:map
   [:awaiter :map]
   [:payload {:optional true} [:maybe :map]]
   [:prompt {:optional true} [:maybe :string]]])

(def JournalEntry
  "One journaled wake."
  [:map
   [:awaiter-id :string]
   [:kind :keyword]
   [:attempt :int]
   [:at :int]
   [:reason {:optional true} [:maybe :keyword]]
   [:label {:optional true} [:maybe :string]]
   [:inbox {:optional true} [:maybe :string]]
   [:payload {:optional true} [:maybe :map]]
   [:delivered {:optional true} [:maybe :map]]])

;; =============================================================================
;; Pure calculations
;; =============================================================================

(defn message-id
  "The inbox message id of the wake of `awaiter-id` on `attempt`. Pure."
  [awaiter-id attempt]
  (str awaiter-id "-" (long (or attempt 1))))

(m/=> message-id [:=> [:cat :string [:maybe :int]] :string])

(defn wake-message
  "The inbox message a :session owner receives for `req`. The body is plain
   data for JSON: the payload travels as EDN text so keywords and sets keep
   their meaning. Pure."
  [owner {:keys [awaiter payload prompt]}]
  (let [id (str (:id awaiter))]
    {:to   (:inbox owner)
     :from "await"
     :id   (message-id id (:attempt awaiter))
     :body {:type       "await/wake"
            :awaiter-id id
            :reason     (some-> (:reason payload) name)
            :label      (:label owner)
            :prompt     prompt
            :payload    (pr-str payload)}}))

(m/=> wake-message [:=> [:cat SessionOwner WakeRequest] :map])

(defn journal-entry
  "The journal entry for the wake `req` of `owner` at `now`, with what the
   inbox answered (`delivered`, nil for a :journal owner). Pure."
  [owner {:keys [awaiter payload]} now delivered]
  (cond-> {:awaiter-id (str (:id awaiter))
           :kind       (:kind owner)
           :attempt    (long (or (:attempt awaiter) 1))
           :at         (long now)
           :reason     (:reason payload)
           :payload    payload}
    (:label owner) (assoc :label (:label owner))
    (:inbox owner) (assoc :inbox (:inbox owner))
    delivered      (assoc :delivered delivered)))

(m/=> journal-entry [:=> [:cat [:map [:kind :keyword]] WakeRequest :int [:maybe :map]] JournalEntry])

;; =============================================================================
;; Ports
;; =============================================================================

(defprotocol IWakeInbox
  (send-wake! [inbox message]
    "Send `message` {:to :from :id :body}; answer a map describing the
     delivery. Throw when it cannot be delivered now."))

(defprotocol IWakeJournal
  (append-wake! [journal entry]
    "Record `entry` durably; answer it."))

(defrecord NatsInbox []
  IWakeInbox
  (send-wake! [_ message]
    (let [conn-fn (requiring-resolve 'hive-mcp.nats.client/current-connection)
          send-fn (requiring-resolve 'hive-mcp.nats.inbox/send!)
          conn    (conn-fn)]
      (when-not conn
        (throw (ex-info "await: no connected event bus; the wake is re-driven"
                        {:error :await/no-bus :inbox (:to message)})))
      (send-fn conn message))))

(defn default-journal-path
  "wakes.ednl beside the await registry file."
  []
  (let [reg-path ((requiring-resolve 'hive-mcp.await.registry/default-path))]
    (str (io/file (.getParentFile (.getAbsoluteFile (io/file reg-path))) "wakes.ednl"))))

(m/=> default-journal-path [:=> [:cat] :string])

(defrecord EdnlJournal [path]
  IWakeJournal
  (append-wake! [_ entry]
    (let [f (io/file (or path (default-journal-path)))]
      (.mkdirs (.getParentFile (.getAbsoluteFile f)))
      (locking EdnlJournal
        (spit f (str (pr-str entry) "\n") :append true))
      entry)))

(def ^:dynamic *inbox*
  "The IWakeInbox a :session resumer delivers through, read when the
   resumer is opened."
  (->NatsInbox))

(def ^:dynamic *journal*
  "The IWakeJournal every wake of these kinds is recorded in, read when the
   resumer is opened."
  (->EdnlJournal nil))

(def ^:dynamic *now*
  "The clock (epoch ms) journal entries are stamped with."
  #(System/currentTimeMillis))

;; =============================================================================
;; Resumers
;; =============================================================================

(defn- refuse-invalid! [schema owner]
  (when-let [explain (m/explain schema owner)]
    (throw (ex-info (str "await: invalid " (name (:kind owner)) " owner"
                         (when (= :session (:kind owner))
                           ": it needs :inbox (the session's inbox id)"))
                    {:error   :await/invalid-owner
                     :owner   owner
                     :explain (pr-str (mapv #(select-keys % [:path :type]) (:errors explain)))}))))

(defmethod ports/open-resumer :session
  [owner]
  (let [owner   (cond-> owner (string? (:inbox owner)) (update :inbox str/trim))
        _       (refuse-invalid! SessionOwner owner)
        inbox   *inbox*
        journal *journal*
        now     *now*]
    (fn resume-session [req]
      (let [delivered (send-wake! inbox (wake-message owner req))
            entry     (append-wake! journal (journal-entry owner req (now) delivered))]
        {:delivered delivered :journaled (select-keys entry [:awaiter-id :attempt :at])}))))

(defmethod ports/open-resumer :journal
  [owner]
  (refuse-invalid! JournalOwner owner)
  (let [journal *journal*
        now     *now*]
    (fn resume-journal [req]
      (let [entry (append-wake! journal (journal-entry owner req (now) nil))]
        {:journaled (select-keys entry [:awaiter-id :attempt :at])}))))

(defmethod ports/arm-on-create? :session [_owner] true)
(defmethod ports/arm-on-create? :journal [_owner] true)
