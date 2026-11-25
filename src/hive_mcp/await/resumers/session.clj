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
             (`swarm inbox read` sees it). :woken means STORED in the inbox
             stream, not read: an inbox nobody arms keeps it for the
             stream's retention.
   :journal  {:kind :journal :label? s}
             Nobody is resumed. Its wake is recorded only by the registry's
             wake journal (ports/IWakeJournal), which records every
             attempt of every owner kind.

   Both kinds never park, so create inserts them already :armed
   (ports/arm-on-create?). The source side is left alone: any open-source
   (:manual today, the RUN-LEDGER-4 sources later) wakes these owners.

   The inbox is a port (ports/IWakeInbox), read from *inbox* when the
   resumer is OPENED (once per awaiter, at create or at restore), so a
   caller binds a double around create! and the resume thread uses the same
   one. The default adapter is hive-mcp.await.adapters.nats-inbox.

   The message id is <awaiter id>-wake: an awaiter wakes at most once, so
   every re-driven attempt publishes the SAME id and the stream stores it
   once within its duplicate window. A send that cannot reach the bus
   throws, the wake stays :waking and is re-driven."
  (:require [clojure.string :as str]
            [hive-mcp.await.adapters.nats-inbox :as nats-inbox]
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

;; =============================================================================
;; Pure calculations
;; =============================================================================

(defn message-id
  "The inbox message id of `awaiter-id`'s wake: the same for every attempt,
   since an awaiter wakes at most once. Pure."
  [awaiter-id]
  (str awaiter-id "-wake"))

(m/=> message-id [:=> [:cat :string] :string])

(defn wake-message
  "The inbox message a :session owner receives for `req`. The body is plain
   data for JSON: the payload travels as EDN text so keywords and sets keep
   their meaning. Pure."
  [owner {:keys [awaiter payload prompt]}]
  (let [id (str (:id awaiter))]
    {:to   (:inbox owner)
     :from "await"
     :id   (message-id id)
     :body {:type       "await/wake"
            :awaiter-id id
            :reason     (some-> (:reason payload) name)
            :label      (:label owner)
            :prompt     prompt
            :payload    (pr-str payload)}}))

(m/=> wake-message [:=> [:cat SessionOwner WakeRequest] :map])

;; =============================================================================
;; Port binding
;; =============================================================================

(def ^:dynamic *inbox*
  "The ports/IWakeInbox a :session resumer delivers through, read when the
   resumer is opened."
  (nats-inbox/inbox))

;; =============================================================================
;; Resumers
;; =============================================================================

(defn- refuse! [owner message]
  (throw (ex-info (str "await: invalid " (name (:kind owner)) " owner: " message)
                  {:error :await/invalid-owner :owner owner})))

(defmethod ports/open-resumer :session
  [owner]
  (let [owner (cond-> owner (string? (:inbox owner)) (update :inbox str/trim))
        inbox *inbox*]
    (when-let [explain (m/explain SessionOwner owner)]
      (refuse! owner (str "it needs :inbox (the session's inbox id); "
                          (pr-str (mapv :path (:errors explain))))))
    (when-not (ports/valid-address? inbox (:inbox owner))
      (refuse! owner (str "inbox " (pr-str (:inbox owner))
                          " is not a deliverable inbox id (1-64 letters, digits, '-' or '_')")))
    (fn resume-session [req]
      {:delivered (ports/send-wake! inbox (wake-message owner req))})))

(defmethod ports/open-resumer :journal
  [owner]
  (when (m/explain JournalOwner owner)
    (refuse! owner "only :label may accompany :kind :journal"))
  (fn resume-journal [_req]
    {:journaled true}))

(defmethod ports/arm-on-create? :session [_owner] true)
(defmethod ports/arm-on-create? :journal [_owner] true)
