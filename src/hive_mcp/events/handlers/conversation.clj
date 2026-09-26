(ns hive-mcp.events.handlers.conversation
  "Event handlers for the Inter-Ling Conversation Protocol.

   Handlers are PURE: they take coeffects + event, return effects.
   All transport (NATS publish, atom mutation, promise delivery) happens in
   `hive-mcp.events.effects.conversation`.

   Handled events:
     - :conversation/tell     — fire-and-forget DM
     - :conversation/ask      — DM that registers a pending response
     - :conversation/respond  — answer to a prior ask, correlated by :ask-id

   Effect surface (consumed by effects/conversation):
     - :conversation/publish-tell      {:envelope ...}
     - :conversation/publish-ask       {:envelope ... :ask-id ...}
     - :conversation/publish-respond   {:envelope ...}
     - :conversation/deliver-response  {:ask-id ... :answer ...}
     - :conversation/inbox-push        {:agent-id ... :envelope ...}
     - :log                            (existing)"

  (:require [hive-mcp.events.core :as ev]
            [hive-mcp.events.interceptors.conversation :as cix]
            [hive-mcp.hivemind.conversation :as conv]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Handler: :conversation/tell
;; =============================================================================

(defn handle-conversation-tell
  "Pure handler for :conversation/tell.

   Effects:
     - :conversation/publish-tell  → boundary publishes on NATS / inbox
     - :conversation/inbox-push    → adds to receiver's piggyback inbox
     - :log
   "
  [_coeffects [_ payload]]
  (let [envelope (conv/tell-envelope payload)]
    {:conversation/publish-tell {:envelope envelope}
     :conversation/inbox-push   {:agent-id (:to envelope)
                                 :envelope envelope}
     :log {:level :debug
           :message (str "tell " (:from envelope) " → " (:to envelope))}}))

;; =============================================================================
;; Handler: :conversation/ask
;; =============================================================================

(defn handle-conversation-ask
  "Pure handler for :conversation/ask.

   Effects:
     - :conversation/register-ask  → register the promise-chan
     - :conversation/publish-ask   → boundary publishes on NATS / inbox
     - :conversation/inbox-push    → drop into receiver's inbox section
     - :log

   The pure layer does NOT block; the parking happens in the tool-call
   boundary that synchronously calls (await-response! ask-id ...).
   This handler simply prepares state + transport."
  [_coeffects [_ payload]]
  (let [envelope (conv/ask-envelope payload)
        ask-id   (:ask-id envelope)]
    {:conversation/register-ask  {:envelope envelope}
     :conversation/publish-ask   {:envelope envelope :ask-id ask-id}
     :conversation/inbox-push    {:agent-id (:to envelope)
                                  :envelope envelope}
     :log {:level :debug
           :message (str "ask " (:from envelope) " → " (:to envelope)
                         " ask-id=" ask-id)}}))

;; =============================================================================
;; Handler: :conversation/respond
;; =============================================================================

(defn handle-conversation-respond
  "Pure handler for :conversation/respond.

   ALWAYS emits :conversation/deliver-response, correlated or not. The
   :conversation/correlated-ask coeffect (from `correlate-ask-id`) is a
   snapshot taken before this handler ran, so an unmatched respond may be
   one that overtook its own ask (an in-process responder answering inside
   publish, or a NATS respond racing the local ask event). Dropping it here
   was the ask window: the asker registered a moment later and parked until
   its timeout. deliver-response! buffers an answer for an unknown ask-id
   (bounded, TTL) and hands it over when the ask registers; for a sender on
   another node the buffered entry simply expires.

   Effects:
     - :conversation/deliver-response (always)
     - :conversation/publish-respond  (always, a remote sender may be parked)
     - :conversation/inbox-push       (so sender can also see the answer in piggyback)
     - :log"
  [coeffects [_ payload]]
  (let [envelope   (conv/respond-envelope payload)
        ask-id     (:ask-id envelope)
        correlated (:conversation/correlated-ask coeffects)]
    {:conversation/deliver-response {:ask-id ask-id :answer (:answer envelope)}
     :conversation/publish-respond  {:envelope envelope}
     :conversation/inbox-push       {:agent-id (:to envelope)
                                     :envelope envelope}
     :log (if correlated
            {:level :debug
             :message (str "respond delivered locally for ask-id " ask-id)}
            {:level :debug
             :message (str "respond for ask-id " ask-id
                           " has no local ask yet; buffered and published")})}))

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-handlers!
  "Register conversation event handlers.

   Safe to call multiple times, and it REGISTERS every time: `reg-event` is
   addressed by key and last-writer-wins. The `defonce`'d flag used to skip the
   body, which meant a hot reload could not rewire these handlers. Kanban
   20260916134011-1246379c.

   Returns true."
  []
  (let [first? (not @*registered)]
    (ev/reg-event :conversation/tell
                  cix/conversation-chain
                  handle-conversation-tell)

    (ev/reg-event :conversation/ask
                  cix/conversation-chain
                  handle-conversation-ask)

    (ev/reg-event :conversation/respond
                  cix/conversation-chain
                  handle-conversation-respond)

    (reset! *registered true)
    (when first?
      (log/info "[hive-events] Conversation handlers registered: :conversation/tell :conversation/ask :conversation/respond"))
    true))

(defn reset-registration!
  "Reset registration state. Test only."
  []
  (reset! *registered false))
