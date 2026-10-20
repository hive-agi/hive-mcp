(ns hive-mcp.channel.inbound
  "Messages an operator surface (Emacs, a web UI) SENDS to the server over the
   WebSocket channel.

   The channel was started without an :on-message handler, so everything a
   client sent was read and dropped. Emacs answers a hivemind ask by sending
   {type \"hivemind-response\" ask-id .. decision ..}; that answer never reached
   the asking agent, which then sat out its timeout.

   `hivemind-response` (pure) reads the answer off a message, and
   `handle-client-message!` delivers it. The answer is any text, and both
   `answer` and `decision` are accepted as its key."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- field
  "Value of key `k` in `msg`, keyword or string keyed: the WebSocket channel
   reads JSON with keyword keys, the bencode channel keeps strings."
  [msg k]
  (let [v (get msg (keyword k) (get msg k))]
    (if (keyword? v) (name v) v)))

(defn hivemind-response
  "Pure: [ask-id answer by] for a client's hivemind-response message, or nil
   when `msg` is not one or carries no answer."
  [msg]
  (when (and (map? msg) (= "hivemind-response" (field msg "type")))
    (let [ask-id (field msg "ask-id")
          answer (or (field msg "answer") (field msg "decision"))
          by (field msg "by")]
      (when (and (string? ask-id) (not (str/blank? ask-id))
                 (string? answer) (not (str/blank? answer)))
        [ask-id answer (if (str/blank? by) "human" by)]))))

(defn handle-client-message!
  "The WebSocket channel's :on-message: deliver a client's answer to the
   pending ask it names. Other messages are logged and ignored. Never
   throws: a bad message must not close the client's socket."
  [msg client-id]
  (try
    (if-let [[ask-id answer by] (hivemind-response msg)]
      (let [respond! (requiring-resolve 'hive-mcp.hivemind.messaging/respond-ask!)
            delivered? (respond! ask-id answer :by by)]
        (log/info "Client" client-id "answered ask" ask-id
                  (if delivered? "(delivered)" "(no such pending ask)"))
        delivered?)
      (log/debug "Client" client-id "sent an unrouted message:" (field msg "type")))
    (catch Exception e
      (log/warn "Client" client-id "message failed:" (.getMessage e))
      nil)))
