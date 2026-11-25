(ns hive-mcp.await.adapters.nats-inbox
  "The default ports/IWakeInbox adapter: the hive inbox on HIVE_INBOX
   (hive-mcp.nats.inbox). The bus is resolved at CALL time, so loading this
   namespace loads nothing of the NATS stack."
  (:require [hive-mcp.await.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord NatsInbox []
  ports/IWakeInbox
  (valid-address? [_ address]
    (boolean ((requiring-resolve 'hive-mcp.nats.inbox/valid-inbox-id?) address)))
  (send-wake! [_ message]
    (let [conn ((requiring-resolve 'hive-mcp.nats.client/current-connection))]
      (when-not conn
        (throw (ex-info "await: no connected event bus; the wake is re-driven"
                        {:error :await/no-bus :inbox (:to message)})))
      ((requiring-resolve 'hive-mcp.nats.inbox/send!) conn message))))

(defn inbox
  "A NatsInbox."
  []
  (->NatsInbox))
