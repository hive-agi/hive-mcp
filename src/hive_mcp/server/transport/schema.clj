(ns hive-mcp.server.transport.schema
  "Value objects shared by the transport starters under
   `hive-mcp.server.transport.*`: the component config a starter is handed,
   the status map it answers, and the host references (atoms, an nREPL
   server) a starter reads or fills.

   Data only. The `m/=>` contracts in each starter namespace read from here,
   and so do the facet tests, so a shape is written once."
  (:require [nrepl.server :as nrepl-server]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Port
  "A port as it reaches a starter: whatever the config resolved, which is a
   long from `parse-long`, or nil when no port is configured."
  [:maybe :int])

(def ComponentConfig
  "The system component config a starter is handed (integrant). Open and
   nil-able: a starter reads only :enabled, :port and :bind, and a key it
   does not read must not be rejected. :enabled is read for truthiness."
  [:maybe
   [:map
    [:enabled {:optional true} :any]
    [:port {:optional true} Port]
    [:bind {:optional true} [:maybe :string]]]])

(def StartStatus
  "What a starter answers once it was enabled: `:running` when listening,
   `:failed` when the start threw. :server is the websocket starter's actual
   port when it is running."
  [:map
   [:status [:enum :running :failed]]
   [:port Port]
   [:server {:optional true} :int]])

(def MaybeStartStatus
  "nil when the transport is not enabled, else a `StartStatus`."
  [:maybe StartStatus])

(def AtomRef
  "A mutable host reference a starter reads or fills (`reset!` / deref)."
  [:fn {:error/message "should be an atom"
        :gen/schema :nil
        :gen/fmap (fn [_] (atom nil))}
   (fn [x] (instance? clojure.lang.IAtom x))])

(def NreplServer
  "A started `nrepl.server/Server` record."
  [:fn {:error/message "should be an nrepl.server/Server"
        :gen/schema :nil
        :gen/fmap (fn [_] (nrepl-server/map->Server {}))}
   (fn [x] (instance? nrepl.server.Server x))])

(def EffectResult
  "`hive-mcp.dns.result/try-effect*`: {:ok v}, or {:error kw :class s
   :message s} when the body threw."
  [:or
   [:map [:ok :any]]
   [:map
    [:error :keyword]
    [:class {:optional true} [:maybe :string]]
    [:message {:optional true} [:maybe :string]]]])
