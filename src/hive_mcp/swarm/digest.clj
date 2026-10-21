(ns hive-mcp.swarm.digest
  "Compact swarm status rows projected from the hivemind shout ring.

   Compat shim: moved to hive-agent.swarm.digest in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.digest" sym))

;; Value schemas for the shim contracts, transcribed from the docstrings of
;; hive-agent.swarm.digest.

(def Shout
  "One shout in an agent's ring."
  [:map
   [:timestamp {:optional true} [:maybe :int]]
   [:event-type {:optional true} [:maybe :keyword]]
   [:message {:optional true} [:maybe :string]]
   [:task {:optional true} [:maybe :string]]
   [:parent-id {:optional true} [:maybe :string]]
   [:data {:optional true} [:maybe [:map [:turn {:optional true} :any]]]]])

(def Entry
  "A registry entry: the agent's shout ring under [:data :messages]."
  [:map [:data {:optional true} [:maybe [:map [:messages {:optional true} [:sequential Shout]]]]]])

(def Row
  "A status row: :a agent, :e last event, :m clipped message, :t task, :turn,
   :parent, :idle-s, :shouts."
  [:map
   [:a :any]
   [:e [:maybe :string]]
   [:shouts [:int {:min 1}]]
   [:idle-s [:int {:min 0}]]
   [:m {:optional true} :string]
   [:t {:optional true} :string]
   [:turn {:optional true} number?]
   [:parent {:optional true} :string]])

(defn agent-row
  {:arglists '([now agent-id entry] [now agent-id entry cap])}
  [& args]
  (apply (impl 'agent-row) args))

;; nil when the ring holds no shouts
(m/=> agent-row [:function
                 [:=> [:cat :int :any [:maybe Entry]] [:maybe Row]]
                 [:=> [:cat :int :any [:maybe Entry] [:int {:min 0}]] [:maybe Row]]])

(def default-message-cap @(impl 'default-message-cap))

(defn render
  {:arglists '([rows])}
  [& args]
  (apply (impl 'render) args))

(m/=> render [:=> [:cat [:sequential Row]] :string])

(defn roster
  {:arglists '([now registry] [now registry {:keys [only-children-of message-cap]}])}
  [& args]
  (apply (impl 'roster) args))

(m/=> roster [:function
              [:=> [:cat :int [:maybe [:map-of :any Entry]]] [:vector Row]]
              [:=> [:cat :int [:maybe [:map-of :any Entry]]
                    [:maybe [:map [:only-children-of {:optional true} :any]
                             [:message-cap {:optional true} [:maybe [:int {:min 0}]]]]]]
               [:vector Row]]])
