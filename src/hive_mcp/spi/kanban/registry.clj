(ns hive-mcp.spi.kanban.registry
  "DEPRECATED shim over hive-spi.kanban.registry. Require that instead.

   The kanban DIP registry lived here, host-local, until hive-spi released it
   (1.4.0). Core now registers and resolves its provider through the library
   namespace. This shim stays for one release for addons that still name it.
   It keeps NO provider table of its own: every fn forwards to the library, so
   a provider registered here is the provider the library facade answers from."
  (:require [hive-mcp.spi.kanban :as kanban]
            [hive-spi.kanban.registry :as registry]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def noops
  "Port key -> Noop instance (the library's table)."
  registry/noops)

(def port->protocol
  "Port key -> the protocol an implementation must satisfy (the library's table)."
  registry/port->protocol)

(defn noop?
  "See hive-spi.kanban.registry/noop?."
  [impl]
  (registry/noop? impl))

(defn register!
  "See hive-spi.kanban.registry/register!."
  [port impl]
  (registry/register! port impl))

(defn unregister!
  "See hive-spi.kanban.registry/unregister!."
  [port]
  (registry/unregister! port))

(defn reset-providers!
  "See hive-spi.kanban.registry/reset-providers!."
  []
  (registry/reset-providers!))

(defn registered?
  "See hive-spi.kanban.registry/registered?."
  [port]
  (registry/registered? port))

(defn provider
  "See hive-spi.kanban.registry/provider."
  [port]
  (registry/provider port))

(defn list-tasks
  "See hive-spi.kanban.registry/list-tasks."
  [query]
  (registry/list-tasks query))

(defn get-task
  "See hive-spi.kanban.registry/get-task."
  [id]
  (registry/get-task id))

(defn transition!
  "See hive-spi.kanban.registry/transition!."
  [request]
  (registry/transition! request))

(defn create-task!
  "See hive-spi.kanban.registry/create-task!."
  [request]
  (registry/create-task! request))

;; =============================================================================
;; Contracts
;; =============================================================================

(def Port [:enum :IKanbanRead :IKanbanWrite])

(def Provider
  "A registered implementation or a Noop. Generates as the Noops."
  [:fn {:error/message "should satisfy a kanban port"
        :gen/schema [:enum :IKanbanRead :IKanbanWrite]
        :gen/fmap noops}
   (fn [x] (or (satisfies? kanban/IKanbanRead x) (satisfies? kanban/IKanbanWrite x)))])

(m/=> noop? [:=> [:cat :any] :boolean])
;; an unknown port or a non-satisfying impl is a wiring error and throws, as
;; documented; the contract states the accepted domain
(m/=> register! [:=> [:cat Port Provider] Provider])
(m/=> unregister! [:=> [:cat :any] :nil])
(m/=> reset-providers! [:=> [:cat] :nil])
(m/=> registered? [:=> [:cat :any] :boolean])
(m/=> provider [:=> [:cat Port] Provider])
;; the facade is permissive in: the port promises never to throw, and a
;; provider decides what it does with an off-schema request
(m/=> list-tasks [:=> [:cat [:maybe :map]] [:vector kanban/Task]])
(m/=> get-task [:=> [:cat :any] [:maybe kanban/Task]])
(m/=> transition! [:=> [:cat [:maybe :map]] kanban/TransitionResult])
(m/=> create-task! [:=> [:cat [:maybe :map]] kanban/CreateResult])
