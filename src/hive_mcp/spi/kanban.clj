(ns hive-mcp.spi.kanban
  "DEPRECATED shim over hive-spi.kanban. Require hive-spi.kanban instead.

   The kanban port lived here, host-local, until hive-spi released it (1.4.0).
   Core now requires the library namespace. This shim stays for one release so
   an addon that still names hive-mcp.spi.kanban keeps working, and it holds no
   protocol or schema of its own: every name below IS the library's value, so
   a record that implements `IKanbanRead` here satisfies
   hive-spi.kanban/IKanbanRead and registers in the one library registry."
  (:require [hive-spi.kanban :as kanban]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schemas (the library's values)
;; =============================================================================

(def Status kanban/Status)
(def Priority kanban/Priority)
(def Task kanban/Task)
(def ListQuery kanban/ListQuery)
(def TransitionRequest kanban/TransitionRequest)
(def CreateRequest kanban/CreateRequest)
(def WriteError kanban/WriteError)
(def TransitionResult kanban/TransitionResult)
(def CreateResult kanban/CreateResult)

;; =============================================================================
;; Protocols and their methods (the library's protocol objects)
;; =============================================================================

(def IKanbanRead kanban/IKanbanRead)
(def IKanbanWrite kanban/IKanbanWrite)

(def list-tasks kanban/list-tasks)
(def get-task kanban/get-task)
(def transition! kanban/transition!)
(def create-task! kanban/create-task!)

;; =============================================================================
;; Conformance
;; =============================================================================

(defn conformance
  "See hive-spi.kanban/conformance."
  ([read-impl write-impl] (kanban/conformance read-impl write-impl))
  ([read-impl write-impl opts] (kanban/conformance read-impl write-impl opts)))

;; =============================================================================
;; Contracts
;; =============================================================================

(def ConformanceRow
  [:map {:closed true}
   [:case [:enum :list/returns-vector :list/every-task-valid :get/miss-is-nil
           :transition/missing-task-is-err :create/blank-title-is-err]]
   [:ok? :boolean]
   [:detail :any]])

(def ReadImpl
  [:fn {:error/message "should satisfy IKanbanRead"
        :gen/schema :nil
        :gen/fmap (fn [_] (reify kanban/IKanbanRead (list-tasks [_ _] []) (get-task [_ _] nil)))}
   (fn [x] (satisfies? kanban/IKanbanRead x))])

(def WriteImpl
  [:fn {:error/message "should satisfy IKanbanWrite"
        :gen/schema :nil
        :gen/fmap (fn [_] (reify kanban/IKanbanWrite
                            (transition! [_ _] {:err {:error :x}})
                            (create-task! [_ _] {:err {:error :x}})))}
   (fn [x] (satisfies? kanban/IKanbanWrite x))])

;; conformance must report a misbehaving impl, not throw on it, so the
;; impls are any value: a non-implementation lands in :detail
(m/=> conformance [:function
                   [:=> [:cat :any :any] [:vector ConformanceRow]]
                   [:=> [:cat :any :any [:maybe [:map
                                                 [:missing-id {:optional true} :string]
                                                 [:directory {:optional true} [:maybe :string]]]]]
                    [:vector ConformanceRow]]])
