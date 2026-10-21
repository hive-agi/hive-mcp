(ns hive-mcp.transport.a2a.schema
  "A2A protocol data structures and JSON-RPC helpers.

   Pure data layer — no dependencies on hive-mcp internals.
   Defines A2A Task, Message, StatusUpdateEvent factories
   and JSON-RPC 2.0 success/error wrappers.

   Spec: https://google.github.io/A2A/"
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Error Codes (JSON-RPC + A2A-specific)
;; =============================================================================

(def error-codes
  "A2A-specific JSON-RPC error codes."
  {:parse-error      -32700
   :invalid-request  -32600
   :method-not-found -32601
   :invalid-params   -32602
   :internal-error   -32603
   :task-not-found   -32001
   :agent-not-found  -32002
   :task-canceled    -32003
   :unauthorized     -32004})

;; =============================================================================
;; Agent Status -> A2A Task State Translation
;; =============================================================================

(def agent-status->task-state
  "Maps DataScript slave status to A2A task state."
  {:spawning    "submitted"
   :working     "working"
   :blocked     "input-required"
   :idle        "completed"
   :error       "failed"
   :terminated  "canceled"})

;; =============================================================================
;; Value schemas (the A2A shapes the factories below build)
;; =============================================================================

(def TaskState
  "An A2A task state. Open: the factories copy whatever state they are
   handed, and callers pass the A2A vocabulary."
  [:string {:gen/elements ["submitted" "working" "input-required" "completed"
                           "failed" "canceled"]}])

(def JsonRpcId
  "A JSON-RPC id as the request carried it: a string, a number, or nil
   when the request could not be read."
  [:maybe [:or :string :int]])

(def Part
  "An A2A message part. Text parts carry :type \"text\" and :text."
  [:map [:type {:optional true} :string] [:text {:optional true} [:maybe :string]]])

(def Message
  "An A2A Message: a role and its parts, as a vector."
  [:map [:role [:maybe :string]] [:parts [:vector :any]]])

(def AgentMessage
  "The status message the factories build: one text part from the agent."
  [:map [:role [:= "agent"]] [:parts [:tuple [:map [:type [:= "text"]] [:text [:maybe :string]]]]]])

(def Status
  "A task's status: its state and the agent's status message."
  [:map [:state [:maybe TaskState]] [:message AgentMessage]])

(def Task
  "An A2A Task."
  [:map
   [:id :any]
   [:contextId :any]
   [:status Status]
   [:kind [:= "task"]]
   [:messages {:optional true} :any]
   [:artifacts {:optional true} :any]
   [:metadata {:optional true} :any]])

(def TaskOpts
  "The `& {:as opts}` tail of `make-task`: alternating key/value pairs or
   one trailing map. Only :messages, :artifacts and :metadata are read."
  (let [entry [:alt [:cat [:= :messages] [:maybe [:sequential Message]]]
               [:cat [:= :artifacts] [:maybe [:sequential :map]]]
               [:cat [:= :metadata] [:maybe :map]]]]
    [:alt [:cat] [:+ entry]
     [:cat [:map [:messages {:optional true} [:maybe [:sequential Message]]]
            [:artifacts {:optional true} [:maybe [:sequential :map]]]
            [:metadata {:optional true} [:maybe :map]]]]]))

(def StatusUpdateEvent
  "An A2A TaskStatusUpdateEvent."
  [:map [:id :any] [:status Status] [:kind [:= "status-update"]]])

(def ArtifactUpdateEvent
  "An A2A TaskArtifactUpdateEvent."
  [:map [:id :any] [:artifact :any] [:kind [:= "artifact-update"]]])

(def RpcSuccess
  "A JSON-RPC 2.0 success response."
  [:map [:jsonrpc [:= "2.0"]] [:id JsonRpcId] [:result :any]])

(def ErrorCode
  "A JSON-RPC error code: one of `error-codes`, or nil when a caller looked
   up a kind that has none."
  [:maybe :int])

(def RpcError
  "A JSON-RPC 2.0 error response. :data is present only when the caller
   passed trailing key/value data."
  [:map [:jsonrpc [:= "2.0"]] [:id JsonRpcId]
   [:error [:map [:code ErrorCode] [:message [:maybe :string]]
            [:data {:optional true} [:map-of :any :any]]]]])

;; =============================================================================
;; A2A Data Factories
;; =============================================================================

(defn make-message
  "Build an A2A Message.
   role: \"user\" | \"agent\"
   parts: vector of Part maps, e.g. [{:type \"text\" :text \"hello\"}]"
  [role parts]
  {:role role
   :parts (vec parts)})

(m/=> make-message [:=> [:cat [:maybe :string] [:maybe [:sequential :any]]] Message])

(defn make-task
  "Build an A2A Task object.
   id: unique task identifier
   context-id: conversation context (groups related tasks)
   state: A2A task state string
   status-msg: human-readable status message
   opts: optional keys — :messages, :artifacts, :metadata"
  [id context-id state status-msg & {:as opts}]
  (cond-> {:id id
           :contextId context-id
           :status {:state state
                    :message (make-message "agent"
                                           [{:type "text" :text status-msg}])}
           :kind "task"}
    (:messages opts)  (assoc :messages (:messages opts))
    (:artifacts opts) (assoc :artifacts (:artifacts opts))
    (:metadata opts)  (assoc :metadata (:metadata opts))))

(m/=> make-task [:=> [:cat :any :any [:maybe TaskState] [:maybe :string] TaskOpts] Task])

(defn make-status-update-event
  "Build an A2A TaskStatusUpdateEvent for SSE streaming."
  [task-id state msg]
  {:id task-id
   :status {:state state
            :message (make-message "agent"
                                   [{:type "text" :text msg}])}
   :kind "status-update"})

(m/=> make-status-update-event [:=> [:cat :any [:maybe TaskState] [:maybe :string]] StatusUpdateEvent])

(defn make-artifact-update-event
  "Build an A2A TaskArtifactUpdateEvent for SSE streaming."
  [task-id artifact]
  {:id task-id
   :artifact artifact
   :kind "artifact-update"})

(m/=> make-artifact-update-event [:=> [:cat :any :any] ArtifactUpdateEvent])

;; =============================================================================
;; JSON-RPC 2.0 Wrappers
;; =============================================================================

(defn jsonrpc-success
  "Wrap result in JSON-RPC 2.0 success response."
  [id result]
  {:jsonrpc "2.0"
   :id id
   :result result})

(m/=> jsonrpc-success [:=> [:cat JsonRpcId :any] RpcSuccess])

(defn jsonrpc-error
  "Wrap error in JSON-RPC 2.0 error response."
  [id code msg & {:as data}]
  (cond-> {:jsonrpc "2.0"
           :id id
           :error {:code code :message msg}}
    data (assoc-in [:error :data] data)))

(m/=> jsonrpc-error
      [:=> [:cat JsonRpcId ErrorCode [:maybe :string]
            [:alt [:cat] [:+ [:cat :keyword :any]] [:cat [:map-of :keyword :any]]]]
       RpcError])
