(ns hive-mcp.workflows.schema
  "Value-object schemas for the workflow FSM subsystem (hive-mcp.workflows.*).

   One term, one schema: the FSM data map, the resources map, the terminal
   handler's fsm argument, a compiled FSM and the trace-log entry are shared by
   every workflow namespace (saa, wrap, complete, catchup, forge-belt), so they
   live here rather than being restated per namespace.

   Every schema generates (malli.generator/generate succeeds); opaque
   predicates carry a :gen/* property.

   LAYERING: depends only on malli. Workflow namespaces require this one, never
   the reverse.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Fn
  "Anything invocable: a fn, a var, a keyword, a set or a map. Generates identity."
  [:fn {:gen/elements [identity] :error/message "must be ifn?"} ifn?])

(def Data
  "The FSM state data map a normal handler reads and returns. Handlers and
   dispatch predicates are called with nil from tests, so nil is admitted."
  [:maybe :map])

(def Resources
  "The resources map (side-effect fns, config) injected at run time. Handlers
   treat a missing resources map like an empty one."
  [:maybe :map])

(def Truthy
  "A generalized boolean: dispatch predicates written as (and ...) / (seq ...)
   answer the last truthy operand, not a Boolean. Only truthiness is promised."
  :any)

(def Fsm
  "The fsm value a terminal handler (:end / :error / :halt) receives."
  [:maybe
   [:map
    [:data {:optional true} Data]
    [:error {:optional true} :any]
    [:current-state-id {:optional true} :any]]])

(def TraceEntry
  "One :trace-log entry appended by the :pre / :post interceptors."
  [:map
   [:state :any]
   [:at :string]
   [:direction [:enum :enter :exit]]])

(def TracedFsm
  "An fsm after a trace interceptor: its :data carries a :trace-log vector."
  [:map [:data [:map [:trace-log [:vector TraceEntry]]]]])

(def CompiledFsm
  "What hive.events.fsm/compile answers: the resolved :fsm graph and :opts."
  [:map
   [:fsm [:map-of :keyword :map]]
   [:opts {:optional true} [:maybe :map]]])

(def RunResult
  "What fsm/run answers: the terminal handler's value. The built-in :end
   handlers answer maps; an injected build/error fn may answer anything."
  :any)

(def Eviction
  "The :eviction value handle-evict assocs."
  [:map
   [:evicted {:optional true} :any]
   [:skipped {:optional true} :boolean]
   [:degraded {:optional true} :boolean]
   [:error {:optional true} [:maybe :string]]])
