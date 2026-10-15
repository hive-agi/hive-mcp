(ns hive-mcp.agent.hooks.schema
  "Contract schemas for the budget guardrail hook. Leaf namespace.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Instant
  "A java.time.Instant."
  [:fn {:gen/elements [java.time.Instant/EPOCH]}
   (fn [x] (instance? java.time.Instant x))])

(def Usd
  "A USD amount or token count as a caller may pass it."
  [:maybe number?])

(def Budget
  "A positive budget, as register-budget! requires."
  [:or [:int {:min 1}] [:double {:min 0.01 :infinite? false :NaN? false}]])

(def BudgetEntry
  "The record kept per registered agent."
  [:map
   [:max-budget-usd :double]
   [:spent-usd :double]
   [:model :string]
   [:tool-calls :int]
   [:registered-at Instant]
   [:last-check-at [:maybe Instant]]
   [:exceeded? :boolean]
   [:denied-calls :int]])

(def BudgetStatus
  "get-budget-status row."
  [:map
   [:agent-id :string]
   [:max-budget-usd :double]
   [:spent-usd :double]
   [:remaining-usd :double]
   [:pct-used :double]
   [:tool-calls :int]
   [:denied-calls :int]
   [:model :string]
   [:exceeded? :boolean]
   [:registered-at :string]])

(def Decision
  "The guardrail verdict: allow, or deny + interrupt."
  [:or
   [:map [:action [:= :allow]]]
   [:map [:action [:= :deny]] [:message :string] [:interrupt? [:= true]]]])

(def TotalSpend
  [:map
   [:total-spend-usd :double]
   [:agent-count :int]
   [:agents [:vector [:map [:agent-id :string] [:spent-usd :double]]]]])
