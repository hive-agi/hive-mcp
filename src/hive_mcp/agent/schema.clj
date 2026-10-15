(ns hive-mcp.agent.schema
  "Value-object schemas for the hive-mcp.agent bounded context.

   One term, one schema: the contracts of the agent namespaces (`m/=>` forms
   next to each namespace) and the schema-synthesized tests share these.
   Every schema here GENERATES (`malli.generator/generate`). Handles that have
   no structural shape (an atom, a File, a backend) carry `:gen/elements`
   so a generator exists without this namespace requiring test.check.

   Contracts state the true domain. Generator sizing never lives here."
  (:require [clojure.java.io :as io]
            [hive-mcp.agent.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ---------------------------------------------------------------------------
;; Budget routing
;; ---------------------------------------------------------------------------

(def Tier
  "A model cost tier, cheapest first."
  [:enum :free :economy :standard :premium])

(def Health
  "Fleet budget health."
  [:enum :healthy :caution :critical :exhausted])

(def Usd
  "A USD amount (or any non-negative-ish number the router divides)."
  number?)

(def BudgetStatus
  "A budget status row as hooks.budget reports it. Open: only the keys the
   router reads are named, all optional (nil status = unrestricted).
   `:pct-used` is a double: `select-tier` formats it with %.0f, which throws
   on a Long."
  [:maybe [:map
           [:pct-used {:optional true} [:maybe :double]]
           [:exceeded? {:optional true} :any]
           [:remaining-usd {:optional true} [:maybe number?]]]])

(def TierSelection
  "What `select-tier` answers (open: route-model assoc's more onto it)."
  [:map
   [:tier Tier]
   [:reason :string]
   [:max-tier Tier]
   [:downgraded? :boolean]
   [:budget-status :any]])

(def RouteResult
  "What `route-model` answers, enabled or not."
  [:map
   [:model :any]
   [:tier Tier]
   [:reason :string]
   [:downgraded? :boolean]])

(def FleetStatus
  "The fleet roll-up inside a `suggest-model` answer."
  [:map
   [:spent number?]
   [:budget number?]
   [:remaining number?]
   [:pct-used number?]
   [:agents number?]])

(def ModelSuggestion
  "What `suggest-model` answers."
  [:map
   [:tier Tier]
   [:model :any]
   [:reason :string]
   [:max-tier Tier]
   [:projected-tasks :int]
   [:per-agent-share number?]
   [:fleet-status FleetStatus]])

(def BudgetRecommendation
  "What `recommend-budget` answers."
  [:map
   [:recommended-budget-usd number?]
   [:fleet-remaining-usd number?]
   [:fleet-spent-usd number?]
   [:fleet-budget-usd number?]
   [:planned-agents number?]
   [:tier-at-budget Tier]])

(def FleetSummary
  "What `fleet-budget-summary` answers."
  [:map
   [:fleet-budget-usd number?]
   [:fleet-spent-usd number?]
   [:fleet-remaining-usd number?]
   [:fleet-pct-used number?]
   [:agent-count :int]
   [:agents [:vector :map]]
   [:health Health]])

;; ---------------------------------------------------------------------------
;; ADT values (hive-dsl.adt/defadt)
;; ---------------------------------------------------------------------------

(def CapabilityKw
  "The closed set of headless capability variants."
  [:enum :cap/hooks :cap/interrupts :cap/subagents :cap/checkpointing
   :cap/mcp-tools :cap/streaming :cap/multi-turn :cap/budget-guard :cap/saa])

(def HeadlessCapability
  "A HeadlessCapability ADT value."
  [:map
   [:adt/type [:= :HeadlessCapability]]
   [:adt/variant CapabilityKw]])

(def RoleKw
  "The closed set of hivemind role variants."
  [:enum :role/hivemind :role/worker :role/standalone])

(def HivemindRole
  "A HivemindRole ADT value."
  [:map
   [:adt/type [:= :HivemindRole]]
   [:adt/variant RoleKw]])

;; ---------------------------------------------------------------------------
;; Error summary
;; ---------------------------------------------------------------------------

(def Frame
  "One stack frame of an error summary."
  [:map
   [:class :string]
   [:method :string]
   [:file [:maybe :string]]
   [:line :int]])

(def ErrorSummary
  "A bounded, structured summary of an error payload (see error-summary)."
  [:schema {:registry {::summary
                       [:map
                        [:class :string]
                        [:message :string]
                        [:frames [:vector Frame]]
                        [:error-type {:optional true} :any]
                        [:cause {:optional true} [:ref ::summary]]]}}
   ::summary])

;; ---------------------------------------------------------------------------
;; Handles with no structural shape
;; ---------------------------------------------------------------------------

(def RingBuffer
  "The atom `ring-buffer/create-ring-buffer` returns."
  [:fn {:gen/elements [(atom {:lines [] :timestamps [] :capacity 5000
                              :total-lines-seen 0 :dropped 0})]}
   (fn [x] (instance? clojure.lang.IAtom x))])

(def File
  "A java.io.File."
  [:fn {:gen/elements [(io/file "hive-mcp-agent-schema-file")]}
   (fn [x] (instance? java.io.File x))])

(def Backend
  "Anything implementing the LLMBackend protocol."
  [:fn {:gen/elements [(reify proto/LLMBackend
                         (chat [_ _ _] {:type :text :content ""})
                         (model-name [_] "schema/backend"))]}
   (fn [x] (satisfies? proto/LLMBackend x))])

;; ---------------------------------------------------------------------------
;; Signer / credential
;; ---------------------------------------------------------------------------

(def SigningKey
  "{:kid :private :public} as `identity/load-or-create-key!` returns it."
  [:map
   [:kid :string]
   [:private bytes?]
   [:public bytes?]])

(def Signer
  "The signer port: a kid and sign/verify fns."
  [:map
   [:kid :string]
   [:sign ifn?]
   [:verify ifn?]])

(def Claimant
  "What the registry says a caller is."
  [:enum :coordinator :spawned :unknown])

(def Mode
  "Identity policy mode."
  [:enum :observe :enforce])

;; ---------------------------------------------------------------------------
;; Tool execution
;; ---------------------------------------------------------------------------

(def ToolResult
  "What `executor/execute-tool` answers."
  [:map
   [:success :boolean]
   [:result {:optional true} :any]
   [:error {:optional true} :any]])

(def ToolMessage
  "A role:tool message for conversation history."
  [:map
   [:role [:= "tool"]]
   [:tool_call_id :any]
   [:name :any]
   [:content :string]])

(def Permissions
  "Permission flags a tool call runs with (a coll, `(set perms)` is taken)."
  [:maybe [:or [:sequential :any] [:set :any]]])

(def GetSlave
  "The registry port: a callable id -> row or nil (a fn, map or keyword all work)."
  [:fn {:gen/elements [(constantly nil)]} ifn?])

(def Domain
  "The soft-resolved domain fns of the hive-agent addon (or nil when absent)."
  [:maybe [:map-of :keyword [:fn {:gen/elements [(constantly nil)]} ifn?]]])

(def RequestedGrant
  "What `grant/requested-grant` answers: the parsed map, or why it cannot be one."
  [:or
   [:map [:ok [:maybe :map]]]
   [:map [:refused :string]]])

(def ChildGrantDecision
  "What `grant/child-grant` answers: a recorded grant, or a refusal."
  [:or
   [:map [:grant :any]]
   [:map [:refused :string] [:data {:optional true} :map]]])

(def ResolveResult
  "What `identity/resolve-request` answers."
  [:map
   [:args :map]
   [:identity [:map
               [:caller-id :any]
               [:claimant Claimant]
               [:verified? :boolean]
               [:claims {:optional true} :any]]]
   [:decision :map]])
