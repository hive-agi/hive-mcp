(ns hive-mcp.addons.schema
  "Value-object schemas for the addon bounded context (`hive-mcp.addons.*`).

   Every schema here GENERATES (`malli.generator/generate`): hive-schemas turns
   the contracts that reference them into property generators, oracles and
   mutants, so the shapes below are the single source for the addons tests.
   Where a shape is a port (an `IAddon` instance) the generator is a `:gen/*`
   property on the schema, never a hand-written test.check generator.

   Manifest shapes (`Manifest`, `AddonType`, ...) stay in
   `hive-mcp.addons.manifest`, tool-claim shapes in
   `hive-mcp.addons.tool-claims`; this namespace owns the registry-side ones."
  (:require [hive-addon.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; The IAddon port
;; =============================================================================

(def stub-addon
  "A minimal `IAddon` instance: the generator seed for `IAddonInstance`."
  (reify proto/IAddon
    (addon-id [_] :schema/stub)
    (addon-type [_] :native)
    (capabilities [_] #{})
    (initialize! [_ _] {:success? true :errors [] :metadata {}})
    (shutdown! [_] {:success? true :errors []})
    (tools [_] [])
    (schema-extensions [_] {})
    (health [_] {:status :ok})))

(def IAddonInstance
  "Anything satisfying `hive-addon.protocol/IAddon`."
  [:fn {:gen/elements [stub-addon]
        :error/message "must satisfy hive-addon.protocol/IAddon"}
   #(satisfies? proto/IAddon %)])

;; =============================================================================
;; Registry vocabulary
;; =============================================================================

(def AddonCandidate
  "What `register-addon!` accepts: ANYTHING, generated as a real addon. The
   `:pre` in `register-addon!` is the gate that rejects a non-addon, and the
   existing suite drives that rejection, so the contract stays permissive in
   (an `IAddonInstance` input would throw at the instrumentation boundary
   before the `:pre` ever saw the value)."
  [:any {:gen/elements [stub-addon]}])

(def RegistryId
  "The key an addon is registered under: the keyword its `addon-id` answers, or
   the string id a manifest carries."
  [:or :keyword [:string {:min 1 :max 24}]])

(def AddonState
  "Lifecycle state of a registry entry."
  [:enum :registered :active :error])

(def Capability
  "A capability keyword an addon declares."
  :keyword)

(def Instant
  "A `java.time.Instant`."
  [:fn {:gen/elements [(java.time.Instant/parse "2026-10-14T00:00:00Z")]}
   #(instance? java.time.Instant %)])

(def AddonEntry
  "One registry entry: the addon plus its lifecycle metadata."
  [:map
   [:addon IAddonInstance]
   [:state AddonState]
   [:registered-at Instant]
   [:reg-seq [:int {:min 1}]]
   [:init-time [:maybe Instant]]
   [:init-result [:maybe :map]]
   [:hook-keys {:optional true} [:set :keyword]]])

(def AddonListing
  "One row of `list-addons`."
  [:map
   [:name RegistryId]
   [:type [:maybe :keyword]]
   [:state AddonState]
   [:registered-at Instant]
   [:init-time [:maybe Instant]]
   [:capabilities [:set Capability]]])

(def AddonOutcome
  "What a lifecycle operation (register / init / shutdown / unregister)
   answers. An addon's own `initialize!` / `shutdown!` result is spliced in, so
   the map is open: only the success flag and the addon name are promised."
  [:map
   [:success? :boolean]
   [:addon-name RegistryId]
   [:errors {:optional true} [:sequential :string]]
   [:already-active? {:optional true} :boolean]
   [:already-inactive? {:optional true} :boolean]
   [:shutdown-errors {:optional true} [:sequential :string]]
   [:elapsed-ms {:optional true} number?]])

(def RegistryStatus
  "`registry-status`: counts by state, tool count, capability index, listing."
  [:map
   [:total :int]
   [:active :int]
   [:registered :int]
   [:error :int]
   [:tool-count :int]
   [:capabilities [:map-of Capability [:vector RegistryId]]]
   [:addons [:vector AddonListing]]])

(def Dependencies
  "`check-dependencies` / `di/check-service-dependencies` answer."
  [:map
   [:satisfied? :boolean]
   [:missing [:set :any]]
   [:available [:set :any]]])

(def InstalledTool
  "An addon tool as installed: its definition tagged with the owning addon."
  [:map [:name :string] [:addon-source RegistryId]])

;; =============================================================================
;; Boot health
;; =============================================================================

(def Issue
  "One boot-health finding."
  [:map
   [:issue [:enum :memory-store-missing :below-floor :expected-missing
            :baseline-drop :mount-failed]]
   [:severity [:enum :error :warn]]
   [:message :string]])

(def BootFacts
  "The facts `boot-health/assess` judges."
  [:map
   [:discovered {:optional true} [:vector [:string {:min 1 :max 12}]]]
   [:mounted {:optional true} [:vector [:string {:min 1 :max 12}]]]
   [:failed {:optional true} [:vector [:string {:min 1 :max 12}]]]
   [:expected-min {:optional true} [:maybe [:int {:min 0 :max 20}]]]
   [:expected {:optional true} [:set [:string {:min 1 :max 12}]]]
   [:baseline {:optional true} [:maybe [:map [:discovered {:optional true}
                                             [:vector [:string {:min 1 :max 12}]]]
                                    [:at {:optional true} :string]]]]
   [:memory {:optional true} [:maybe [:map
                                      [:backend {:optional true} [:maybe :string]]
                                      [:deferred-to {:optional true} [:maybe :string]]
                                      [:store-set? {:optional true} [:maybe :boolean]]]]]])

(def Snapshot
  "A recorded boot-health snapshot: the facts plus the judgement."
  [:map
   [:issues [:vector Issue]]
   [:degraded? :boolean]
   [:expected-count [:maybe :int]]])
