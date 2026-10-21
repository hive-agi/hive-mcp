(ns hive-mcp.router.resolve
  "L1 pure — `(config, memory-type) → Result<ProviderSpec>`.

   Zero I/O. Zero global state. Takes the resolved `:embedder` config
   map verbatim and the memory-type keyword/string; returns a
   `hive-dsl.result/Result` of `ProviderSpec`.

   This is the canonical type→spec resolver; the L2 `IRouter` impl
   (`router/default.clj`) wraps it.

   Resolution order (highest to lowest precedence):
     1. `[:routes <:type/X>]`                  — explicit kw-namespaced route
     2. `[:routes <:X>]`                       — bare-keyword route (legacy)
     3. `[:default]`                           — fallback
     4. err `:router/no-default`               — nothing usable

   Once the provider key is known, `[:providers <key>]` produces the
   `ProviderSpec` via `embedder.spec/make`. Unknown provider keys
   surface as `:router/unknown-provider`."
  (:require [hive-dsl.result :as r]
            [hive-mcp.embedder.spec :as spec]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- type->keys
  "Sequence of route-lookup keys to try, in precedence order, for a
   given memory-type input. Accepts keyword (`:type/note`, `:note`),
   string (`\"note\"`, `\"type/note\"`), or nil. Returns up to two
   keywords."
  [memory-type]
  (let [bare (cond
               (keyword? memory-type) (name memory-type)
               (string? memory-type)  (let [s memory-type]
                                        (if-let [slash (some-> s (.indexOf "/") (#(when (>= % 0) %)))]
                                          (subs s (inc slash))
                                          s))
               :else                   nil)]
    (when bare
      [(keyword "type" bare)
       (keyword bare)])))

;;; ---------------------------------------------------------------------------
;;; Schemas
;;; ---------------------------------------------------------------------------

(def ProviderEntry
  "One `[:providers <key>]` entry of the `:embedder` block. Every key is
   optional here: `embedder.spec/make` is the validator, and a malformed
   entry surfaces as `:router/invalid-provider-spec`."
  [:map
   [:impl {:optional true} :keyword]
   [:model {:optional true} :string]
   [:dimension {:optional true} :int]
   [:max-tokens {:optional true} :int]])

(def EmbedderConfig
  "The resolved `:embedder` config block the resolvers read verbatim.
   Open map; nil reads as an empty block (every lookup misses)."
  [:maybe
   [:map
    [:default {:optional true} [:maybe :keyword]]
    [:routes {:optional true} [:maybe [:map-of :keyword :keyword]]]
    [:providers {:optional true} [:maybe [:map-of :keyword ProviderEntry]]]
    [:escalation {:optional true} [:maybe [:map [:heavy-tier-key {:optional true} :keyword]]]]]])

(def RouterError
  "The err variants the resolvers name (see the ns docstring)."
  [:enum :router/no-default :router/unknown-provider :router/invalid-provider-spec])

(def SpecResult
  "`Result<ProviderSpec>` — `{:ok ProviderSpec}` or `{:error RouterError ...}`."
  [:or
   [:map [:ok spec/schema]]
   [:map [:error RouterError]]])

(defn resolve-provider-key
  "Pure — return the provider key for `memory-type` per the resolution
   order in this ns docstring. Returns a keyword or nil."
  [config memory-type]
  (let [routes (:routes config)]
    (or (some #(get routes %) (type->keys memory-type))
        (:default config))))

;; memory-type: the body accepts any value (a non keyword/string resolves via
;; :default only)
(m/=> resolve-provider-key [:=> [:cat EmbedderConfig :any] [:maybe :keyword]])

(defn resolve-spec
  "Pure — return `(ok ProviderSpec)` for `memory-type`, or an err
   variant naming the failure mode. Validates the spec via
   `embedder.spec/make` so a malformed `:providers` entry surfaces
   here rather than at the embed call site."
  [config memory-type]
  (if-let [provider-key (resolve-provider-key config memory-type)]
    (if-let [provider (get-in config [:providers provider-key])]
      (try
        (r/ok (spec/make
                {:provider/key        provider-key
                 :provider/impl       (:impl provider)
                 :provider/model      (:model provider)
                 :provider/dim        (:dimension provider)
                 :provider/max-tokens (:max-tokens provider)}))
        (catch Exception e
          (r/err :router/invalid-provider-spec
                 {:provider-key provider-key
                  :provider     provider
                  :cause        (.getMessage e)})))
      (r/err :router/unknown-provider
             {:provider-key provider-key
              :memory-type  memory-type
              :known        (-> config :providers keys vec)}))
    (r/err :router/no-default
           {:memory-type memory-type
            :hint        "Set [:embedder :default <provider-key>] or [:embedder :routes <type>]"})))

(m/=> resolve-spec [:=> [:cat EmbedderConfig :any] SpecResult])
