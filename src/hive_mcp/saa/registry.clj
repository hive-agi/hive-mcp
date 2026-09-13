(ns hive-mcp.saa.registry
  "Façade over the six child SAA registries
   (phase-providers / scorers / planners / tool-intents / plan-stores /
   dispatch-modes).

   Single SOLID-clean dispatch surface for SAA addon contributions: any
   `(hooks [this])` map entry whose key namespace is \"saa\" routes here via
   `register-by-key!` / `deregister-by-owner!`. The accepted values are the
   `hive-mcp.saa.types/SaaRegistryEntry` variants.

   Owner = addon-id keyword (or :saa/core for boot-seeded entries).
   Per-owner ownership tagging means `deregister-by-owner!` is O(owner-keys)
   and never clobbers another addon's entries.

   Protocol resolvers always return a satisfying record (LSP): the boot-seeded
   :saa/default entries back every lookup so the caller cannot tell a default
   from an addon contribution. The two Act port resolvers answer nil when
   nothing fills the port: `lookup-plan-store` (no store seeded) and
   `lookup-dispatch-mode` (unknown mode)."
  (:require [hive-mcp.saa.types :as types :refer [SaaRegistryEntry]]
            [hive-mcp.saa.registry.phase-providers :as r-providers]
            [hive-mcp.saa.registry.scorers :as r-scorers]
            [hive-mcp.saa.registry.planners :as r-planners]
            [hive-mcp.saa.registry.tool-intents :as r-intents]
            [hive-mcp.saa.registry.plan-stores :as r-stores]
            [hive-mcp.saa.registry.dispatch-modes :as r-dispatch]
            [hive-mcp.saa.adapters :as adapters]
            [hive-mcp.saa.scorer :as scorer]
            [hive-mcp.saa.planner :as planner]
            [hive-dsl.adt :refer [adt-case]]
            [hive-dsl.result :refer [rescue]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private default-id :saa/default)

;; =============================================================================
;; Hook-key dispatch — validated via adt-case on SaaRegistryEntry
;; =============================================================================

(defn- register-entry!
  "Route one validated SaaRegistryEntry to its child registry."
  [owner entry]
  (adt-case SaaRegistryEntry entry
    :saa/phase-provider (r-providers/register! owner default-id {:provider (:provider entry)})
    :saa/scorer         (r-scorers/register!   owner default-id {:scorer (:scorer entry)})
    :saa/planner        (r-planners/register!  owner default-id {:planner (:planner entry)})
    :saa/tool-intent    (r-intents/register!   owner (:intent entry) {:tools (:tools entry)})
    :saa/plan-store     (r-stores/register!    owner default-id {:store (:store entry)})
    :saa/dispatch-mode  (r-dispatch/register!  owner (:mode entry) {:dispatch (:dispatch entry)})))

(defn register-by-key!
  "Route SAA addon `(hooks)` entries to the right child registry.

   `entries` is a vector of SaaRegistryEntry ADT values (per saa.types).
   Each entry is validated via `adt-case` exhaustiveness. Owner stamp is the
   addon-id passed by the lifecycle.

   `k` is accepted for hook-walk symmetry; routing is by the entry's variant.

   Returns a vector of per-entry outcomes (`:ok` | `:replaced` | `:conflict`)."
  [owner k entries]
  (let [entries (cond (sequential? entries) entries
                      (map? entries)        [entries]
                      :else                  nil)]
    (when (nil? entries)
      (log/warn "[saa.registry] non-vector value for hook key — skipping"
                {:owner owner :key k}))
    (mapv (fn [entry]
            (if (types/saa-registry-entry? entry)
              (register-entry! owner entry)
              (do (log/warn "[saa.registry] non-SaaRegistryEntry value — ignored"
                            {:owner owner :key k :entry entry})
                  :ignored)))
          (or entries []))))

(defn deregister-by-key!
  "Remove every entry an addon registered under a SAA `k`.

   Provided for symmetry with register-by-key!. Calling it is equivalent to
   `deregister-by-owner!` scoped to the relevant child registry."
  [owner k]
  (case k
    :saa/phase-provider (r-providers/deregister-by-owner! owner)
    :saa/scorer         (r-scorers/deregister-by-owner! owner)
    :saa/planner        (r-planners/deregister-by-owner! owner)
    :saa/tool-intent    (r-intents/deregister-by-owner! owner)
    :saa/plan-store     (r-stores/deregister-by-owner! owner)
    :saa/dispatch-mode  (r-dispatch/deregister-by-owner! owner)
    nil))

(defn deregister-by-owner!
  "Clear every entry across all six child registries owned by `owner`."
  [owner]
  {:providers      (r-providers/deregister-by-owner! owner)
   :scorers        (r-scorers/deregister-by-owner! owner)
   :planners       (r-planners/deregister-by-owner! owner)
   :tool-intents   (r-intents/deregister-by-owner! owner)
   :plan-stores    (r-stores/deregister-by-owner! owner)
   :dispatch-modes (r-dispatch/deregister-by-owner! owner)})

;; =============================================================================
;; Resolvers — ALWAYS return a satisfying record (LSP)
;; =============================================================================

(defn lookup-phase-provider-or-default
  "Return the IPhaseProvider for `provider-id`, the :saa/default entry, or a
   freshly-constructed DefaultPhaseProvider. Never nil (LSP) — independent of
   whether the core-seed has run."
  ([] (lookup-phase-provider-or-default default-id))
  ([provider-id]
   (or (some-> (r-providers/lookup provider-id) :provider)
       (some-> (r-providers/lookup default-id) :provider)
       (adapters/->default-phase-provider nil))))

(defn lookup-scorer-or-default
  "Return the IObservationScorer for `scorer-id`, the :saa/default entry, or a
   freshly-constructed DefaultObservationScorer. Never nil (LSP) — seed-independent."
  ([] (lookup-scorer-or-default default-id))
  ([scorer-id]
   (or (some-> (r-scorers/lookup scorer-id) :scorer)
       (some-> (r-scorers/lookup default-id) :scorer)
       (scorer/->default-scorer))))

(defn lookup-planner-or-default
  "Return the IPlanSynthesizer for `planner-id`, the :saa/default entry, or a
   freshly-constructed NoopPlanSynthesizer. Never nil (LSP) — seed-independent."
  ([] (lookup-planner-or-default default-id))
  ([planner-id]
   (or (some-> (r-planners/lookup planner-id) :planner)
       (some-> (r-planners/lookup default-id) :planner)
       (planner/->noop-planner))))

(defn resolve-tools
  "Resolve the concrete tool set for `provider-id` + neutral `capability`.

   PROVIDER-SCOPED: unions the provider's owner slice with the :saa/core
   neutral fallback. Returns a sorted vector of tools."
  [provider-id capability]
  (let [core-slice     (r-intents/lookup-owner-slice :saa/core capability)
        provider-slice (r-intents/lookup-owner-slice provider-id capability)]
    (vec (sort (into (set core-slice) provider-slice)))))

(defn lookup-plan-store
  "Return the registered plan-store fn (fn [plan agent-id directory]), or nil
   when no owner fills the :saa/plan-store port. The kernel seeds none."
  ([] (lookup-plan-store default-id))
  ([store-id]
   (some-> (r-stores/lookup store-id) :store)))

(defn lookup-dispatch-mode
  "Return the dispatch fn (fn [plan agent-id ctx]) registered for `mode`, or nil
   when no owner contributed that mode."
  [mode]
  (some-> (r-dispatch/lookup mode) :dispatch))

;; =============================================================================
;; Snapshot — pure value across all four child registries
;; =============================================================================

(defn snapshot
  "Immutable snapshot across all six child SAA registries.

   `:version` is a hash callers can stamp onto compiled plans."
  []
  (let [providers (r-providers/snapshot)
        scorers   (r-scorers/snapshot)
        planners  (r-planners/snapshot)
        intents   (r-intents/snapshot)
        stores    (r-stores/snapshot)
        dispatch  (r-dispatch/snapshot)]
    {:providers providers :scorers scorers :planners planners :tool-intents intents
     :plan-stores stores :dispatch-modes dispatch
     :version (hash [(:version providers) (:version scorers)
                     (:version planners) (:version intents)
                     (:version stores) (:version dispatch)])}))

(defn reset-for-test!
  "Clear all six child registries. Test-only."
  []
  (r-providers/reset-for-test!)
  (r-scorers/reset-for-test!)
  (r-planners/reset-for-test!)
  (r-intents/reset-for-test!)
  (r-stores/reset-for-test!)
  (r-dispatch/reset-for-test!))

(defn restore!
  "Put every child registry back to the state captured by a prior `snapshot`.
   Test-only: fixtures snapshot before a test and restore after it."
  [snap]
  (r-providers/restore! (:providers snap))
  (r-scorers/restore! (:scorers snap))
  (r-planners/restore! (:planners snap))
  (r-intents/restore! (:tool-intents snap))
  (r-stores/restore! (:plan-stores snap))
  (r-dispatch/restore! (:dispatch-modes snap)))

;; =============================================================================
;; Bootstrap: seed :saa/core owner before first registration arrives
;; =============================================================================
;;
;; Lazy `require` (not top-level :require) breaks the load-order cycle:
;;   core-seed -> registry  (top-level)
;;   registry -> core-seed  (deferred until registry is fully loaded)

(defonce ^:private __saa-core-seeded__
  (rescue
   {:status :failed :reason "core-seed load threw — :saa/core entries absent"}
   (require 'hive-mcp.saa.core-seed)
   {:status :ok}))
