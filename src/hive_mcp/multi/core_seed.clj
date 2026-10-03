(ns hive-mcp.multi.core-seed
  "Project the existing 21 consolidated tools + 36 DSL verbs + 9 param aliases
   into multi.registry as the synthetic `:multi/core` owner.

   Seeds on every load of this namespace, so the registry is populated before
   any addon `(hooks [this])` walk arrives. Registering under :multi/core is an
   idempotent same-owner replace, so each load leaves the :multi/core entries
   this code builds.

   This decouples multi.handler from consolidated.multi at the type level
   (DIP) — the handler dispatches through the registry, not the literal map.

   External addons can never deregister `:multi/core` entries because
   `deregister-by-owner!` is invoked only with the addon's own id.

   Decision: 20260429230453-7e7627cc"
  (:require [hive-mcp.multi.registry :as registry]
            [hive-mcp.multi.batchables :as bx]
            [hive-mcp.dsl.verbs :as dsl-verbs]
            [taoensso.timbre :as log]
            [hive-mcp.tools.consolidated.roster :as roster]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private core-owner :multi/core)

(defn- seed-tools!
  "Seed registry.tools by enumerating consolidated tool symbols from
   `tools.consolidated.roster` (single source of truth).

   Each handler symbol is `requiring-resolve`d at boot — the consolidated.X
   namespace loads on demand. This breaks the static-require chain that
   previously coupled `consolidated.multi` to every consolidated.X namespace
   (DIP). Adding a new consolidated tool is a single row added to
   `hive-mcp.tools.consolidated.roster/consolidated-tools`.

   Composite tools (analysis) have no per-tool ns; their handler is built
   on demand via `composite/build-composite-handler` keyed on tool-name."
  []
  (let [composite-builder (try (requiring-resolve 'hive-mcp.tools.composite/build-composite-handler)
                               (catch Throwable _ nil))
        seeded-leaf
        (reduce
         (fn [acc [tool-name sym]]
           (if-let [v (try (requiring-resolve sym) (catch Throwable t
                                                     (log/warn "[multi.core-seed] requiring-resolve failed"
                                                               {:tool tool-name :sym sym
                                                                :err (.getMessage t)})
                                                     nil))]
             (do (registry/register-by-key! core-owner :multi/tool
                                            [{:tool-name tool-name :handler @v}])
                 (inc acc))
             acc))
         0
         roster/consolidated-tools)
        seeded-composite
        (if-not composite-builder
          (do (log/warn "[multi.core-seed] composite/build-composite-handler not resolvable — skipping composite seed")
              0)
          (reduce
           (fn [acc tool-name]
             (registry/register-by-key! core-owner :multi/tool
                                        [{:tool-name tool-name
                                          :handler (composite-builder tool-name)}])
             (inc acc))
           0
           roster/composite-tools))]
    (+ seeded-leaf seeded-composite)))

(defn- seed-verbs!
  "Seed registry.verbs from hive-mcp.dsl.verbs/verb-table."
  []
  (let [table dsl-verbs/verb-table]
    (doseq [[code {:keys [tool command]}] table]
      (registry/register-by-key! core-owner :multi/verb
                                 [{:code code :tool tool :command command}]))
    (count table)))

(defn- seed-aliases!
  "Seed registry.aliases from hive-mcp.dsl.verbs/param-aliases."
  []
  (let [aliases dsl-verbs/param-aliases]
    (doseq [[short full] aliases]
      (registry/register-by-key! core-owner :multi/param-alias
                                 [{:short short :full full}]))
    (count aliases)))

(defn- seed-batchables!
  "Seed registry.batchables with the explicit Batchable records for the three
   highest-leverage core tools: memory, kg, kanban.

   Each record wraps the existing handle-batch-X handlers so multi's per-op
   loop collapses into ONE store round-trip per op-class per wave instead of
   the N round-trips today's tools/cli/make-batch-handler path produces.

   Tools without an explicit Batchable still resolve to the LSP-clean
   DefaultBatchableAdapter via registry/lookup-batchable-or-default."
  []
  (let [entries [{:tool-name "memory" :record (bx/memory-batchable)}
                 {:tool-name "kg"     :record (bx/kg-batchable)}
                 {:tool-name "kanban" :record (bx/kanban-batchable)}]]
    (doseq [entry entries]
      (registry/register-by-key! core-owner :multi/batchable [entry]))
    (count entries)))

(defn- seed!
  "Register every :multi/core seed. Returns the count registered per child registry."
  []
  {:tools      (seed-tools!)
   :verbs      (seed-verbs!)
   :aliases    (seed-aliases!)
   :batchables (seed-batchables!)})

(def installed
  "Counts registered per child registry by the latest load of this namespace.
   Evaluating it seeds :multi/core, so every load re-registers; registering
   under :multi/core is an idempotent same-owner replace."
  (let [result (seed!)]
    (log/info "[multi.core-seed] seeded :multi/core owner" result)
    result))

(defn install!
  "Deregister every :multi/core entry, then seed again. Returns the count
   registered per child registry. The registry bootstrap calls it, so the
   seed converges even when a refresh recreates the registry while this
   namespace stays loaded."
  []
  (registry/deregister-by-owner! core-owner)
  (let [result (seed!)]
    (log/info "[multi.core-seed] re-seeded :multi/core owner" result)
    result))
