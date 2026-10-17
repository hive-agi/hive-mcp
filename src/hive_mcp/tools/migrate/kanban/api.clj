(ns hive-mcp.tools.migrate.kanban.api
  "Public boundary for the kanban migrator. Wires concrete adapters into
   a `deps` map and exposes a thin façade that callers (REPL users, MCP
   tool handler, scheduled jobs) consume.

   Default wiring:
     :source-lister  → milvus :default
     :source-reader  → milvus :default
     :target-reader  → registry store :kanban, through its port
     :writer         → registry store :kanban, through its port
     :state          → file at hive-mcp.tools.migrate.kanban.state/default-path

   Override any slot to swap stores (test stubs, alternate backends)."
  (:refer-clojure :exclude [run! reset!])
  (:require [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.migrate.kanban.events :as mig-events]
            [hive-mcp.tools.migrate.kanban.milvus :as mig-milvus]
            [hive-mcp.tools.migrate.kanban.state :as mig-state]
            [hive-mcp.tools.migrate.kanban.usecases :as uc]
            [hive-mcp.tools.migrate.kanban.store :as mig-store]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn make-deps
  "Build the dependency map. Pass overrides as kwargs to swap any slot. A
   store is looked up in the registry only for a slot left to its default.

   The target store is the DECORATED registry store (the seal layers), so
   both target slots go through its port (`mig-store`): the :writer's
   `add-entry!` seals what it writes, and the :target-reader's
   `get-entries`/`get-entry` answer with content opened. Never read the raw
   base store here: its ciphertext would classify as an empty card and be
   overwritten.

   The :target-probe answers which ids the target holds at all, from the
   backend's id listing (`scan-ids`) only, never its content: an entry the
   seal's visibility layer hides reads as absent, and must not be written
   over.

   The :target-durability gate refuses a run (or a step) while the target
   store does not report itself connected: a disconnected store may answer
   add-entry! from an in-memory fallback, which is no migration."
  [& {:keys [source-store target-store state-path
             source-lister source-reader target-reader target-probe
             target-durability writer state]}]
  (mig-events/init!)
  (let [src-store (delay (or source-store (proto/get-store)))
        tgt-store (delay (or target-store (proto/get-store :kanban)))]
    {:source-lister     (or source-lister (mig-milvus/make-id-lister @src-store))
     :source-reader     (or source-reader (mig-milvus/make-entry-reader @src-store))
     :target-reader     (or target-reader (mig-store/make-entry-reader @tgt-store))
     :target-probe      (or target-probe (mig-store/make-id-probe @tgt-store))
     :target-durability (or target-durability (mig-store/make-durability @tgt-store))
     :writer            (or writer (mig-store/make-entry-writer @tgt-store))
     :state             (or state (mig-state/make (or state-path
                                                       mig-state/default-path)))}))

;; =============================================================================
;; Façade — each delegates to the use case with the wired deps
;; =============================================================================

(defn init-ids!
  "Phase A: list source ids and persist into state."
  ([] (init-ids! (make-deps)))
  ([deps] (uc/init-ids! deps)))

(defn step!
  "One migration batch."
  ([opts] (step! (make-deps) opts))
  ([deps opts] (uc/step! deps opts)))

(defn run!
  "Loop step! until done, until the target only queues a write, or until
   `:max-steps` is reached."
  ([opts] (run! (make-deps) opts))
  ([deps opts] (uc/run! deps opts)))

(defn status
  "Read-only progress snapshot."
  ([] (status (make-deps)))
  ([deps] (uc/status deps)))

(defn reset!
  "Wipe the state file and start over. Phase A must rerun afterwards."
  ([] (reset! (make-deps)))
  ([{:keys [state] :as _deps}]
   ((requiring-resolve 'hive-mcp.tools.migrate.kanban.ports/reset-state!) state)))
