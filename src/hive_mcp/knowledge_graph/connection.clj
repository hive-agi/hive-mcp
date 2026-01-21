(ns hive-mcp.knowledge-graph.connection
  "DataScript connection management for Knowledge Graph.

   Provides:
   - Separate connection from swarm state (different concerns)
   - Connection lifecycle (create, get, reset)
   - Shared helper functions (now, gen-edge-id)

   Architecture rationale:
   - KG is a dedicated DataScript DB, separate from swarm coordination
   - Enables independent schema evolution
   - Supports future persistence/serialization (Phase 2)

   SOLID-S: Single Responsibility - connection lifecycle only.
   SOLID-D: Dependency Inversion - other KG modules depend on this abstraction."
  (:require [datascript.core :as d]
            [taoensso.timbre :as log]
            [hive-mcp.knowledge-graph.schema :as schema]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Connection Management (Thread-Safe Atom)
;;; =============================================================================

;; Global DataScript connection atom for Knowledge Graph.
;; Separate from swarm connection - different domain concerns.
;; Thread-safe via DataScript's internal atom.
(defonce ^:private conn (atom nil))

(defn create-conn
  "Create a new DataScript connection with KG schema.
   Returns the connection (atom wrapper around db)."
  []
  (d/create-conn schema/kg-schema))

(defn get-conn
  "Get the global KG connection, creating if needed."
  []
  (or @conn
      (do
        (reset! conn (create-conn))
        (log/info "Created Knowledge Graph DataScript connection")
        @conn)))

(defn reset-conn!
  "Reset the global KG connection to empty state.

   CAUTION: Destroys all edge data. Use for testing only."
  []
  (reset! conn (create-conn))
  (log/debug "Knowledge Graph DataScript connection reset"))

(defn ensure-conn
  "Ensure connection exists, return it."
  []
  (get-conn))

;;; =============================================================================
;;; Helper Functions
;;; =============================================================================

(defn now
  "Current timestamp as java.util.Date."
  []
  (java.util.Date.))

(defn gen-edge-id
  "Generate a unique edge ID with timestamp prefix for sortability.

   Format: edge-<timestamp>-<random-suffix>
   Example: edge-20260120T143052-a1b2c3

   The timestamp prefix enables chronological sorting of edges."
  []
  (let [ts (-> (java.time.LocalDateTime/now)
               (.format (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss")))
        suffix (subs (str (java.util.UUID/randomUUID)) 0 6)]
    (str "edge-" ts "-" suffix)))
