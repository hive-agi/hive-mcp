(ns hive-mcp.swarm.datascript
  "DataScript-based state management for swarm hivemind coordination.

   BACKWARD COMPATIBILITY FACADE (TRIMMED)
   ========================================
   This namespace re-exports actively-used public functions from split modules.
   New code should import from sub-modules directly:

   - hive-mcp.swarm.datascript.schema      - Schema definitions
   - hive-mcp.swarm.datascript.connection  - Connection management
   - hive-mcp.swarm.datascript.lings       - Slave/Task/Claim CRUD
   - hive-mcp.swarm.datascript.queries     - Read-only queries
   - hive-mcp.swarm.datascript.coordination - Coordinators/Wrap queue

   DDD: Repository pattern for swarm entity persistence."
  (:require [hive-mcp.swarm.datascript.connection :as connection]
            [hive-mcp.swarm.datascript.lings :as lings]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.datascript.coordination :as coordination]
            [datascript.core :as d]
            [hive-spi.swarm.protocol :as proto]
            [hive-mcp.swarm.datascript.registry :as registry])
  (:refer-clojure :exclude []))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Re-exports from connection (actively used in production)
;;; =============================================================================

(def get-conn #'connection/get-conn)
(def reset-conn! #'connection/reset-conn!)

;;; =============================================================================
;;; Re-exports from lings (actively used in production)
;;; =============================================================================

;; Slave operations
(def add-slave! #'lings/add-slave!)
(def update-slave! #'lings/update-slave!)
(def remove-slave! #'lings/remove-slave!)

;; Critical ops guard
(def can-kill? #'lings/can-kill?)

;; Task operations
(def add-task! #'lings/add-task!)

;; Claim operations
(def claim-file! #'lings/claim-file!)
(def release-claim! #'lings/release-claim!)

;; Task lifecycle
(def complete-task! #'lings/complete-task!)

;;; =============================================================================
;;; Re-exports from queries (actively used in production)
;;; =============================================================================

;; Slave queries
(def get-slave #'queries/get-slave)
(def get-all-slaves #'queries/get-all-slaves)
(def get-slaves-by-status #'queries/get-slaves-by-status)
(def get-slaves-by-project #'queries/get-slaves-by-project)
(def get-slave-ids-by-project #'queries/get-slave-ids-by-project)

;; Claim queries
(def has-conflict? #'queries/has-conflict?)
(def check-file-conflicts #'queries/check-file-conflicts)
(def get-claims-for-file #'queries/get-claims-for-file)

;; Task queries
(def get-task #'queries/get-task)

;; Stats
(def db-stats #'queries/db-stats)

;;; =============================================================================
;;; Re-exports from coordination (actively used in production)
;;; =============================================================================

;; Wrap queue
(def add-wrap-notification! #'coordination/add-wrap-notification!)
(def get-unprocessed-wraps-for-project #'coordination/get-unprocessed-wraps-for-project)
(def get-unprocessed-wraps-for-hierarchy #'coordination/get-unprocessed-wraps-for-hierarchy)
(def get-unprocessed-wraps-for-session #'coordination/get-unprocessed-wraps-for-session)
(def mark-wrap-processed! #'coordination/mark-wrap-processed!)

;; Coordinators (used via resolve in server/init.clj and server/lifecycle.clj)
(def register-coordinator! #'coordination/register-coordinator!)
(def mark-coordinator-terminated! #'coordination/mark-coordinator-terminated!)

;; Stale claim cleanup
(def cleanup-stale-claims! #'coordination/cleanup-stale-claims!)

;; Completed tasks (session-scoped registry)
(def register-completed-task! #'coordination/register-completed-task!)
(def get-completed-task #'coordination/get-completed-task)
(def get-completed-tasks-this-session #'coordination/get-completed-tasks-this-session)
(def clear-completed-tasks! #'coordination/clear-completed-tasks!)

;; Kanban movements (session-scoped status transitions)
(def register-kanban-movement! #'coordination/register-kanban-movement!)
(def get-kanban-movements-this-session #'coordination/get-kanban-movements-this-session)
(def clear-kanban-movements! #'coordination/clear-kanban-movements!)

;;; =============================================================================
;;; Swarm store facade (consumers query the store without datascript.core)
;;; =============================================================================
;;;
;;; Writes, snapshots and listeners go through hive-spi.swarm.protocol/ISwarmDb,
;;; so they follow whatever store the :swarm slot holds and honour the test-conn
;;; isolation seam. The query legs (q / q-db / pull) call datascript.core on the
;;; db VALUE the port hands back, because ISwarmDb carries no query method yet;
;;; this namespace is kernel.edn's declared owner of datascript.core, so the
;;; dependency stops here instead of reaching every consumer.

(defn db-handle
  "The default ISwarmDb for the swarm coordination store."
  []
  (registry/get-default-db))

(defn transact!
  "Transact tx-data against the swarm store. Returns the tx-report."
  [tx-data]
  (proto/-transact! (db-handle) tx-data))

(defn current-db
  "Snapshot (db value) of the swarm store."
  []
  (proto/-current-db (db-handle)))

(defn q
  "Datalog query against the current swarm store snapshot; inputs follow the
   db in :in."
  [query & inputs]
  (apply d/q query (current-db) inputs))

(defn q-db
  "Datalog query against db, a snapshot from current-db or a :db-snapshot
   coeffect; inputs follow the db in :in."
  [db query & inputs]
  (apply d/q query db inputs))

(defn pull
  "Pull pattern for eid (entity id or lookup ref) from the swarm store.
   Returns nil when the entity does not exist."
  [pattern eid]
  (d/pull (current-db) pattern eid))

(defn listen!
  "Register a tx listener under key on the swarm store. Returns key."
  [key callback]
  (proto/-listen! (db-handle) key callback))

(defn unlisten!
  "Remove the tx listener registered under key on the swarm store."
  [key]
  (proto/-unlisten! (db-handle) key))
