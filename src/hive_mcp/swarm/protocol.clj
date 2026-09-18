(ns hive-mcp.swarm.protocol
  "Swarm state management protocols (ISP-segregated).

   Provides unified interfaces for swarm persistence, enabling
   multiple backend implementations (DataScript, Datalevin, etc.).

   Protocols (ISP — Interface Segregation Principle):
   - ISwarmRegistry   — Slave + Task CRUD
   - IClaimStore      — File claim lifecycle
   - ICriticalOps     — Kill guard operations
   - ICoordination    : Wrap queue, coordinators, session registries
   - ISwarmDb         — Low-level DB access boundary

   Design: Functional DDD Repository pattern.
   OCP via protocols + records (compiles to Java interfaces).
   DIP: All consumer code depends on these abstractions, never on backends.

   Compat shim; the protocols live in hive-spi.swarm.protocol."
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(:require [hive-spi.swarm.protocol :as spi]))

(def ISwarmRegistry spi/ISwarmRegistry)
(def IClaimStore spi/IClaimStore)
(def ICriticalOps spi/ICriticalOps)
(def ICoordination spi/ICoordination)
(def ISwarmDb spi/ISwarmDb)

(def add-slave! spi/add-slave!)
(def get-slave spi/get-slave)
(def update-slave! spi/update-slave!)
(def remove-slave! spi/remove-slave!)
(def get-all-slaves spi/get-all-slaves)
(def get-slaves-by-status spi/get-slaves-by-status)
(def get-slaves-by-project spi/get-slaves-by-project)
(def add-task! spi/add-task!)
(def get-task spi/get-task)
(def update-task! spi/update-task!)
(def get-tasks-for-slave spi/get-tasks-for-slave)

(def -claim-file! spi/-claim-file!)
(def -release-claim! spi/-release-claim!)
(def -release-claims-for-slave! spi/-release-claims-for-slave!)
(def -release-claims-for-task! spi/-release-claims-for-task!)
(def -get-claims-for-file spi/-get-claims-for-file)
(def -get-all-claims spi/-get-all-claims)
(def -has-conflict? spi/-has-conflict?)
(def -check-file-conflicts spi/-check-file-conflicts)
(def -refresh-claim! spi/-refresh-claim!)
(def -cleanup-stale-claims! spi/-cleanup-stale-claims!)
(def -archive-claim-to-history! spi/-archive-claim-to-history!)
(def -get-recent-claim-history spi/-get-recent-claim-history)
(def -add-to-wait-queue! spi/-add-to-wait-queue!)

(def -enter-critical-op! spi/-enter-critical-op!)
(def -exit-critical-op! spi/-exit-critical-op!)
(def -get-critical-ops spi/-get-critical-ops)
(def -can-kill? spi/-can-kill?)

(def -add-wrap-notification! spi/-add-wrap-notification!)
(def -get-unprocessed-wraps spi/-get-unprocessed-wraps)
(def -get-unprocessed-wraps-for-project spi/-get-unprocessed-wraps-for-project)
(def -get-unprocessed-wraps-for-hierarchy spi/-get-unprocessed-wraps-for-hierarchy)
(def -mark-wrap-processed! spi/-mark-wrap-processed!)
(def -register-coordinator! spi/-register-coordinator!)
(def -update-heartbeat! spi/-update-heartbeat!)
(def -get-coordinator spi/-get-coordinator)
(def -get-all-coordinators spi/-get-all-coordinators)
(def -get-coordinators-for-project spi/-get-coordinators-for-project)
(def -mark-coordinator-terminated! spi/-mark-coordinator-terminated!)
(def -cleanup-stale-coordinators! spi/-cleanup-stale-coordinators!)
(def -remove-coordinator! spi/-remove-coordinator!)
(def -register-completed-task! spi/-register-completed-task!)
(def -get-completed-tasks-this-session spi/-get-completed-tasks-this-session)
(def -clear-completed-tasks! spi/-clear-completed-tasks!)
(def -register-kanban-movement! spi/-register-kanban-movement!)
(def -get-kanban-movements-this-session spi/-get-kanban-movements-this-session)
(def -clear-kanban-movements! spi/-clear-kanban-movements!)

(def -transact! spi/-transact!)
(def -current-db spi/-current-db)
(def -listen! spi/-listen!)
(def -unlisten! spi/-unlisten!)
(def -db-stats spi/-db-stats)
(def -reset-db! spi/-reset-db!)
(def -close! spi/-close!)

(def slave-statuses spi/slave-statuses)
(def task-statuses spi/task-statuses)

(def add-slave!* spi/add-slave!*)
(def get-slave* spi/get-slave*)
(def update-slave!* spi/update-slave!*)
(def remove-slave!* spi/remove-slave!*)
(def get-all-slaves* spi/get-all-slaves*)
(def get-slaves-by-status* spi/get-slaves-by-status*)
(def get-slaves-by-project* spi/get-slaves-by-project*)
(def add-task!* spi/add-task!*)
(def get-task* spi/get-task*)
(def update-task!* spi/update-task!*)
(def get-tasks-for-slave* spi/get-tasks-for-slave*)
