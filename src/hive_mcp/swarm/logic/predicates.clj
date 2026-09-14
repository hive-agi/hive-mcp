(ns hive-mcp.swarm.logic.predicates
  "Pure core.logic predicates and relations for swarm coordination.

   This module contains:
   1. Database Relations (pldb/db-rel) - Schema definitions
   2. Core Predicates - Pure logic goals

   CLARITY Compliance:
   - C (Composition): Predicates compose via core.logic conde/all
   - L (Layers): Pure logic, no side effects or state
   - R (Represented Intent): Clear predicate naming with -o suffix

   Naming Convention:
   - Predicates end in -o (core.logic convention for goals)
   - Relations are nouns (slave, task, claims, etc.)

   See Also:
   - swarm/logic.clj - Stateful operations using these predicates"
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Database Relations (pldb/db-rel)
;; =============================================================================

;; Slave entity: tracks slave state
;; slave-id: unique identifier (e.g., "swarm-worker-123")
;; status: :idle :working :spawning :starting :error
(pldb/db-rel slave ^:index slave-id status)

;; Task entity: tracks task ownership and state
;; task-id: unique identifier
;; slave-id: which slave owns this task
;; status: :dispatched :completed :timeout :error
(pldb/db-rel task ^:index task-id slave-id status)

;; File claim: which slave currently "owns" a file
;; file-path: absolute or relative path to file
;; slave-id: the slave working on this file
(pldb/db-rel claims ^:index file-path slave-id)

;; Task dependency: task-id depends on dep-task-id completing first
(pldb/db-rel depends-on ^:index task-id dep-task-id)

;; Task files: associates a task with files it operates on
;; Used for releasing claims when task completes
(pldb/db-rel task-files ^:index task-id file-path)

;; =============================================================================
;; Core Predicates (Logic Goals)
;; =============================================================================

(defn file-conflicto
  "Goal: succeeds if file-path is claimed by a DIFFERENT slave.

   Arguments:
   - file-path: the file being checked
   - requesting-slave: the slave wanting to claim the file
   - conflicting-slave: (output) the slave that has conflicting claim"
  [file-path requesting-slave conflicting-slave]
  (l/all
   (claims file-path conflicting-slave)
   (l/!= requesting-slave conflicting-slave)))

(defn task-completedo
  "Goal: succeeds if task-id has status :completed."
  [task-id]
  (l/fresh [slave-id]
           (task task-id slave-id :completed)))

(defn task-pendingo
  "Goal: succeeds if task-id is NOT completed."
  [task-id]
  (l/fresh [slave-id status]
           (task task-id slave-id status)
           (l/!= status :completed)))

;; =============================================================================
;; Circular Dependency Detection
;; =============================================================================

(defn reachable-fromo
  "Goal: succeeds if target is reachable from source via depends-on relation.
   This is the transitive closure of the dependency graph."
  [source target]
  (l/conde
    ;; Direct dependency
   [(depends-on source target)]
    ;; Transitive dependency
   [(l/fresh [mid]
             (depends-on source mid)
             (reachable-fromo mid target))]))

(defn would-deadlocko
  "Goal: succeeds if adding dependency from task-a to task-b would create a cycle.

   A cycle would exist if task-b can already reach task-a (meaning task-a
   somehow depends on task-b, so making task-a depend on task-b creates a loop)."
  [task-a task-b]
  (reachable-fromo task-b task-a))
