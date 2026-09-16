(ns hive-mcp.swarm.logic
  "Logic programming engine for swarm hivemind coordination.

   WHY CORE.LOGIC (vs DataScript)?
   ===============================
   This module uses core.logic pldb (Prolog-style in-memory database) for
   **declarative constraint queries** that would be awkward in DataScript:

   1. Transitive closure (reachability) - deadlock detection via reachable-fromo
   2. Negation-as-failure - conflict detection (file claimed by DIFFERENT slave)

   DataScript (swarm/datascript.clj) handles **entity persistence**:
   - CRUD operations on slaves, tasks, coordinators, wraps
   - Datomic-style pull queries for entity attributes
   - Transaction history and listeners

   WHEN TO USE WHICH:
   - Need to store/query entity state? → DataScript
   - Need transitive/recursive queries? → core.logic (this module)
   - Need conflict/constraint checking? → core.logic (this module)

   THREAD SAFETY
   =============
   All mutations go through atom swap! operations. For atomic check+claim,
   use coordinator/atomic-claim-files! which locks the logic-db atom.

   SEE ALSO
   ========
   - swarm/logic/predicates.clj - Pure relations and predicates
   - swarm/coordinator.clj - High-level API using this module
   - swarm/datascript.clj  - Entity state management"
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [taoensso.timbre :as log]
            [hive-mcp.swarm.logic.predicates :as pred]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Internal Relations & Predicates (from logic.predicates)
;; =============================================================================

(def ^:private slave pred/slave)
(def ^:private task pred/task)
(def ^:private claims pred/claims)
(def ^:private task-files pred/task-files)

(def ^:private file-conflicto #'pred/file-conflicto)
(def ^:private would-deadlocko #'pred/would-deadlocko)

;; =============================================================================
;; Database State (Thread-Safe Atom)
;; =============================================================================

(defonce ^:private logic-db
  (atom (pldb/db)))

(defn get-logic-db-atom
  "Get the logic-db atom for external locking.

   Used by coordinator.clj for atomic check+claim operations.
   DO NOT use for direct manipulation - use the provided mutation functions instead."
  []
  logic-db)

(defn reset-db!
  "Reset the logic database to empty state."
  []
  (reset! logic-db (pldb/db))
  (log/debug "Logic database reset"))

(defmacro ^:private with-db
  "Execute a logic query against the current database."
  [& body]
  `(pldb/with-db @logic-db ~@body))

;; =============================================================================
;; Database Mutation Functions
;; =============================================================================

(defn add-slave!
  "Add a slave to the logic database."
  [slave-id status]
  (swap! logic-db pldb/db-fact slave slave-id status)
  (log/debug "Added slave to logic db:" slave-id status))

(defn slave-exists?
  "Check if a slave exists in the database."
  [slave-id]
  (not (empty? (with-db
                 (l/run 1 [s]
                        (slave slave-id s))))))

(defn add-claim!
  "Add a file claim for a slave."
  [file-path slave-id]
  (swap! logic-db pldb/db-fact claims file-path slave-id)
  (log/debug "Added file claim:" file-path "→" slave-id))

(defn remove-claim!
  "Remove a specific file claim."
  [file-path slave-id]
  (swap! logic-db pldb/db-retraction claims file-path slave-id)
  (log/debug "Removed file claim:" file-path "→" slave-id))

(defn release-claims-for-slave!
  "Release all file claims for a slave."
  [slave-id]
  (let [files (with-db
                (l/run* [f]
                        (claims f slave-id)))]
    (doseq [f files]
      (remove-claim! f slave-id))
    (log/debug "Released" (count files) "claims for slave:" slave-id)))

(defn get-all-claims
  "Get all file claims from logic-db.
   Returns vector of {:file path :slave-id id} maps."
  []
  (vec (with-db
         (l/run* [q]
                 (l/fresh [f s]
                          (claims f s)
                          (l/== q {:file f :slave-id s}))))))

(defn add-task-file!
  "Associate a file with a task (for claim tracking)."
  [task-id file-path]
  (swap! logic-db pldb/db-fact task-files task-id file-path))

(defn get-files-for-task
  "Get all files associated with a task.
   Returns vector of file paths."
  [task-id]
  (vec (with-db
         (l/run* [f]
                 (task-files task-id f)))))

(defn release-claims-for-task!
  "Release all file claims associated with a task.

   Gets slave-id from claims relation directly, not from task relation.
   This is necessary because atomic-claim-files! populates task-files and claims
   but NOT the task relation."
  [task-id]
  (let [files (with-db
                (l/run* [f]
                        (task-files task-id f)))
        released-count (atom 0)]
    ;; For each file, find and remove its claim from claims relation directly
    (doseq [f files]
      (let [slave-id (first (with-db
                              (l/run 1 [s]
                                     (claims f s))))]
        (when slave-id
          (remove-claim! f slave-id)
          (swap! released-count inc))))
    (log/debug "Released" @released-count "claims for task:" task-id)))

;; =============================================================================
;; Query Functions (Public API)
;; =============================================================================

(defn check-file-conflicts
  "Check for file conflicts for a proposed set of files.
   Returns list of {:file path :held-by slave-id} conflicts."
  [requesting-slave files]
  (when (seq files)
    (with-db
      (l/run* [q]
              (l/fresh [file other-slave]
                       (l/membero file files)
                       (file-conflicto file requesting-slave other-slave)
                       (l/== q {:file file :held-by other-slave}))))))

(defn check-would-deadlock
  "Check if adding a dependency would create a circular dependency.
   Returns true if deadlock would occur."
  [task-id dep-task-id]
  (not (empty?
        (with-db
          (l/run 1 [q]
                 (would-deadlocko task-id dep-task-id)
                 (l/== q :cycle))))))

(defn get-claim-for-file
  "Get claim info for a specific file path.
   Returns {:file path :slave-id id} or nil if not claimed."
  [file-path]
  (first
   (with-db
     (l/run 1 [q]
            (l/fresh [s]
                     (claims file-path s)
                     (l/== q {:file file-path :slave-id s}))))))

(defn release-claim-for-file!
  "Release a claim for a specific file path, regardless of owner.
   Returns true if claim was released, false if file wasn't claimed."
  [file-path]
  (if-let [claim (get-claim-for-file file-path)]
    (do
      (remove-claim! file-path (:slave-id claim))
      true)
    false))

;; =============================================================================
;; Debugging Helpers
;; =============================================================================

(defn- get-all-slaves
  "Get all registered slaves."
  []
  (with-db
    (l/run* [q]
            (l/fresh [id status]
                     (slave id status)
                     (l/== q {:slave-id id :status status})))))

(defn- get-all-tasks
  "Get all registered tasks."
  []
  (with-db
    (l/run* [q]
            (l/fresh [tid sid status]
                     (task tid sid status)
                     (l/== q {:task-id tid :slave-id sid :status status})))))

(defn db-stats
  "Get statistics about the current database state."
  []
  {:slaves (count (get-all-slaves))
   :tasks (count (get-all-tasks))
   :claims (count (get-all-claims))})
