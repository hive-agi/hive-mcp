(ns hive-mcp.server.guards
  "Process-role guards for the swarm.

   Provides:
   - Child ling detection (HIVE_MCP_ROLE / HIVE_LING_DEPTH) used to deny
     recursive spawning.
   - Guard configuration flags (enabled?, enforcement mode).
   - Coordinator protection (when-not-coordinator) so test fixtures do not
     corrupt production state."

  (:require [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Configuration
;;; =============================================================================

(def ^:dynamic *enforcement-mode*
  "Enforcement mode for guard violations.

   Values:
     :warn  - Log warning but allow (default)
     :block - Reject with guidance message"
  :warn)

(def ^:dynamic *guard-enabled?*
  "Whether guard checks are enabled at all."
  true)

;; Atom tracking whether coordinator is running.
;; Set via mark-coordinator-running!, checked by when-not-coordinator.
(defonce ^:private coordinator-running-atom (atom false))

;;; =============================================================================
;;; Environment Access
;;; =============================================================================

(defn- get-env-var
  "Get an environment variable. Extracted for testability."
  [var-name]
  (System/getenv var-name))

;;; =============================================================================
;;; Child Ling Detection (Self-Call Prevention)
;;; =============================================================================

(defn child-ling?
  "Check if this JVM is running inside a child ling (not the coordinator).

   Returns true when HIVE_MCP_ROLE env var is set to \"child-ling\".
   This env var is set by spawn code (agent_sdk_strategy.clj, headless.clj)
   when creating child processes, to prevent recursive self-calls.

   The coordinator process never has this env var set."
  []
  (= "child-ling" (get-env-var "HIVE_MCP_ROLE")))

(defn coordinator?
  "Check if this JVM is the coordinator (not a child ling).

   Inverse of child-ling?. Returns true for the main coordinator process
   and false for spawned child lings."
  []
  (not (child-ling?)))

(defn get-role
  "Get the current process role as a string.

   Returns:
     \"child-ling\"  - if running as spawned child
     \"coordinator\" - if running as main coordinator (default)"
  []
  (or (get-env-var "HIVE_MCP_ROLE") "coordinator"))

(defn ling-depth
  "Get the current ling nesting depth.

   Returns the integer value of HIVE_LING_DEPTH env var, or 0 if unset.
   Depth 0 = coordinator, depth 1 = first-level child, etc.
   Used to detect and prevent recursive spawning chains."
  []
  (let [depth-str (get-env-var "HIVE_LING_DEPTH")]
    (if (and depth-str (seq depth-str))
      (try (Integer/parseInt depth-str)
           (catch NumberFormatException _ 0))
      0)))

(defn child-ling-env
  "Generate environment variables map for spawning a child ling.

   Sets HIVE_MCP_ROLE to \"child-ling\" and increments HIVE_LING_DEPTH.
   Pass this to ProcessBuilder env or MCP server env config.

   Returns:
     {\"HIVE_MCP_ROLE\" \"child-ling\"
      \"HIVE_LING_DEPTH\" \"<current+1>\"}"
  []
  {"HIVE_MCP_ROLE" "child-ling"
   "HIVE_LING_DEPTH" (str (inc (ling-depth)))})

;;; =============================================================================
;;; Configuration Helpers
;;; =============================================================================

(defn set-enforcement-mode!
  "Set the enforcement mode.

   Arguments:
     mode - :warn or :block"
  [mode]
  (when (#{:warn :block} mode)
    (alter-var-root #'*enforcement-mode* (constantly mode))
    (log/info "Guard enforcement mode set to:" mode)))

(defn enable-guards!
  "Enable guards."
  []
  (alter-var-root #'*guard-enabled?* (constantly true))
  (log/info "Guards enabled"))

(defn disable-guards!
  "Disable guards."
  []
  (alter-var-root #'*guard-enabled?* (constantly false))
  (log/info "Guards disabled"))

;;; =============================================================================
;;; Status
;;; =============================================================================

(defn guard-status
  "Get current guard configuration status.

   Returns map with :enabled?, :mode, and the process role fields."
  []
  {:enabled? *guard-enabled?*
   :mode *enforcement-mode*
   :child-ling? (child-ling?)
   :role (get-role)
   :ling-depth (ling-depth)
   :agent-id (get-env-var "CLAUDE_SWARM_SLAVE_ID")
   :coordinator-running? @coordinator-running-atom})

;;; =============================================================================
;;; Coordinator Protection
;;; =============================================================================

(defn coordinator-running?
  "Check if coordinator is currently running.

   Used to protect production state from test fixtures.
   Returns true after mark-coordinator-running! is called."
  []
  @coordinator-running-atom)

(defn mark-coordinator-running!
  "Mark that coordinator is running, enabling production guards.

   Called once during server startup. After this:
   - when-not-coordinator will skip destructive operations
   - Test fixtures won't corrupt production state"
  []
  (reset! coordinator-running-atom true)
  (log/info "Coordinator marked as running - production guards active"))

(defn mark-coordinator-stopped!
  "Mark that coordinator has stopped. For testing only."
  []
  (reset! coordinator-running-atom false)
  (log/info "Coordinator marked as stopped"))

(defmacro when-not-coordinator
  "Execute body only if coordinator is NOT running.

   Prevents corrupting production state when tests run in same JVM.

   Arguments:
     msg  - Description logged when skipped
     body - Forms to execute when coordinator not running

   Example:
     (when-not-coordinator \"reset-conn! blocked\"
       (reset! conn (create-conn)))"
  [msg & body]
  `(if (coordinator-running?)
     (log/debug "Guarded operation skipped (coordinator running):" ~msg)
     (do ~@body)))
