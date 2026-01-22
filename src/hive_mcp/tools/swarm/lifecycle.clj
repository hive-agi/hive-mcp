(ns hive-mcp.tools.swarm.lifecycle
  "Swarm lifecycle handlers - spawn and kill operations.

   ADR-001 Phase 2: Registration handled by event-driven sync.
   The slave-spawned event from elisp triggers register-ling! via
   channel subscription (see registry/start-registry-sync!).

   ADR-003: Kill guard prevents termination during critical operations
   (wrap, commit, dispatch). Uses datascript critical-ops tracking.

   Project-scoped kill: When slave_id='all' and directory is provided,
   only kills lings belonging to that project (not all lings globally).

   SOLID: SRP - Single responsibility for spawn/kill operations.
   CLARITY: Y - Yield safe failure with timeout handling."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.registry :as registry]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.hivemind :as hivemind]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [hive-mcp.tools.memory.scope :as scope]
            [clojure.string :as str]
            [hive-mcp.telemetry.prometheus :as prom]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Spawn Handler
;; ============================================================

(defn handle-swarm-spawn
  "Spawn a new Claude slave instance.
   Uses timeout to prevent MCP blocking.

   ADR-001 Phase 2: Registration now handled by event-driven sync.
   The slave-spawned event from elisp triggers register-ling! via
   channel subscription (see registry/start-registry-sync!).

   Parameters:
   - name: Name for the slave (required)
   - presets: List of preset names to apply
   - cwd: Working directory for the slave
   - role: Predefined role
   - terminal: Terminal type (vterm/eat)

   CLARITY: I - Inputs validated (name required)
   SOLID: SRP - Only handles spawn, not registration"
  [{:keys [name presets cwd _role terminal]}]
  (core/with-swarm
    (let [presets-str (when (seq presets)
                        (format "'(%s)" (str/join " " (map #(format "\"%s\"" %) presets))))
          elisp (format "(json-encode (hive-mcp-swarm-api-spawn \"%s\" %s %s %s))"
                        (v/escape-elisp-string (or name "slave"))
                        (or presets-str "nil")
                        (if cwd (format "\"%s\"" (v/escape-elisp-string cwd)) "nil")
                        (if terminal (format "\"%s\"" terminal) "nil"))
          ;; Use 10s timeout for spawn as it may take longer
          {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 10000)]
      (cond
        timed-out
        (core/mcp-timeout-error "Spawn operation" :extra-data {:slave_name name})

        success
        ;; ADR-001 Phase 2: No manual registration here.
        ;; Registration is handled by event-driven sync when elisp emits
        ;; slave-spawned event via channel (see registry/start-registry-sync!).
        (do
          ;; CLARITY-T: Update Prometheus gauge (estimate +1 until registry sync)
          (let [current-count (count (registry/get-available-lings))]
            (prom/set-lings-active! (inc current-count)))
          (core/mcp-success result))

        :else
        (core/mcp-error (str "Error: " error))))))

;; ============================================================
;; Kill Handler
;; ============================================================

(defn- format-blocking-ops
  "Format blocking operations for error message."
  [ops]
  (str/join ", " (map name ops)))

(defn- check-any-critical-ops
  "Check if any slaves have critical operations blocking kill-all.
   Returns {:can-kill? bool :blocked-slaves [{:id :ops}...]}"
  ([]
   (check-any-critical-ops nil))
  ([slave-ids]
   (let [ids-to-check (or slave-ids (keys (registry/get-available-lings)))
         blocked (->> ids-to-check
                      (map (fn [slave-id]
                             (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave-id)]
                               (when-not can-kill?
                                 {:id slave-id :ops blocking-ops}))))
                      (filter some?))]
     (if (seq blocked)
       {:can-kill? false :blocked-slaves blocked}
       {:can-kill? true :blocked-slaves []}))))

(defn- kill-single-slave!
  "Kill a single slave by ID. Returns MCP response.

   CLARITY: SRP - Extracted from handle-swarm-kill for reuse."
  [slave_id]
  (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave_id)]
    (if can-kill?
      ;; Safe to kill
      (let [elisp (format "(json-encode (hive-mcp-swarm-api-kill \"%s\"))"
                          (v/escape-elisp-string slave_id))
            {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 3000)]
        (cond
          timed-out
          {:success false :error "timeout" :slave-id slave_id}

          success
          (do
            ;; Clear from both registries: DataScript + hivemind agent-registry
            (hivemind/clear-agent! slave_id)
            {:success true :result result :slave-id slave_id})

          :else
          {:success false :error error :slave-id slave_id}))
      ;; Blocked by critical ops
      {:success false
       :error (format "critical ops: %s" (format-blocking-ops blocking-ops))
       :slave-id slave_id})))

(defn handle-swarm-kill
  "Kill a slave or all slaves.
   Uses short timeout (3s) as kill should be fast.
   Removes killed lings from registry.

   KILL GUARD (ADR-003): Blocks kill during critical operations.
   Critical ops: :wrap (session crystallization), :commit (git),
   :dispatch (task dispatch in progress).

   PROJECT-SCOPED KILL: When slave_id='all' and directory is provided,
   only kills lings belonging to that project (not all lings globally).

   Parameters:
   - slave_id: ID of slave to kill, or \"all\" to kill all
   - directory: Working directory to scope kill (optional, for 'all' only)

   CLARITY: Y - Yield safe failure with timeout handling
   CLARITY: I - Inputs guarded (critical ops check)
   SOLID: SRP - Only handles kill operation"
  [{:keys [slave_id directory]}]
  (core/with-swarm
    ;; Kill Guard: Check for critical operations before proceeding
    (if (= slave_id "all")
      ;; Kill all (optionally project-scoped)
      (let [project-id (when directory (scope/get-current-project-id directory))
            ;; Get slave IDs to kill (project-scoped or all)
            slave-ids (if project-id
                        (ds/get-slave-ids-by-project project-id)
                        (keys (registry/get-available-lings)))
            {:keys [can-kill? blocked-slaves]} (check-any-critical-ops slave-ids)]
        (cond
          ;; No lings to kill
          (empty? slave-ids)
          (core/mcp-success {:killed 0
                             :project-id project-id
                             :message (if project-id
                                        (format "No lings found for project '%s'" project-id)
                                        "No lings to kill")})

          ;; Blocked by critical ops
          (not can-kill?)
          (core/mcp-error
           (format "KILL BLOCKED: Cannot kill slaves with critical operations in progress. Blocked slaves: %s"
                   (str/join ", " (map (fn [{:keys [id ops]}]
                                         (format "%s (%s)" id (format-blocking-ops ops)))
                                       blocked-slaves))))

          ;; Project-scoped kill: kill each ling individually
          project-id
          (let [results (mapv kill-single-slave! slave-ids)
                killed (filter :success results)
                failed (remove :success results)]
            ;; Update Prometheus gauge
            (let [current-count (count (registry/get-available-lings))]
              (prom/set-lings-active! (max 0 (- current-count (count killed)))))
            (core/mcp-success {:killed (count killed)
                               :failed (count failed)
                               :project-id project-id
                               :details {:killed (mapv :slave-id killed)
                                         :failed (mapv #(select-keys % [:slave-id :error]) failed)}}))

          ;; Global kill-all (no directory provided)
          :else
          (let [elisp "(json-encode (hive-mcp-swarm-api-kill-all))"
                {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 3000)]
            (cond
              timed-out
              (core/mcp-timeout-error "Kill operation" :extra-data {:slave_id slave_id})

              success
              (do
                ;; Clear all registries: DataScript + hivemind agent-registry
                ;; Bug fix (task 9871bcf4): hivemind_status showed stale entries
                (registry/clear-registry!)
                (reset! hivemind/agent-registry {})
                ;; CLARITY-T: Reset Prometheus gauge to 0
                (prom/set-lings-active! 0)
                (core/mcp-success result))

              :else
              (core/mcp-error (str "Error: " error))))))
      ;; Single slave kill
      (let [{:keys [can-kill? blocking-ops]} (ds/can-kill? slave_id)]
        (if can-kill?
          ;; Safe to kill
          (let [elisp (format "(json-encode (hive-mcp-swarm-api-kill \"%s\"))"
                              (v/escape-elisp-string slave_id))
                {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp 3000)]
            (cond
              timed-out
              (core/mcp-timeout-error "Kill operation" :extra-data {:slave_id slave_id})

              success
              (do
                ;; Clear from both registries: DataScript + hivemind agent-registry
                ;; Bug fix (task 9871bcf4): hivemind_status showed stale entries
                (hivemind/clear-agent! slave_id)
                ;; CLARITY-T: Decrement Prometheus gauge (estimate current - 1)
                (let [current-count (count (registry/get-available-lings))]
                  (prom/set-lings-active! (max 0 (dec current-count))))
                (core/mcp-success result))

              :else
              (core/mcp-error (str "Error: " error))))
          ;; Blocked by critical ops
          (core/mcp-error
           (format "KILL BLOCKED: Cannot kill slave '%s' - critical operation(s) in progress: %s. Wait for completion or use force if necessary."
                   slave_id
                   (format-blocking-ops blocking-ops))))))))
