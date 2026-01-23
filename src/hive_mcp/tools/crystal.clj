(ns hive-mcp.tools.crystal
  "Crystal/wrap workflow tools for session crystallization.

   Handles progressive crystallization of session data into long-term memory.

   SOLID: Single Responsibility - crystal/wrap logic in dedicated module.
   DDD: Application service layer exposing crystal domain functionality."
  (:require [hive-mcp.emacsclient :as ec]
            [hive-mcp.crystal.hooks :as crystal-hooks]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects]
            [hive-mcp.agent.context :as ctx]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- hive-mcp-el-available?
  "Check if hive-mcp.el is loaded in Emacs."
  []
  (let [{:keys [success result]} (ec/eval-elisp "(featurep 'hive-mcp)")]
    (and success (= result "t"))))

(defn handle-wrap-gather
  "Gather session data for wrap workflow without storing.

   Uses progressive crystallization to harvest:
   - Session progress notes (from kanban completions)
   - Completed tasks (ephemeral notes tagged session-progress)
   - Git commits since session start
   - Recall patterns (for smart promotion)

   Params:
   - directory: Working directory for git operations. Pass caller's cwd to ensure
     git status/commits come from the correct project, not the MCP server's directory.
     Falls back to *request-ctx* directory if not provided.

   Returns gathered data for confirmation before crystallization."
  [{:keys [directory] :as _params}]
  ;; Fallback chain: explicit param → request-ctx
  (let [effective-dir (or directory (ctx/current-directory))]
    (log/info "wrap-gather with crystal harvesting" (when effective-dir (str "directory:" effective-dir)))
    (try
      ;; Use crystal hooks for comprehensive harvesting
      (let [harvested (crystal-hooks/harvest-all {:directory effective-dir})
            ;; Also get elisp-side data for completeness
            ;; Pass directory to elisp so git operations use correct project
            elisp-result (when (hive-mcp-el-available?)
                           (let [elisp-call (if effective-dir
                                              (format "(json-encode (hive-mcp-api-wrap-gather \"%s\"))" effective-dir)
                                              "(json-encode (hive-mcp-api-wrap-gather))")
                                 {:keys [success result]} (ec/eval-elisp elisp-call)]
                             (when success
                               (try (json/read-str result :key-fn keyword)
                                    (catch Exception _ nil)))))
            ;; Merge both sources
            combined {:crystal harvested
                      :elisp elisp-result
                      :session (:session harvested)
                      :summary (merge (:summary harvested)
                                      {:has-elisp-data (some? elisp-result)})}]
        {:type "text"
         :text (json/write-str combined)})
      (catch Exception e
        (log/error e "wrap-gather failed")
        {:type "text"
         :text (json/write-str {:error (.getMessage e)
                                :fallback "Use elisp-only gather"})
         :isError true}))))

(defn handle-wrap-crystallize
  "Crystallize session data into long-term memory.

   Takes harvested data (from wrap-gather) and:
   1. Creates session summary (short-term duration)
   2. Promotes entries that meet score threshold
   3. Flushes recall buffer
   4. Emits wrap_notify event for hivemind permeation

   Params:
   - agent_id (REQUIRED for lings): Ling's slave-id. CRITICAL: MCP server runs in
     coordinator's JVM, so System/getenv reads coordinator's env, NOT the ling's.
     Lings MUST pass their CLAUDE_SWARM_SLAVE_ID explicitly via this parameter
     to ensure proper attribution in wrap-queue and HIVEMIND piggyback messages.
     Falls back to *request-ctx* agent-id if not provided.
   - directory: Working directory for project scoping. Pass your cwd to ensure
     wrap is tagged with correct project-id for scoped permeation.
     Falls back to *request-ctx* directory if not provided.

   Call after wrap-gather when ready to persist."
  [{:keys [agent_id directory] :as _params}]
  ;; Fallback chain: explicit param → request-ctx → env var → default
  (let [effective-dir (or directory (ctx/current-directory))
        ctx-agent-id (ctx/current-agent-id)
        env-agent-id (System/getenv "CLAUDE_SWARM_SLAVE_ID")
        effective-agent (or agent_id ctx-agent-id env-agent-id "coordinator")]
    (log/info "wrap-crystallize" (str "agent-id:" effective-agent)
              (when effective-dir (str "directory:" effective-dir)))
    (try
      (let [harvested (crystal-hooks/harvest-all {:directory effective-dir})
            result (crystal-hooks/crystallize-session harvested)
            ;; Derive project-id from directory for scoped permeation
            project-id (scope/get-current-project-id effective-dir)]

        ;; FIX: Check if crystallize-session returned an error
        ;; This happens when Chroma fails (e.g., embedding not configured)
        (if (:error result)
          (do
            (log/error "wrap-crystallize: crystallize-session failed:" (:error result))
            {:type "text"
             :text (json/write-str (assoc result :project-id project-id))
             :isError true})

          ;; Success path - proceed with wrap_notify and return
          (let [;; Warn if falling back past request-ctx (likely wrong for lings)
                _ (when (and (nil? agent_id) (nil? ctx-agent-id) env-agent-id)
                    (log/warn "wrap-crystallize: agent_id not passed explicitly and no request-ctx,"
                              "falling back to env var" env-agent-id "- this may show as coordinator for ling calls"))
                ;; Warn if defaulting to "coordinator"
                _ (when (and (nil? agent_id) (nil? ctx-agent-id) (nil? env-agent-id))
                    (log/warn "wrap-crystallize: No agent_id provided, no request-ctx, and CLAUDE_SWARM_SLAVE_ID not set"
                              "- defaulting to 'coordinator'. Lings should pass agent_id explicitly."))
                ;; Ensure stats is a map (defensive against JSON decode issues)
                safe-stats (if (map? (:stats result)) (:stats result) {})]
            ;; Emit wrap_notify via hive-events for Crystal Convergence
            (try
              (ev/dispatch [:crystal/wrap-notify
                            {:agent-id effective-agent
                             :session-id (:session result)
                             :project-id project-id
                             :created-ids (when-let [sid (:summary-id result)] [sid])
                             :stats safe-stats}])
              (log/info "wrap-crystallize: emitted wrap_notify for" effective-agent "project:" project-id
                        "summary-id:" (:summary-id result))
              (catch Exception e
                (log/warn "wrap-crystallize: failed to emit wrap_notify:" (.getMessage e))))
            {:type "text"
             :text (json/write-str (assoc result :project-id project-id))})))
      (catch Exception e
        (log/error e "wrap-crystallize failed")
        {:type "text"
         :text (json/write-str {:error (.getMessage e)})
         :isError true}))))

(defn handle-permeate-crystals
  "Process wrap queue entries from ling sessions.

   Coordinator calls this to process wrap notifications from lings:
   1. Gets unprocessed wraps from DataScript queue (filtered by project)
   2. Marks each as processed
   3. Returns stats about what was permeated

   Params:
   - directory: Working directory for project scoping. Pass your cwd to ensure
     only wraps from the current project are permeated. This prevents cross-project
     contamination where wraps from project A would pollute project B's memory.
   - include_children: When true (default), includes wraps from child projects
     using hierarchical prefix matching. E.g., 'myproject' also matches
     'myproject:submodule'. Set to false for exact project matching only.

   Part of Crystal Convergence - hivemind permeating ling session data."
  [{:keys [directory include_children] :as _params}]
  (let [project-id (scope/get-current-project-id directory)
        ;; Default to including children (hierarchical matching)
        include-children? (if (nil? include_children) true include_children)]
    (log/info "permeate-crystals: processing wrap queue for project:" project-id
              "include-children:" include-children?)
    (try
      (let [queue-items (if include-children?
                          (ds/get-unprocessed-wraps-for-hierarchy project-id)
                          (ds/get-unprocessed-wraps-for-project project-id))
            processed-count (count queue-items)
            agent-ids (map :wrap-queue/agent-id queue-items)
            ;; Collect unique project-ids for visibility into hierarchy
            project-ids (distinct (map :wrap-queue/project-id queue-items))]
        ;; Mark each as processed
        (doseq [item queue-items]
          (ds/mark-wrap-processed! (:wrap-queue/id item)))
        ;; Return stats
        {:type "text"
         :text (json/write-str {:processed processed-count
                                :agent-ids (vec agent-ids)
                                :project-id project-id
                                :project-ids-matched (vec project-ids)
                                :include-children include-children?
                                :status "ok"})})
      (catch Exception e
        (log/error e "permeate-crystals failed")
        {:type "text"
         :text (json/write-str {:error (.getMessage e)})
         :isError true}))))

;; Tool definitions
(def tools
  [{:name "wrap_gather"
    :description "Gather session data for wrap workflow. Returns recent notes, git commits, kanban activity without storing. Use before wrap to preview/confirm data."
    :inputSchema {:type "object"
                  :properties {:directory {:type "string"
                                           :description "Working directory for git operations. Pass your cwd to ensure git context comes from your project, not the MCP server's directory."}}
                  :required []}
    :handler handle-wrap-gather}

   {:name "wrap_crystallize"
    :description "Crystallize session data into long-term memory. Creates session summary, promotes entries meeting score threshold, and flushes recall buffer. Call after wrap_gather to persist. CRITICAL FOR LINGS: You MUST pass your CLAUDE_SWARM_SLAVE_ID as agent_id parameter - the MCP server runs in coordinator's JVM so env vars won't work. Without explicit agent_id, wrap_notify will show 'coordinator' instead of your ling ID."
    :inputSchema {:type "object"
                  :properties {:agent_id {:type "string"
                                          :description "REQUIRED FOR LINGS: Your CLAUDE_SWARM_SLAVE_ID. Without this, wrap attribution shows 'coordinator' instead of your ling ID. MCP server runs in coordinator's JVM - System.getenv reads coordinator's env, not yours."}
                               :directory {:type "string"
                                           :description "Working directory for project scoping. Pass your cwd to ensure wrap is tagged with correct project-id for scoped permeation."}}
                  :required []}
    :handler handle-wrap-crystallize}

   {:name "mcp_permeate_crystals"
    :description "Process wrap queue from ling sessions. Coordinator calls this to permeate ling session data. Returns stats about processed wraps including agent IDs and counts. IMPORTANT: Pass directory to scope permeation to current project only."
    :inputSchema {:type "object"
                  :properties {:directory {:type "string"
                                           :description "Working directory for project scoping. Pass your cwd to ensure only wraps from the current project are permeated, preventing cross-project contamination."}
                               :include_children {:type "boolean"
                                                  :description "When true (default), includes wraps from child projects using hierarchical prefix matching. E.g., 'myproject' also matches 'myproject:submodule'. Set to false for exact project matching only."}}
                  :required []}
    :handler handle-permeate-crystals}])
