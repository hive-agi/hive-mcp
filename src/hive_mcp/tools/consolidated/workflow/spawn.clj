(ns hive-mcp.tools.consolidated.workflow.spawn
  "Spawn engine for forge workflows: ling spawning and the spark! orchestrator.

   Key refactorings from parent workflow.clj:
   - spawn-one! unifies spawn-one-vterm! and spawn-one-headless! (DRY)
   - spark! decomposed into compute-route-batches + execute-spawn-batches

   Lings are the only forge execution unit.

   Extracted from workflow.clj to reduce cyclomatic complexity."
  (:require [hive-mcp.tools.consolidated.workflow.readiness :as ready]
            [hive-mcp.tools.consolidated.workflow.execution-routing :as routing]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.tools.agent.dispatch :as dispatch]
            [hive-mcp.tools.consolidated.kanban :as c-kanban]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.config.core :as config]
            [hive-mcp.agent.budget-router :as budget-router]
            [hive-mcp.agent.spawn-mode-registry :as spawn-registry]
            [hive-mcp.agent.ling.lifecycle :as lifecycle]
            [hive-mcp.agent.ling.headless-registry :as headless-reg]
            [hive-mcp.agent.ling.terminal-registry :as terminal-reg]
            [hive-mcp.dns.result :as result]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.workflows.support :as support]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ── Constants ───────────────────────────────────────────────────────────────

(def ^:private vterm-max-slots
  "Hard cap for Emacs-bound lings per daemon (claude or vterm)."
  (spawn-registry/slot-limit :claude))

(def ^:private headless-modes
  "Set of spawn modes that don't require Emacs."
  spawn-registry/headless-modes)

;; ── Effect Ports ────────────────────────────────────────────────────────────

(def ^:private default-spark-ports
  "Effect ports spark! uses when a caller supplies none. Every default is a var,
   so it is resolved when the port is invoked.
     :spawn-agent-fn  params -> MCP spawn result
     :await-ready-fn  (agent-id spawn-mode) -> readiness map
     :send-prompt-fn  {:agent_id :prompt} -> MCP dispatch result
     :kanban-fn       kanban command map -> MCP result (in-progress marking)
     :agents-fn       () -> slave maps
     :project-id-fn   directory -> project-id"
  {:spawn-agent-fn #'spawn/handle-spawn
   :await-ready-fn #'ready/wait-for-ling-ready
   :send-prompt-fn #'dispatch/handle-dispatch
   :kanban-fn      #'c-kanban/handle-kanban
   :agents-fn      #'queries/get-all-slaves
   :project-id-fn  #'scope/get-current-project-id})

(defn- spark-ports
  "Supplied ports over default-spark-ports; an absent or nil port takes its default."
  [ports]
  (merge default-spark-ports (into {} (remove (comp nil? val)) ports)))

;; ── Budget Routing ──────────────────────────────────────────────────────────

(defn- budget-route-model
  "When budget-routing config is enabled, use budget-router to pick model.
   When disabled (default), returns the requested model unchanged."
  [requested-model]
  (if (config/get-service-value :forge :budget-routing :default false)
    (let [suggestion (budget-router/suggest-model {:model requested-model})
          selected   (:model suggestion)]
      (when (:downgraded? suggestion false)
        (log/info "SPARK: budget-router downgraded model"
                  {:requested requested-model :selected selected
                   :tier (:tier suggestion) :reason (:reason suggestion)}))
      selected)
    requested-model))

;; ── Unified Spawn-One ───────────────────────────────────────────────────────

(defn- make-spawn-params
  "Build spawn params, preserving each task's execution overrides."
  [{:keys [agent-name effective-dir default-presets model provider task spawn-mode-kw task-id]}]
  (let [execution (get-in task [:context :execution])
        model (or (:model execution) model)
        provider (or (:provider execution) provider)
        presets (or (:presets execution) default-presets)
        requested-mode (keyword (or (:spawn-mode execution) spawn-mode-kw :claude))
        mode (if (or provider (:spawn-mode execution))
               (lifecycle/resolve-effective-mode {:spawn-mode requested-mode})
               requested-mode)
        _ (when (or provider (:spawn-mode execution))
            (when-not (or (headless-reg/get-headless-backend mode)
                          (terminal-reg/get-terminal-addon mode))
              (throw (ex-info "Execution spawn mode has no registered strategy"
                              {:type :execution/unavailable-mode :spawn-mode mode})))
            (let [providers (if (= :claude mode) #{:claude}
                                (:provides (headless-reg/headless-metadata mode)))]
              (when (and provider (seq providers)
                         (not (contains? providers (keyword provider))))
                (throw (ex-info "Provider is incompatible with execution spawn mode"
                                {:type :execution/provider-mismatch
                                 :provider provider :spawn-mode mode :supported providers})))))]
    (cond-> {:type "ling" :name agent-name :cwd effective-dir :presets presets}
      task-id (assoc :kanban_task_id task-id)
      model (assoc :model model)
      provider (assoc :provider provider)
      (or (:spawn-mode execution) (not= :claude mode))
      (assoc :spawn_mode (subs (str mode) 1)))))

(defn- parse-spawn-result
  "Extract agent-id and spawn-mode from spawn handler response."
  [spawn-result fallback-name]
  (let [spawn-text   (when (map? spawn-result) (:text spawn-result))
        spawn-parsed (when (string? spawn-text)
                       (result/rescue nil (json/read-str spawn-text :key-fn keyword)))]
    {:agent-id   (or (:agent-id spawn-parsed) fallback-name)
     :spawn-mode (when-let [sm (:spawn-mode spawn-parsed)] (keyword sm))}))

(defn- try-dispatch!
  "Single dispatch attempt through send-prompt-fn. Returns its raw result map."
  [send-prompt-fn agent-id task]
  (send-prompt-fn
   {:agent_id agent-id
    :prompt   (str (or (:title task) (:id task) "untitled")
                   (when-let [desc (:description task)]
                     (str "\n\n" desc))
                   "\n\nImplement directly.")}))

(defn- dispatch-to-ling!
  "Send task prompt to a ready ling and update kanban status through `ports`.
   Returns nil on success, or {:dispatch-error <msg>} on failure.
   Retries once after 2s on first failure."
  [{:keys [agent-id task effective-dir ports]}]
  (let [{:keys [send-prompt-fn kanban-fn]} (spark-ports ports)
        result1 (try-dispatch! send-prompt-fn agent-id task)]
    (if (:isError result1)
      (do
        (log/warn "DISPATCH: first attempt failed, retrying in 2s"
                  {:agent-id agent-id :error (:text result1)})
        (Thread/sleep 2000)
        (let [result2 (try-dispatch! send-prompt-fn agent-id task)]
          (if (:isError result2)
            (do
              (log/error "DISPATCH: both attempts failed"
                         {:agent-id agent-id :error (:text result2)})
              {:dispatch-error (str "dispatch failed after 2 attempts: " (:text result2))})
            (do
              (support/mark-task-inprogress! kanban-fn effective-dir (:id task))
              nil))))
      (do
        (support/mark-task-inprogress! kanban-fn effective-dir (:id task))
        nil))))

(defn- spawn-and-wait!
  "Spawn with persona installed before startup; undo registration on failure.
   Spawn and readiness go through `ports`."
  [{:keys [agent-name task spawn-mode-kw ports] :as opts}]
  (let [{:keys [spawn-agent-fn await-ready-fn]} (spark-ports ports)
        params (make-spawn-params opts)
        persona (get-in task [:context :execution :persona])
        register! (when persona (ext/get-extension :agent/register-persona-lens))
        unregister! (when persona (ext/get-extension :agent/unregister-persona-lens))
        registered-id (atom agent-name)]
    (when (and persona (not (and register! unregister!)))
      (throw (ex-info "Persona catchup provider unavailable"
                      {:type :persona/unavailable :agent-id agent-name})))
    (when persona (register! agent-name persona))
    (try
      (let [spawn-result (spawn-agent-fn params)
            payload (try (json/read-str (:text spawn-result) :key-fn keyword)
                         (catch Exception _ nil))
            _ (when (or (:isError spawn-result) (false? (:success payload)))
                (throw (ex-info "Agent spawn failed"
                                {:type :execution/spawn-failed :result spawn-result})))
            parsed (parse-spawn-result spawn-result agent-name)
            agent-id (:agent-id parsed)
            reported-mode (or (:spawn-mode parsed) (some-> (:spawn_mode params) keyword) spawn-mode-kw)
            _ (when (and persona (not= agent-name agent-id))
                (register! agent-id persona)
                (reset! registered-id agent-id)
                (unregister! agent-name))]
        (assoc (await-ready-fn agent-id reported-mode)
               :agent-id agent-id :spawn-mode reported-mode))
      (catch Exception e
        (when persona (unregister! @registered-id))
        (throw e)))))

(defn- handle-ready-ling
  "Pure: assemble success result map after dispatch."
  [{:keys [agent-id title task-id route model spawn-mode best-effort?]}]
  (cond-> {:agent-id agent-id :task-title title :task-id task-id
           :spawned true :route route :model model}
    (not= :claude route) (assoc :spawn-mode spawn-mode)
    best-effort?         (assoc :best-effort true)))

(defn- handle-timeout-ling
  "Handle ling that timed out during readiness.
   Slave exists → best-effort dispatch; no slave → error result."
  [{:keys [spawn-result task effective-dir base-result title task-id route model spawn-mode ports]}]
  (let [{:keys [agent-id slave elapsed-ms phase]} spawn-result]
    (if slave
      (do
        (log/warn (str "SPARK[" (name route) "]: readiness timeout but slave exists — dispatching anyway")
                  {:agent-id agent-id :elapsed-ms elapsed-ms :phase phase})
        (let [dr (dispatch-to-ling! {:agent-id agent-id :task task
                                     :effective-dir effective-dir :ports ports})]
          (if-let [err (:dispatch-error dr)]
            (do (log/warn (str "SPARK[" (name route) "]: best-effort dispatch failed")
                          {:agent agent-id :task title :error err})
                (assoc base-result :agent-id agent-id :error err))
            (do (log/info (str "SPARK[" (name route) "]: best-effort dispatch succeeded")
                          {:agent agent-id :task title})
                (handle-ready-ling {:agent-id agent-id :title title :task-id task-id
                                    :route route :model model :spawn-mode spawn-mode
                                    :best-effort? true})))))
      (do
        (log/warn (str "SPARK[" (name route) "]: ling not in DB, skipping dispatch")
                  {:agent-id agent-id :elapsed-ms elapsed-ms :phase phase})
        (assoc base-result :agent-id agent-id
               :error (str "Readiness timeout (" (name (or phase :unknown)) ")"))))))

(defn- spawn-one!
  "Spawn and dispatch a single ling. Unified from spawn-one-vterm! and
   spawn-one-headless! — route determined by :route parameter."
  [{:keys [task effective-dir default-presets model route spawn-mode-kw ports]}]
  (let [title       (or (:title task) (:id task) "untitled")
        task-id     (:id task)
        agent-name  (str (if (= :claude route) "forja-cl-" "forja-hl-")
                         (java.util.UUID/randomUUID))
        ready-mode  (or spawn-mode-kw (if (= :claude route) :claude :headless))
        base-result {:agent-id agent-name :task-title title :task-id task-id
                     :spawned false :route route}
        r (result/rescue
           base-result
           (let [sw       (spawn-and-wait! {:agent-name      agent-name
                                            :effective-dir   effective-dir
                                            :default-presets default-presets
                                            :model           model
                                            :route           route
                                            :spawn-mode-kw   ready-mode
                                            :task-id         task-id
                                            :task            task
                                            :ports           ports})
                 agent-id (:agent-id sw)]
             (if (:ready? sw)
               (let [dr (dispatch-to-ling! {:agent-id agent-id :task task
                                            :effective-dir effective-dir :ports ports})]
                 (if-let [err (:dispatch-error dr)]
                   (do (log/warn (str "SPARK[" (name route) "]: dispatch failed after spawn")
                                 {:agent agent-id :task title :error err})
                       (assoc base-result :agent-id agent-id :error err))
                   (do (log/info (str "SPARK[" (name route) "]: spawned+dispatched")
                                 {:agent agent-id :task title :model model})
                       (handle-ready-ling {:agent-id agent-id :title title :task-id task-id
                                           :route route :model model
                                           :spawn-mode (:spawn-mode sw)}))))
               (handle-timeout-ling {:spawn-result sw :task task :effective-dir effective-dir
                                     :base-result base-result :title title :task-id task-id
                                     :route route :model model
                                     :spawn-mode (:spawn-mode sw) :ports ports}))))]
    (cond-> r
      (and (not (:spawned r)) (not (:error r)) (::result/error (meta r)))
      (assoc :error (get-in (meta r) [::result/error :message] "unknown")))))

;; ── Spark Helpers ───────────────────────────────────────────────────────────

(defn- count-project-lings
  "Count active lings for the given project, split by route type.
   `agents-fn` answers the slave maps to count."
  [project-id agents-fn]
  (let [active-status #{:active :running :working :idle :spawning}
        project-lings (->> (agents-fn)
                           (filter #(= 1 (:slave/depth %)))
                           (filter #(active-status (:slave/status %)))
                           (filter (fn [a] (if project-id
                                             (= project-id (:slave/project-id a))
                                             true))))
        active-vterm    (count (remove #(headless-modes (:ling/spawn-mode %)) project-lings))
        active-headless (count (filter #(headless-modes (:ling/spawn-mode %)) project-lings))]
    {:active-vterm active-vterm :active-headless active-headless
     :active-total (+ active-vterm active-headless)}))

(defn- compute-route-batches
  "Allocate ling tasks to vterm/headless batches based on mode and slots.
   Returns [vterm-tasks headless-tasks]."
  [{:keys [effective-spawn-mode ling-tasks max-slots active-counts]}]
  (let [{:keys [active-vterm active-total]} active-counts]
    (case effective-spawn-mode
      (:claude :vterm)
      (let [cap   (min (or max-slots vterm-max-slots) vterm-max-slots)
            avail (max 0 (- cap active-total))]
        [(vec (take avail ling-tasks)) []])
      (:headless :agent-sdk :openrouter)
      (let [avail (max 0 (- (or max-slots 10) active-total))]
        [[] (vec (take avail ling-tasks))])
      ;; :mixed — fill vterm, overflow to headless
      (let [total-cap   (or max-slots 10)
            total-avail (max 0 (- total-cap active-total))
            vt-avail    (min (max 0 (- vterm-max-slots active-vterm)) total-avail)
            vt-batch    (vec (take vt-avail ling-tasks))
            hl-avail    (max 0 (- total-avail (count vt-batch)))
            hl-batch    (vec (take hl-avail (drop vt-avail ling-tasks)))]
        [vt-batch hl-batch]))))

(defn- execute-spawn-batches
  "Spawn lings for vterm and headless task batches.
   Headless spawns run in parallel (futures) since they're independent subprocesses.
   Vterm spawns run sequentially to avoid overwhelming Emacs."
  [{:keys [vterm-tasks headless-tasks effective-dir default-presets model
           headless-spawn-mode ports]}]
  (let [;; Headless: spawn all in parallel via futures, then deref
        hl-futures (doall (for [task headless-tasks]
                            (future
                              (spawn-one! {:task task :effective-dir effective-dir
                                           :default-presets default-presets :model model
                                           :route :headless :spawn-mode-kw headless-spawn-mode
                                           :ports ports}))))
        ;; Vterm: sequential to avoid Emacs contention
        vt-results (doall (for [task vterm-tasks]
                            (spawn-one! {:task task :effective-dir effective-dir
                                         :default-presets default-presets :model model
                                         :route :claude :ports ports})))
        ;; Collect headless results with per-future timeout
        ;; Per-future timeout: 2x readiness (60s + 5s retry + 60s) + spawn/dispatch overhead = 140s
        per-future-timeout-ms 140000
        hl-results (mapv (fn [f]
                           (deref f per-future-timeout-ms
                                  {:spawned false :error "Future deref timeout"}))
                         hl-futures)]
    {:vt-results vt-results
     :hl-results hl-results}))

(defn- build-spark-response
  "Merge spawn batch results into unified spark! response."
  [{:keys [vt-results hl-results active-counts max-slots]}]
  (let [all-ling (concat vt-results hl-results)]
    {:spawned       (filterv :spawned all-ling)
     :failed        (filterv (complement :spawned) all-ling)
     :count         (count (filter :spawned all-ling))
     :routes        {:vterm    {:spawned (filterv :spawned vt-results)
                                :count (count (filter :spawned vt-results))
                                :active-before (:active-vterm active-counts)
                                :max-slots vterm-max-slots}
                     :headless {:spawned (filterv :spawned hl-results)
                                :count (count (filter :spawned hl-results))
                                :active-before (:active-headless active-counts)
                                :max-slots (or max-slots 10)}}
     :slots-used    (count (filter :spawned all-ling))
     :active-before (:active-total active-counts)
     :max-slots     (or max-slots 10)}))

(defn- empty-spark-response
  "Empty spark response when no tasks to spawn."
  [active-counts max-slots]
  {:spawned [] :failed [] :count 0
   :routes {:vterm {:count 0 :active-before (:active-vterm active-counts)
                    :max-slots vterm-max-slots}
            :headless {:count 0 :active-before (:active-headless active-counts)
                       :max-slots (or max-slots 10)}}
   :slots-used 0
   :active-before (:active-total active-counts)
   :max-slots (or max-slots 10)})

;; ── Orchestrator Mode ──────────────────────────────────────────────────────

(defn- build-orchestrator-prompt
  "Build the structured prompt for an orchestrator ling.
   Bundles all tasks with their pre-gathered context refs."
  [tasks context-result directory]
  (let [task-lines (mapv (fn [task]
                           (let [ctx    (get context-result (:id task))
                                 summary (or (:summary ctx) "")]
                             (str "### Task: " (or (:title task) (:id task)) "\n"
                                  "ID: " (:id task) "\n"
                                  (when-let [desc (:description task)]
                                    (str "Description: " desc "\n"))
                                  (when (seq summary)
                                    (str "Context:\n" summary "\n"))
                                  (when-let [refs (:ctx-refs ctx)]
                                    (str "Context refs: " (pr-str refs) "\n"))
                                  (when-let [kg-ids (:kg-node-ids ctx)]
                                    (str "KG seeds: " (pr-str (take 5 kg-ids)) "\n")))))
                         tasks)]
    (str "You are the kanban orchestrator for this milestone.\n\n"
         "## Tasks (" (count tasks) " total)\n\n"
         (clojure.string/join "\n" task-lines)
         "\n## Instructions\n"
         "1. For each task above, spawn a Task subagent (max 3 parallel)\n"
         "2. Each subagent implements the task using hive MCP tools\n"
         "3. Collect results, then: kanban batch-update to mark done\n"
         "4. Check kanban status for milestone progress\n"
         "5. Loop if more tasks remain\n\n"
         "Working directory: " (or directory "auto") "\n"
         "NEVER spawn lings or call forge-strike. Use Task subagents ONLY.\n")))

(defn- spawn-orchestrator!
  "Spawn a single orchestrator ling with all tasks bundled.
   The orchestrator uses Task subagents for parallelism. Effects go through `ports`."
  [{:keys [tasks directory model context-result ports]}]
  (let [{:keys [spawn-agent-fn await-ready-fn send-prompt-fn kanban-fn]} (spark-ports ports)
        effective-dir  (or directory (ctx/current-directory) (System/getProperty "user.dir"))
        agent-name     (str "forja-orch-" (System/currentTimeMillis))
        prompt         (build-orchestrator-prompt tasks context-result effective-dir)]
    (log/info "SPARK[orchestrator]: spawning 1 orchestrator for" (count tasks) "tasks")
    (let [spawn-result (spawn-agent-fn
                        {:type    "ling"
                         :name    agent-name
                         :cwd     effective-dir
                         :presets ["orchestrator" "ling" "mcp-first"]
                         :model   model})
          parsed       (parse-spawn-result spawn-result agent-name)
          agent-id     (:agent-id parsed)
          ready-mode   (or (:spawn-mode parsed) :claude)
          ready        (await-ready-fn agent-id ready-mode)]
      (if (or (:ready? ready) (:slave ready))
        (let [dispatch-result (send-prompt-fn
                               {:agent_id agent-id
                                :prompt   prompt})]
          (if (:isError dispatch-result)
            (do
              (log/error "SPARK[orchestrator]: dispatch failed" {:agent agent-id})
              {:spawned [] :failed [{:agent-id agent-id :error "dispatch failed"}]
               :count 0 :mode :orchestrator})
            (do
              (log/info "SPARK[orchestrator]: dispatched" (count tasks) "tasks to" agent-id)
              ;; Mark all tasks as inprogress
              (support/mark-tasks-inprogress! kanban-fn effective-dir tasks {:skip-nil? false})
              {:spawned [{:agent-id agent-id :task-count (count tasks)
                          :mode :orchestrator :spawned true}]
               :failed  []
               :count   1
               :mode    :orchestrator})))
        (do
          (log/error "SPARK[orchestrator]: ling readiness timeout" {:agent agent-id})
          {:spawned [] :failed [{:agent-id agent-id :error "readiness timeout"}]
           :count 0 :mode :orchestrator})))))

;; ── Spark! Orchestrator ─────────────────────────────────────────────────────

(defn spark!
  "Spawn lings for ready tasks.
   Routes: :claude (Emacs), :vterm, :headless/:agent-sdk/:openrouter,
   :orchestrator (single ling + Task subagents), :mixed (default: fill vterm slots,
   overflow to headless).
   A task carrying per-task execution goes to a ling in every ling mode, in task order.
   :orchestrator cannot honor it: such a task is not dispatched and is reported in
   :failed as :execution/unsupported-mode while the other tasks proceed; when every
   task is rejected, nothing is spawned.
   Ling spawn, readiness, dispatch, in-progress marking, agent listing and project
   resolution go through `ports` (keys of default-spark-ports); the 1-arity uses the defaults.
   The :dispatch-fn, :wait-ready-fn and :update-fn keys of `opts` are not ports and are ignored."
  ([opts] (spark! opts {}))
  ([{:keys [directory max_slots presets tasks spawn_mode spawn-mode model context-result]}
    ports]
   (let [ports                (spark-ports ports)
         model                (budget-route-model model)
         effective-spawn-mode (keyword (or spawn_mode spawn-mode :mixed))
         _ (when (= :drone effective-spawn-mode)
             (throw (ex-info "Drone spawn mode was removed: lings are the forge execution unit"
                             {:type :execution/unsupported-mode
                              :spawn-mode effective-spawn-mode
                              :fix "Use spawn_mode mixed, claude, vterm or a headless mode"})))]
     (if (= :orchestrator effective-spawn-mode)
       (let [{:keys [accepted rejected]} (routing/reject-execution-routed :orchestrator tasks)]
         (update (if (and (seq rejected) (empty? accepted))
                   {:spawned [] :failed [] :count 0 :mode :orchestrator}
                   (spawn-orchestrator! {:tasks accepted :directory directory :model model
                                         :context-result context-result :ports ports}))
                 :failed (fnil into []) rejected))
       (let [ling-tasks      (vec tasks)
             effective-dir   (or directory (ctx/current-directory) (System/getProperty "user.dir"))
             project-id      (when effective-dir ((:project-id-fn ports) effective-dir))
             active-counts   (count-project-lings project-id (:agents-fn ports))
             default-presets (or presets ["ling" "mcp-first" "saa"])

             [vterm-tasks headless-tasks]
             (compute-route-batches {:effective-spawn-mode effective-spawn-mode
                                     :ling-tasks ling-tasks :max-slots max_slots
                                     :active-counts active-counts})

             headless-spawn-mode (if (headless-modes effective-spawn-mode)
                                   effective-spawn-mode :headless)]
         (if (and (empty? vterm-tasks) (empty? headless-tasks))
           (empty-spark-response active-counts max_slots)
           (let [batch-results (execute-spawn-batches
                                {:vterm-tasks vterm-tasks :headless-tasks headless-tasks
                                 :effective-dir effective-dir :default-presets default-presets
                                 :model model :headless-spawn-mode headless-spawn-mode
                                 :ports ports})]
             (build-spark-response (assoc batch-results
                                          :active-counts active-counts
                                          :max-slots max_slots)))))))))
