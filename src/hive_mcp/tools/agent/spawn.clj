(ns hive-mcp.tools.agent.spawn
  "Agent spawn handler for creating new ling agents.

   Includes defense-in-depth guard: child lings (spawned agents) are
   denied from spawning further agents to prevent recursive self-call
   chains (Ling→agent.spawn→Ling→agent.spawn→∞)."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.type-registry :as agent-type-registry]
            [hive-mcp.agent.spawn-mode-registry :as spawn-registry]
            [hive-mcp.agent.openrouter :as llm-registry]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-spi.swarm.guards :as guards]
            [hive-mcp.config.core :as config]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [hive-mcp.channel.audience :as audience]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- resolve-project-scope
  "Resolve effective project-id for a spawned agent via hierarchy."
  [project_id cwd parent]
  (or project_id
      (when cwd
        (let [inferred (kg-scope/infer-scope-from-path cwd)]
          (when (and inferred (not= inferred "global"))
            inferred)))
      (when parent
        (when-let [parent-data (queries/get-slave parent)]
          (:slave/project-id parent-data)))
      (when cwd
        (last (str/split cwd #"/")))))

;;; =============================================================================
;;; Spawn Guard (Defense-in-Depth — Layer 3)
;;; =============================================================================

(defn- build-spawn-denied-message
  "Build dynamic spawn denial message with current depth info."
  []
  (str "SPAWN DENIED: Child lings cannot spawn agents.\n\n"
       "You are running as a child ling (HIVE_MCP_ROLE=child-ling, depth="
       (guards/ling-depth) ").\n"
       "Agent spawning is restricted to the coordinator to prevent recursive\n"
       "self-call chains (Ling→spawn→Ling→spawn→∞).\n\n"
       "If you need parallel work, use hivemind_shout to request the coordinator\n"
       "to spawn agents on your behalf."))


(defn- heap-pressure-defer
  "Layer 4 OOM backpressure: best-effort heap-headroom admission check.
   Returns a {:level :heap-pct} map when a new spawn should be DEFERRED
   (JVM heap fraction >= the soft watermark), or nil to ADMIT.

   Lings launch inside (or alongside) this nREPL JVM; N concurrent
   heavy spawns atop the multi-GB KG floor have driven kernel OOMs. We shed
   *new* spawns under pressure rather than hard-kill live agents.

   Reuses the self-contained hive-cache.mem-guard governor, lazily resolved —
   hive-mcp does NOT statically depend on hive-cache. FAIL-OPEN: a missing
   governor, any sampling error, or config opt-out all ADMIT (return nil), so
   the guard can never wedge the spawn path.

   Config — note hive-mcp.config.resolve/get-service-value uses (or val default)
   which swallows boolean false, so the kill switch is a default-FALSE *disable*
   flag, not a default-true enable flag:
     [:swarm :heap-admission-disabled] default false; set true (or env
       HIVE_MCP_SWARM_HEAP_ADMISSION_DISABLED=true) to force the gate off.
     [:swarm :heap-admission-soft]      0.0-1.0 heap fraction; nil = mem-guard
       default 0.80 (env HIVE_MCP_SWARM_HEAP_ADMISSION_SOFT)."
  []
  (try
    (when-not (config/get-service-value :swarm :heap-admission-disabled
                                        :env "HIVE_MCP_SWARM_HEAP_ADMISSION_DISABLED"
                                        :parse #(Boolean/parseBoolean %)
                                        :default false)
      (when-let [check (requiring-resolve 'hive-cache.mem-guard/check)]
        (let [soft (config/get-service-value :swarm :heap-admission-soft
                                             :env "HIVE_MCP_SWARM_HEAP_ADMISSION_SOFT"
                                             :parse parse-double
                                             :default nil)
              wm   (when (number? soft) {:soft soft})
              {:keys [level heap-pct]} (check nil wm)]
          (when (contains? #{:soft :hard} level)
            {:level level :heap-pct heap-pct}))))
    (catch Throwable _ nil)))

;;; =============================================================================
;;; Spawn Handler
;;; =============================================================================

(defn effective-parent
  "The parent a spawn is attributed to. An explicit non-blank `parent` wins.
   Otherwise the calling agent (`:_caller_id`, stamped on every MCP request by
   the transport) is the parent. A coordinator-lane caller counts only when
   it names a SESSION (`coordinator:<session>`), so the spawn's shouts reach
   that one window; a lane spelled without a session (`coordinator`,
   `coordinator-hive`) leaves the spawn root-level, the lane the audience
   layer routes to every coordinator reader."
  [{:keys [parent _caller_id]}]
  (let [explicit (when-not (str/blank? (str parent)) parent)
        caller   (when-not (str/blank? (str _caller_id)) (str _caller_id))]
    (or explicit
        (when caller
          (if (audience/coordinator-reader? caller)
            (when (audience/coordinator-session caller) caller)
            caller)))))

(defn- normalize-tier
  "Normalize the optional economy role. cheap delegates model selection to the
   configured ling default; frontier permits an explicit model."
  [v]
  (when (some? v)
    (let [tier (if (keyword? v) v (keyword (str v)))]
      (if (contains? #{:cheap :frontier} tier)
        tier
        (throw (ex-info "tier must be cheap or frontier"
                        {:param "tier" :value v}))))))

(defn- normalize-token-budget
  "Normalize a positive context-reconstruction budget from MCP JSON."
  [v]
  (when (some? v)
    (let [n (if (string? v) (parse-long v) v)]
      (if (and (integer? n) (pos? n))
        (long n)
        (throw (ex-info "token_budget must be a positive integer"
                        {:param "token_budget" :value v}))))))

(defn spawn-brief
  "The initial task a spawn carries: `task`, else `prompt`. Blank counts as
   absent. Throws ex-info when both are given and differ."
  [{:keys [task prompt]}]
  (let [t (when-not (str/blank? task) task)
        p (when-not (str/blank? prompt) prompt)]
    (if (and t p (not= t p))
      (throw (ex-info "task and prompt disagree: pass one initial brief"
                      {:param "prompt"}))
      (or t p))))

(defn handle-spawn
  "Spawn a new ling agent.

   Defense-in-depth: denies spawn when called from a child ling process
   (HIVE_MCP_ROLE=child-ling). This prevents recursive agent spawning.

   The spawn's parent is `effective-parent`: the `parent` param when given,
   else the calling agent — so grandchild routing needs no priming.

   The initial brief is `spawn-brief`: `task`, else `prompt`. The response
   reports `:task-attached` and carries a `:warning` when the ling starts
   with no brief.

   The full request map rides on opts under :spawn/request for the
   :spawn/opts-overlay extension seam, and is stripped before planning."
  [{:keys [type name cwd presets model provider tier token_budget project_id kanban_task_id spawn_mode agents max_budget_usd kg_compress sliding_window_size verbose llm_retries] :as params}]
  ;; Layer 3: Defense-in-depth spawn guard
  (if-let [_ (when (guards/child-ling?) :denied)]
    (do
      (log/warn "Spawn denied: child ling attempted agent spawn"
                {:role (guards/get-role) :depth (guards/ling-depth)})
      (mcp-error (build-spawn-denied-message)))
    (if-let [defer (heap-pressure-defer)]
      ;; Layer 4: heap-headroom backpressure — defer rather than launch.
      (do
        (log/warn "Spawn deferred: heap pressure backpressure" defer)
        (mcp-json {:success  false
                   :deferred true
                   :reason   "heap-pressure"
                   :level    (clojure.core/name (:level defer))
                   :heap-pct (:heap-pct defer)
                   :message  (str "Spawn deferred: JVM heap at " (:heap-pct defer)
                                  "% (>= soft watermark). Best-effort OOM "
                                  "backpressure — existing agents keep running; "
                                  "retry once active lings drain.")}))
      (let [agent-type (keyword type)]
      (if-not (and (agent-type-registry/valid-type? agent-type)
                   (agent-type-registry/spawnable? agent-type))
        (mcp-error (str "type must be one of: " (pr-str (agent-type-registry/mcp-enum))))
        (try
          ;; Resolve provider+model via registry chain
          (let [parent (effective-parent params)
                worker-tier (normalize-tier tier)
                token-budget (normalize-token-budget token_budget)
                brief (spawn-brief params)
                resolved (llm-registry/resolve-provider-model
                           {:provider provider
                            :model (if (= :cheap worker-tier) nil model)
                            :agent-type agent-type})
                effective-model (:model resolved)
                effective-provider (:provider resolved)
                agent-id (or name (helpers/generate-agent-id agent-type))
                effective-project-id (resolve-project-scope project_id cwd parent)]
            (case agent-type
              :ling
              (let [presets-vec (cond
                                  (nil? presets) []
                                  (string? presets) [presets]
                                  (sequential? presets) (vec presets)
                                  :else [presets])
                    effective-spawn-mode (keyword (or spawn_mode "claude"))
                    _ (when-not (spawn-registry/valid-mode? effective-spawn-mode)
                        (throw (ex-info (str "spawn_mode must be one of: " (pr-str spawn-registry/mcp-modes))
                                        {:spawn-mode spawn_mode})))
                    normalized-agents (when (map? agents)
                                        (reduce-kv
                                         (fn [m agent-name agent-spec]
                                           (assoc m (clojure.core/name agent-name)
                                                  (if (map? agent-spec)
                                                    (reduce-kv (fn [m2 k v]
                                                                 (assoc m2 (keyword k) v))
                                                               {} agent-spec)
                                                    agent-spec)))
                                         {} agents))
                    ling-agent (ling/->ling agent-id (cond-> {:cwd cwd
                                                              :presets presets-vec
                                                              :project-id effective-project-id
                                                              :spawn-mode effective-spawn-mode
                                                              :model effective-model
                                                              :provider effective-provider}
                                                       normalized-agents (assoc :agents normalized-agents)
                                                       max_budget_usd    (assoc :max-budget-usd max_budget_usd)
                                                       (some? kg_compress) (assoc :kg-compress? kg_compress)
                                                       (some? verbose)   (assoc :verbose? (if (string? verbose)
                                                                                            (= "true" verbose)
                                                                                            (boolean verbose)))
                                                       llm_retries       (assoc :llm-retries (if (string? llm_retries)
                                                                                               (parse-long llm_retries)
                                                                                               llm_retries))
                                                       token-budget      (assoc :token-budget token-budget)
                                                       sliding_window_size (assoc :sliding-window-size sliding_window_size)))
                    slave-id (proto/spawn! ling-agent (cond-> {:task brief
                                                               :parent parent
                                                               :kanban-task-id kanban_task_id
                                                               :spawn-mode (:spawn-mode ling-agent)
                                                               :model effective-model
                                                               :provider effective-provider
                                                               :spawn/request params}
                                                        max_budget_usd (assoc :max-budget-usd max_budget_usd)))]
                (log/info "Spawned ling" {:requested-id agent-id
                                          :slave-id slave-id
                                          :parent parent
                                          :spawn-mode (:spawn-mode ling-agent)
                                          :provider effective-provider
                                          :model effective-model
                                          :tier worker-tier
                                          :token-budget token-budget
                                          :task-attached (some? brief)
                                          :cwd cwd :presets presets-vec
                                          :project-id effective-project-id})
                (mcp-json (cond-> {:success true
                                   :agent-id slave-id
                                   :type :ling
                                   :parent parent
                                   :spawn-mode (:spawn-mode ling-agent)
                                   :provider effective-provider
                                   :model effective-model
                                   :tier worker-tier
                                   :token-budget token-budget
                                   :task-attached (some? brief)
                                   :cwd cwd
                                   :presets presets-vec
                                   :project-id effective-project-id}
                            (nil? brief)
                            (assoc :warning (str "Spawned with no task: the ling has no brief "
                                                 "and will idle until `agent dispatch` sends one.")))))))
          (catch Exception e
            (log/error "Failed to spawn agent" {:type agent-type :error (ex-message e)})
            (mcp-error (str "Failed to spawn " (clojure.core/name agent-type) ": " (ex-message e))))))))))
