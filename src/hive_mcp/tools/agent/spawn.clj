(ns hive-mcp.tools.agent.spawn
  "Agent spawn handler for creating new ling agents.

   Includes defense-in-depth guard: child lings (spawned agents) are
   denied from spawning further agents to prevent recursive self-call
   chains (Ling→agent.spawn→Ling→agent.spawn→∞)."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.ling.lifecycle :as lifecycle]
            [hive-mcp.agent.type-registry :as agent-type-registry]
            [hive-mcp.agent.spawn-mode-registry :as spawn-registry]
            [hive-mcp.agent.openrouter :as llm-registry]
            [hive-mcp.agent.provider.preflight :as provider-preflight]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-spi.swarm.guards :as guards]
            [hive-mcp.config.core :as config]
            [hive-mcp.schema.tools :as schema]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [hive-mcp.channel.audience :as audience]
            [hive-mcp.agent.ling.headless-registry :as headless-registry]
            [hive-mcp.emacs.client :as emacs-client]
            [hive-mcp.agent.grant :as grant]))
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

(defn sandbox-decision
  "Pure reading of the optional `sandbox` spawn param: nil (host default),
   true, false, or :refused. Fails CLOSED: only a boolean or the strings
   \"true\"/\"false\" are accepted. Anything else, a backend name such as
   \"darkmatter\" or a map naming one, is :refused rather than read as false,
   which would run the ling with no sandbox at all, or as true, which would
   silently swap the requested backend for bwrap. Per-spawn backends are card
   20261006212408-519d3e2b."
  [v]
  (cond
    (nil? v)      nil
    (boolean? v)  v
    (= "true" v)  true
    (= "false" v) false
    :else         :refused))

(defn normalize-sandbox
  "`sandbox-decision` of V, throwing on :refused so the spawn is refused."
  [v]
  (let [d (sandbox-decision v)]
    (if (= :refused d)
      (throw (ex-info (str "sandbox must be true or false; got " (pr-str v)
                           ". A per-spawn sandbox backend (e.g. darkmatter) is not"
                           " supported yet, so the spawn is refused rather than run"
                           " unsandboxed.")
                      {:param "sandbox" :value v}))
      d)))

(defn- normalize-token-budget
  "Normalize a positive context-reconstruction budget from MCP JSON."
  [v]
  (when (some? v)
    (let [n (if (string? v) (parse-long v) v)]
      (if (and (integer? n) (pos? n))
        (long n)
        (throw (ex-info "token_budget must be a positive integer"
                        {:param "token_budget" :value v}))))))

(def ^:private coerce-loop-params
  "Raw MCP llm_retries/resume/chat_run_id -> valid SpawnLoopParams, or ex-info."
  (schema/param-coercer "llm_retries/resume/chat_run_id" schema/SpawnLoopParams))

(defn- resume->backend
  "Pure: a valid ResumeParam as the kebab map the headless backend reads."
  [{:keys [run_id at prompt]}]
  (cond-> {:run-id run_id}
    at     (assoc :at at)
    prompt (assoc :prompt prompt)))

(defn turn-budget-normalizer
  "The turn-budget port: hive-agent's turn_budget normalizer, soft-resolved so
   core requires no addon statically and grows no namespace under the frozen
   hive-agent extraction. nil when hive-agent is not on the classpath."
  []
  (try (some-> (requiring-resolve 'hive-agent.swarm.wave-params/normalize-turn-budget) deref)
       (catch Throwable _ nil)))

(defn turn-budget-opt
  "Pure: the lease spec for a raw turn_budget V through NORMALIZE (the port).
   nil V -> nil (the backend's defaults). A V with no NORMALIZE (hive-agent
   absent) throws ex-info naming turn_budget, so a lease is never dropped
   silently."
  [normalize v]
  (when (some? v)
    (if normalize
      (normalize v)
      (throw (ex-info "turn_budget needs the hive-agent addon, which is not loaded"
                      {:param "turn_budget" :value v})))))

(defn loop-opts
  "Validate the loop params of a spawn request once, by SpawnLoopParams, and
   return them as ling opts: {:llm-retries n :resume {...} :chat-run-id s
   :turn-budget {...}}, absent keys omitted. A malformed value throws ex-info
   with the humanized errors.

   :turn-budget is the lease spec (hive-agent.swarm.wave-params/normalize-turn-budget,
   reached through the turn-budget port, see turn-budget-normalizer):
   kebab keys, :judge a keyword. It rides the ling ctx to the headless
   backend, which reads it as hive-agent.loop.spawn/build-spawn-config's
   :turn-budget.

   NORMALIZE is that port, a fn raw-turn_budget -> lease spec; the 1-arity
   resolves it from the hive-agent addon."
  ([params] (loop-opts params (turn-budget-normalizer)))
  ([params normalize]
   (let [{:keys [llm_retries resume chat_run_id]}
         (coerce-loop-params (select-keys params [:llm_retries :resume :chat_run_id]))
         turn-budget (turn-budget-opt normalize (:turn_budget params))]
     (cond-> {}
       llm_retries (assoc :llm-retries llm_retries)
       resume      (assoc :resume (resume->backend resume))
       chat_run_id (assoc :chat-run-id chat_run_id)
       turn-budget (assoc :turn-budget turn-budget)))))

(defn normalize-resume
  "The MCP `resume` object as the kebab map the headless backend reads:
   {:run-id str :at {:seq n}|{:turn t}|absent :prompt str?}. nil in, nil out."
  [v]
  (:resume (loop-opts {:resume v})))

(defn chat-point-refusal
  "Pure: an error message when OPTS carry chat-point params that the
   resolved spawn MODE would silently drop, else nil. CAPABILITIES is the
   set MODE's backend declared at registration; only :chat-points reads them."
  [mode opts capabilities]
  (when-let [given (seq (filter #(contains? opts %) [:resume :chat-run-id]))]
    (when-not (contains? capabilities :chat-points)
      (str (str/join " and " (map {:resume "resume" :chat-run-id "chat_run_id"} given))
           " needs a chat-point capable headless backend; this spawn resolved to "
           (pr-str mode) ", which would drop them."))))

(defn provider-preflight-refusal
  "An error map when the resolved spawn MODE would call PROVIDER with a
   credential that cannot work (secret missing, key rejected, credit
   exhausted), else nil. Only a registered headless backend calls the provider
   through hive's configured key; a terminal mode runs its own CLI and is not
   checked. The provider checks themselves live in
   `hive-mcp.agent.provider.preflight` and read the open provider registry."
  [mode provider]
  (when (and provider (contains? (headless-registry/registered-headless) mode))
    (provider-preflight/refusal provider)))

(def ^:dynamic *editor-reachable?*
  "0-arg port: true when an Emacs daemon answers. Read per spawn."
  (fn [] (boolean (emacs-client/emacs-running?))))

(defn editor-preflight-refusal
  "Message refusing MODE when it needs Emacs and REACHABLE? answers false, else nil.
   REACHABLE? is only called for Emacs-bound modes."
  [mode reachable?]
  (when (and (spawn-registry/requires-emacs? mode) (not (reachable?)))
    (str "spawn_mode " (clojure.core/name mode) " needs a running Emacs daemon, "
         "and none answered. Start Emacs (emacs --daemon) or retry with "
         "spawn_mode=\"headless\", which needs no Emacs.")))

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

(def ^:dynamic *grant-domain*
  "0-arg port: the grant domain fns (hive-mcp.agent.grant/domain), or nil
   when the hive-agent addon is not loaded. Rebound by tests."
  grant/domain)

(def ^:dynamic *get-slave*
  "1-arg port: registry row for a slave id. Rebound by tests."
  (fn [id] (queries/get-slave id)))

(defn spawn-grant
  "The grant decision for a spawn by `parent` asking for `requested` (the
   raw `grant` param, nil = share). {:grant wire-or-nil} or {:refused msg}.
   See hive-mcp.agent.grant/child-grant. With no grant recorded on the
   parent's lineage and none requested, nothing changes: {:grant nil}.

   `requested` is first read by `grant/requested-grant`: a JSON-object
   string is parsed (a client whose cached schema predates the parameter
   sends it as text), any other non-map is refused by message. It used to
   reach `attenuate` as is and die on a ClassCastException."
  [parent requested]
  (let [{requested :ok shape-refusal :refused} (grant/requested-grant requested)]
    (if shape-refusal
      {:refused shape-refusal}
      (let [get-slave *get-slave*
            parent-wire (grant/recorded-grant get-slave parent)
            child-depth (inc (long (grant/depth-of get-slave parent)))]
        (grant/child-grant (when (or parent-wire requested) (*grant-domain*))
                           parent-wire requested child-depth)))))

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
  [{:keys [type name cwd presets model provider tier token_budget project_id kanban_task_id spawn_mode agents max_budget_usd kg_compress sliding_window_size verbose sandbox] :as params}]
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
                {child-grant :grant grant-refusal :refused} (spawn-grant parent (:grant params))
                _ (when grant-refusal
                    (throw (ex-info grant-refusal {:grant/refused true})))
                worker-tier (normalize-tier tier)
                token-budget (normalize-token-budget token_budget)
                loop-params (loop-opts params)
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
                    effective-spawn-mode (keyword (or spawn_mode (lifecycle/default-spawn-mode)))
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
                                                       (seq loop-params) (merge loop-params)
                                                       token-budget      (assoc :token-budget token-budget)
                                                       sliding_window_size (assoc :sliding-window-size sliding_window_size)
                                                       (some? sandbox)   (assoc :sandbox (normalize-sandbox sandbox))))
                    _ (when-let [refusal (chat-point-refusal (:spawn-mode ling-agent) loop-params
                                                             (headless-registry/headless-capabilities (:spawn-mode ling-agent)))]
                        (throw (ex-info refusal {:spawn-mode (:spawn-mode ling-agent)})))
                    _ (when-let [refusal (editor-preflight-refusal (:spawn-mode ling-agent) *editor-reachable?*)]
                        (throw (ex-info refusal {:spawn-mode (:spawn-mode ling-agent)
                                                 :fallback "headless"})))
                    _ (when-let [err (provider-preflight-refusal (:spawn-mode ling-agent) effective-provider)]
                        (throw (ex-info (str "Provider " (clojure.core/name effective-provider)
                                             " cannot serve this spawn: " (:fix err))
                                        err)))
                    slave-id (proto/spawn! ling-agent (cond-> {:task brief
                                                               :parent parent
                                                               :kanban-task-id kanban_task_id
                                                               :spawn-mode (:spawn-mode ling-agent)
                                                               :model effective-model
                                                               :provider effective-provider
                                                               :spawn/request params}
                                                        max_budget_usd (assoc :max-budget-usd max_budget_usd)
                                                        child-grant    (assoc :grant child-grant)))]
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
                            child-grant
                            (assoc :grant child-grant)
                            (nil? brief)
                            (assoc :warning (str "Spawned with no task: the ling has no brief "
                                                 "and will idle until `agent dispatch` sends one.")))))))
          (catch Exception e
            (log/error "Failed to spawn agent" {:type agent-type :error (ex-message e)})
            (mcp-error (str "Failed to spawn " (clojure.core/name agent-type) ": " (ex-message e))))))))))
