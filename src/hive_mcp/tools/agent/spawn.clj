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
            [hive-mcp.transport.spawn :as spawn-chain]
            [hive-mcp.agent.type-registry :as agent-type-registry]
            [hive-mcp.agent.spawn-mode-registry :as spawn-registry]
            [hive-mcp.agent.openrouter :as llm-registry]
            [hive-mcp.agent.provider.preflight :as provider-preflight]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-spi.swarm.guards :as guards]
            [hive-mcp.schema.tools :as schema]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [hive-mcp.channel.audience :as audience]
            [hive-mcp.agent.ling.headless-registry :as headless-registry]
            [hive-mcp.emacs-ext.client :as emacs-client]
            [hive-mcp.agent.grant :as grant]
            [malli.core :as m]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.context.request :as request-ctx]
            [hive-mcp.agent.ling.spawn :as spawn-impl]
            [hive-mcp.system.heap-admission :as heap-admission]
            [hive-mcp.extensions.registry :as ext]))
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

(defn- heap-refusal
  "Layer 4: the spawn refusal for a hard-pressure `reading`
   (`hive-mcp.system.heap-admission/hard-pressure`). Nothing is queued and
   the brief is dropped, so it says so, with the measured figures and when to
   retry."
  [{:keys [level basis heap-pct now-pct post-gc-pct pool watermarks]}]
  (let [hard-pct (some-> (:hard watermarks) double (* 100.0) Math/round int)
        retry-s  heap-admission/retry-after-s]
    {:success            false
     :refused            true
     :reason             "heap-pressure"
     :level              (some-> level clojure.core/name)
     :basis              (clojure.core/name (or basis :instant))
     :heap_post_gc_pct   post-gc-pct
     :heap_now_pct       now-pct
     :hard_watermark_pct hard-pct
     :retry_after_s      retry-s
     :brief_kept         false
     :message
     (str "Spawn refused: heap pressure. "
          (if (= :post-gc basis)
            (str "The JVM heap is " heap-pct "% full after its last old-gen collection"
                 (when pool (str " (" pool ")")) ", at or above the hard watermark of "
                 hard-pct "% (" now-pct "% in use right now, counting uncollected garbage). ")
            (str "The JVM heap is " heap-pct "% in use, at or above the hard watermark of "
                 hard-pct "%; no post-GC reading is available here, so this figure "
                 "counts uncollected garbage. "))
          "Nothing was queued and the brief was NOT kept: resend the full task text. "
          "Retry after " retry-s "s. If the post-GC figure stays at or above "
          hard-pct "%, the live set is too large for -Xmx: raise -Xmx or stop idle lings.")}))

;;; =============================================================================
;;; Spawn Handler
;;; =============================================================================

(defn caller-parent
  "The parent a spawn by `caller-id` is attributed to when no `parent` is
   named. A coordinator-lane caller counts only when it names a SESSION
   (`coordinator:<session>`), so the spawn's shouts reach that one window; a
   lane spelled without a session (`coordinator`, `coordinator-hive`) leaves
   the spawn root-level, the lane the audience layer routes to every
   coordinator reader. Blank or nil is root-level. The reading is
   `hive-mcp.agent.ling.spawn/caller-lineage`, shared with the spawn plan."
  [caller-id]
  (spawn-impl/caller-lineage caller-id))

(defn effective-parent
  "The parent a spawn is attributed to. An explicit non-blank `parent` wins.
   Otherwise the calling agent (`:_caller_id`, stamped on every MCP request by
   the transport) is the parent, read by `caller-parent`.

   This is routing (whose window the spawn's shouts reach), not trust: an
   explicit `parent` may name any agent, so nothing that grants membership
   reads it. See `spawner`."
  [{:keys [parent _caller_id]}]
  (let [explicit (when-not (str/blank? (str parent)) parent)]
    (or explicit (caller-parent _caller_id))))

(defn spawner
  "Who actually asked for this spawn: the request's caller, never the
   `parent` param. The verified caller id of the request in flight wins,
   then the transport-stamped `:_caller_id`, then the request context's
   asserted caller, each read by `caller-parent`. The empty string when the
   caller is unknown or root-level.

   It is recorded as :slave/spawned-by, and enclave membership (hive-cloak's
   lineage) follows it instead of :slave/parent, so naming a member session as
   `parent` inherits nothing."
  [{:keys [_caller_id]}]
  (let [present (fn [s] (when-not (str/blank? (str s)) (str s)))
        caller  (or (present (request-ctx/current-verified-caller-id))
                    (present _caller_id)
                    (present (request-ctx/current-caller-id)))]
    (or (caller-parent caller) "")))

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

(defn time-budget-opt
  "Pure: a raw time_budget_ms V as the run's wall-clock budget in ms. An
   integer or a string of one; positive -> that long. nil, zero or negative
   -> nil (no deadline, as hive-agent.loop.time-budget reads them). Anything
   else throws ex-info naming time_budget_ms."
  [v]
  (when (some? v)
    (let [n (cond
              (integer? v) v
              (string? v)  (parse-long (str/trim v))
              :else        nil)]
      (when-not (integer? n)
        (throw (ex-info "time_budget_ms must be an integer number of milliseconds"
                        {:param "time_budget_ms" :value v})))
      (when (pos? n) (long n)))))

(defn loop-opts
  "Validate the loop params of a spawn request once, by SpawnLoopParams, and
   return them as ling opts: {:llm-retries n :resume {...} :chat-run-id s
   :turn-budget {...} :time-budget-ms n}, absent keys omitted. A malformed
   value throws ex-info with the humanized errors.

   :time-budget-ms is the run's wall-clock budget (see time-budget-opt); it
   rides the ling ctx to the headless backend the way :turn-budget does.

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
         turn-budget (turn-budget-opt normalize (:turn_budget params))
         time-budget (time-budget-opt (:time_budget_ms params))]
     (cond-> {}
       llm_retries (assoc :llm-retries llm_retries)
       resume      (assoc :resume (resume->backend resume))
       chat_run_id (assoc :chat-run-id chat_run_id)
       turn-budget (assoc :turn-budget turn-budget)
       time-budget (assoc :time-budget-ms time-budget)))))

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

(defn resolve-spawn-mode
  "The spawn mode of a request and how it was chosen. An explicit `requested`
   mode stays strict: it is returned as is, with no fallback, so the preflight
   refuses an unusable one by name. With none, the :spawn chain
   (config.edn [:transports :spawn :chain], shipped [:headless :tmux :vterm])
   picks the first mode that can serve: one needing Emacs is skipped, with its
   reason, while Emacs is down. REACHABLE? is the Emacs port.
   Answers {:mode kw :resolved-by :explicit|:primary|:fallback :skipped [..]}.
   Throws ex-info naming every skipped mode when none can serve."
  [requested reachable?]
  (if requested
    {:mode (keyword requested) :resolved-by :explicit :skipped []}
    (let [out (spawn-chain/resolve-mode! (spawn-chain/default-env reachable?))]
      (if-let [entry (find out :ok)]
        (let [{:keys [via resolved-by skipped]} (val entry)]
          {:mode via :resolved-by resolved-by :skipped skipped})
        (throw (ex-info (str "no spawn mode can serve: "
                             (str/join "; " (map (fn [{:keys [adapter reason error]}]
                                                   (str (clojure.core/name adapter) " "
                                                        (clojure.core/name reason)
                                                        (when-let [msg (:message error)]
                                                          (str " (" msg ")"))))
                                                 (:skipped out))))
                        {:skipped (:skipped out)}))))))

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
   Either answer also carries :depth, the lineage depth the child sits at
   (parent depth + 1; a coordinator is 0), which the registry row records.

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
        (assoc (grant/child-grant (when (or parent-wire requested) (*grant-domain*))
                                  parent-wire requested child-depth)
               :depth child-depth)))))

(defn kanban-claim-refusal
  "Claim card `kanban-task-id` for the spawner before the ling starts, and
   answer the refusal text when another live owner holds it, else nil. The
   claim owner is the spawn's `parent` lineage: the explicit parent when
   given, else the caller (`effective-parent`), so the coordinator that spawns
   holds the card and its ling works under that claim. `steal_claim` true
   takes the card over. No card, no owner, or a claim that cannot be checked
   (store down, unknown card) never blocks a spawn."
  [{:keys [kanban_task_id steal_claim] :as params}]
  (when-not (str/blank? (str kanban_task_id))
    (when-let [owner (effective-parent params)]
      (let [res (try ((requiring-resolve 'hive-mcp.tools.kanban.events/dispatch-claim!)
                      {:task-id kanban_task_id :owner owner
                       :steal-claim? (contains? #{true "true"} steal_claim)})
                     (catch Throwable e
                       (log/debug "kanban claim on spawn failed (non-fatal):" (ex-message e))
                       nil))]
        (when (= :kanban/claim-refused (:error res))
          (:message res))))))

(m/=> kanban-claim-refusal [:=> [:cat [:maybe :map]] [:maybe :string]])

(defn- spawn-request-context
  "Collect the request-derived values of a spawn, in their original read order."
  [{:keys [type name cwd presets model provider tier token_budget project_id] :as params}]
  (let [agent-type (keyword type)
        parent (effective-parent params)
        {child-grant :grant grant-refusal :refused child-depth :depth}
        (spawn-grant parent (:grant params))]
    (when grant-refusal
      (throw (ex-info grant-refusal {:grant/refused true})))
    (let [worker-tier (normalize-tier tier)
          token-budget (normalize-token-budget token_budget)
          loop-params (loop-opts params)
          brief (or (spawn-brief params)
                    (when (:resume loop-params)
                      "Continue the resumed run from where it stopped."))
          resolved (llm-registry/resolve-provider-model
                    {:provider provider
                     :model (if (= :cheap worker-tier) nil model)
                     :agent-type agent-type})
          seed-model (:model resolved)
          seed-provider (:provider resolved)
          agent-id (or name (helpers/generate-agent-id agent-type))
          effective-project-id (resolve-project-scope project_id cwd parent)]
      {:params params :agent-type agent-type :parent parent
       :child-grant child-grant :child-depth child-depth :worker-tier worker-tier
       :token-budget token-budget :loop-params loop-params :brief brief
       :seed-model seed-model :seed-provider seed-provider :agent-id agent-id
       :presets presets :project-id effective-project-id})))

(defn- normalize-preset-names
  [presets]
  (cond
    (nil? presets) []
    (string? presets) [presets]
    (sequential? presets) (vec presets)
    :else [presets]))

(defn- normalize-agent-definitions
  [agents]
  (when (map? agents)
    (reduce-kv
     (fn [m agent-name agent-spec]
       (assoc m (clojure.core/name agent-name)
              (if (map? agent-spec)
                (reduce-kv (fn [m2 k v] (assoc m2 (keyword k) v)) {} agent-spec)
                agent-spec)))
     {} agents)))

(defn- normalize-verbosity
  [verbose]
  (if (string? verbose) (= "true" verbose) (boolean verbose)))

(defn- choose-spawn-mode!
  "Resolve and validate the requested spawn mode, retaining chain logging."
  [requested]
  (let [chosen-mode (resolve-spawn-mode requested *editor-reachable?*)
        effective-spawn-mode (:mode chosen-mode)]
    (when (seq (:skipped chosen-mode))
      (log/info "Spawn mode chosen through the :spawn chain"
                (select-keys chosen-mode [:mode :resolved-by :skipped])))
    (when-not (spawn-registry/valid-mode? effective-spawn-mode)
      (throw (ex-info (str "spawn_mode must be one of: " (pr-str spawn-registry/mcp-modes))
                      {:spawn-mode requested})))
    effective-spawn-mode))

(defn- ling-runtime-options
  [opts {:keys [params loop-params token-budget]}]
  (let [{:keys [max_budget_usd kg_compress verbose sliding_window_size]} params]
    (cond-> opts
      max_budget_usd (assoc :max-budget-usd max_budget_usd)
      (some? kg_compress) (assoc :kg-compress? kg_compress)
      (some? verbose) (assoc :verbose? (normalize-verbosity verbose))
      (seq loop-params) (merge loop-params)
      token-budget (assoc :token-budget token-budget)
      sliding_window_size (assoc :sliding-window-size sliding_window_size))))

(defn- ling-construction-options
  [{:keys [params seed-model seed-provider] :as context}
   {:keys [mode presets agents]}]
  (let [{:keys [cwd project-id]} context
        base (cond-> {:cwd cwd
                      :presets presets
                      :project-id project-id
                      :spawn-mode mode
                      :model seed-model
                      :provider seed-provider}
               agents (assoc :agents agents)
               (some? (:sandbox params))
               (assoc :sandbox (normalize-sandbox (:sandbox params))))]
    (ling-runtime-options base context)))

(defn- prepare-ling!
  "Promote the collected request into a ling and its construction options."
  [{:keys [params agent-id] :as context}]
  (let [{:keys [presets spawn_mode agents]} params
        presets-vec (normalize-preset-names presets)
        mode (choose-spawn-mode! spawn_mode)
        normalized-agents (normalize-agent-definitions agents)
        construction {:mode mode :presets presets-vec :agents normalized-agents}
        ling-agent (ling/->ling agent-id (ling-construction-options context construction))]
    (assoc context :ling-agent ling-agent :mode (:spawn-mode ling-agent)
           :presets-vec presets-vec)))

(defn- ensure-ling-preflight!
  [{:keys [mode loop-params]}]
  (when-let [refusal (chat-point-refusal mode loop-params
                                         (headless-registry/headless-capabilities mode))]
    (throw (ex-info refusal {:spawn-mode mode})))
  (when-let [refusal (editor-preflight-refusal mode *editor-reachable?*)]
    (throw (ex-info refusal {:spawn-mode mode :fallback "headless"}))))

(defn- seed-spawn-options
  [{:keys [params parent child-depth child-grant brief mode
           seed-model seed-provider]}]
  (let [{:keys [kanban_task_id max_budget_usd]} params]
    (cond-> {:task brief
             :parent parent
             :depth child-depth
             :spawned-by (spawner params)
             :kanban-task-id kanban_task_id
             :spawn-mode mode
             :model seed-model
             :provider seed-provider
             :spawn/request params}
      max_budget_usd (assoc :max-budget-usd max_budget_usd)
      child-grant (assoc :grant child-grant))))

(defn- route-spawn!
  "Route exactly once, then preflight the provider that will actually run."
  [{:keys [params ling-agent] :as context}]
  (let [routed-opts (spawn-impl/route-spawn-opts (seed-spawn-options context) ling-agent)
        routed-provider (:provider routed-opts)]
    (when-let [err (provider-preflight-refusal (:spawn-mode ling-agent) routed-provider)]
      (throw (ex-info (str "Provider " (clojure.core/name routed-provider)
                           " cannot serve this spawn: " (:fix err)) err)))
    (when-let [refusal (kanban-claim-refusal params)]
      (throw (ex-info refusal {:kanban/claim-refused (:kanban_task_id params)})))
    (assoc context :routed-opts routed-opts :choice (:router/choice routed-opts)
           :routed-model (:model routed-opts) :routed-provider routed-provider)))

(defn- prepare-spawn!
  [{:keys [agent-type] :as context}]
  (case agent-type
    :ling (let [prepared (prepare-ling! context)]
            (ensure-ling-preflight! prepared)
            (route-spawn! prepared))))

(defn- spawn-at-boundary!
  [{:keys [ling-agent routed-opts] :as context}]
  (assoc context :slave-id (proto/spawn! ling-agent (dissoc routed-opts :spawn/request))))

(defn- log-spawned-ling!
  [{:keys [agent-id slave-id parent mode routed-provider routed-model choice
           seed-model seed-provider worker-tier token-budget brief params presets-vec
           project-id]}]
  (log/info "Spawned ling" {:requested-id agent-id :slave-id slave-id :parent parent
                            :spawn-mode mode :provider routed-provider :model routed-model
                            :routed? (some? choice) :requested-model seed-model
                            :requested-provider seed-provider :tier worker-tier
                            :token-budget token-budget :task-attached (some? brief)
                            :cwd (:cwd params) :presets presets-vec :project-id project-id}))

(defn- spawn-response
  [{:keys [slave-id parent mode routed-provider routed-model worker-tier choice
           seed-model seed-provider token-budget brief params presets-vec project-id
           child-grant]}]
  (mcp-json
   (cond-> {:success true :agent-id slave-id :type :ling :parent parent
            :spawn-mode mode :provider routed-provider :model routed-model
            :tier worker-tier :routed? (some? choice) :requested-model seed-model
            :requested-provider seed-provider :token-budget token-budget
            :task-attached (some? brief) :cwd (:cwd params) :presets presets-vec
            :project-id project-id}
     choice (assoc :router-choice (select-keys choice [:pool :strategy :work-class]))
     child-grant (assoc :grant child-grant)
     (nil? brief) (assoc :warning (str "Spawned with no task: the ling has no brief "
                                       "and will idle until `agent dispatch` sends one.")))))

(defn- handle-spawn-request!
  [params]
  (let [agent-type (keyword (:type params))]
    (try
      (-> params spawn-request-context prepare-spawn! spawn-at-boundary!
          (doto log-spawned-ling!) spawn-response)
      (catch Exception e
        (log/error "Failed to spawn agent" {:type agent-type :error (ex-message e)})
        (mcp-error (str "Failed to spawn " (clojure.core/name agent-type) ": " (ex-message e)))))))

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
  [params]
  (if (guards/child-ling?)
    (do
      (log/warn "Spawn denied: child ling attempted agent spawn"
                {:role (guards/get-role) :depth (guards/ling-depth)})
      (mcp-error (build-spawn-denied-message)))
    (if-let [refusal (some-> (heap-admission/hard-pressure
                              (or (ext/get-extension :agent/heap-probe)
                                  heap-admission/heap-probe))
                             heap-refusal)]
      (do
        (log/warn "Spawn refused: heap pressure" (dissoc refusal :message))
        (mcp-json refusal))
      (let [agent-type (keyword (:type params))]
        (if-not (and (agent-type-registry/valid-type? agent-type)
                     (agent-type-registry/spawnable? agent-type))
          (mcp-error (str "type must be one of: "
                          (pr-str (agent-type-registry/mcp-enum))))
          (handle-spawn-request! params))))))

;;; =============================================================================
;;; Contracts
;;; =============================================================================

(def SpawnParams
  "agent spawn params: an open map. Every key is validated (or defaulted) by
   the body, which answers an MCP error for a bad one, so all are optional."
  [:maybe [:map
           [:type {:optional true} [:maybe [:or :string :keyword]]]
           [:name {:optional true} [:maybe :string]]
           [:cwd {:optional true} [:maybe :string]]
           [:task {:optional true} [:maybe :string]]
           [:prompt {:optional true} [:maybe :string]]
           [:parent {:optional true} [:maybe :string]]
           [:_caller_id {:optional true} :any]]])

(def ResumeBackend
  "A valid resume request as the kebab map the headless backend reads."
  [:map
   [:run-id [:string {:min 1}]]
   [:at {:optional true} [:or [:map [:seq nat-int?]] [:map [:turn nat-int?]]]]
   [:prompt {:optional true} :string]])

(def LoopOpts
  "Ling loop opts read from a spawn request; absent keys omitted."
  [:map
   [:llm-retries {:optional true} nat-int?]
   [:resume {:optional true} ResumeBackend]
   [:chat-run-id {:optional true} [:string {:min 1}]]
   [:turn-budget {:optional true} :map]
   [:time-budget-ms {:optional true} pos-int?]])

(def SpawnMode
  "A spawn mode keyword (open: addons register more), or nil."
  [:maybe :keyword])

(def GrantDecision
  "spawn-grant's answer: a grant (nil = share) or a refusal message."
  [:or
   [:map [:grant [:maybe :map]] [:depth {:optional true} :int]]
   [:map [:refused :string] [:depth {:optional true} :int]]])

(m/=> effective-parent [:=> [:cat SpawnParams] [:maybe :string]])

(m/=> caller-parent [:=> [:cat :any] [:maybe :string]])

;; Never nil: the empty string records "root-level", so a lineage reader can
;; tell a spawn whose spawner is known to be nobody from a row written before
;; :slave/spawned-by existed.
(m/=> spawner [:=> [:cat SpawnParams] :string])

(m/=> sandbox-decision [:=> [:cat :any] [:maybe [:or :boolean [:= :refused]]]])

(m/=> normalize-sandbox [:=> [:cat :any] [:maybe :boolean]])

(m/=> turn-budget-normalizer [:=> [:cat] [:maybe helpers/Port]])

(m/=> turn-budget-opt [:=> [:cat [:maybe helpers/Port] :any] [:maybe :map]])

(m/=> time-budget-opt [:=> [:cat :any] [:maybe pos-int?]])

(m/=> loop-opts
      [:function
       [:=> [:cat [:maybe :map]] LoopOpts]
       [:=> [:cat [:maybe :map] [:maybe helpers/Port]] LoopOpts]])

(m/=> normalize-resume [:=> [:cat :any] [:maybe ResumeBackend]])

(m/=> chat-point-refusal
      [:=> [:cat SpawnMode [:maybe :map] [:maybe [:set :keyword]]] [:maybe :string]])

(m/=> provider-preflight-refusal
      [:=> [:cat SpawnMode [:maybe :keyword]]
       [:maybe [:map [:error :keyword] [:provider :keyword] [:fix :string]]]])

(m/=> editor-preflight-refusal [:=> [:cat SpawnMode helpers/Port] [:maybe :string]])

(m/=> resolve-spawn-mode
      [:=> [:cat [:maybe [:or :string :keyword]] helpers/Port]
       [:map [:mode :keyword] [:resolved-by [:enum :explicit :primary :fallback]]
        [:skipped [:vector :map]]]])

(m/=> spawn-brief [:=> [:cat SpawnParams] [:maybe :string]])

(m/=> spawn-grant [:=> [:cat [:maybe :string] :any] GrantDecision])

(m/=> handle-spawn [:=> [:cat SpawnParams] rb/McpResponse])
