(ns hive-mcp.agent.ling.spawn
  "Spawn pipeline for Ling agents: plan computation, preset loading, readiness
   dispatch, and the `Ling` defrecord itself."
  (:require [hive-mcp.agent.protocol :refer [IAgent]]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.agent.ling.headless-registry :as headless-reg]
            [hive-mcp.agent.ling.lifecycle :as lifecycle]
            [hive-mcp.agent.ling.spawn-store :as spawn-store]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.protocols.dispatch :as dispatch-ctx]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [taoensso.timbre :as log]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.swarm.channel :as swarm-channel]
            [hive-mcp.extensions.soft :as soft]
            [hive-mcp.swarm.claim.negotiate :as claim-negotiate]
            [hive-mcp.agent.ling.start-preflight :as start-preflight]
            [hive-mcp.agent.grant :as grant]
            [hive-mcp.agent.identity :as agent-identity]
            [hive-mcp.swarm.datascript :as ds]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(declare ordered-presets)

(def Presets
  "Preset names as a ling carries them: declared order, or sorted."
  [:vector :string])

(def LingOpts
  "The construction opts `slave->ling-opts` reads back from a slave row."
  [:map
   [:cwd [:maybe :string]]
   [:presets Presets]
   [:project-id [:maybe :string]]
   [:spawn-mode :keyword]
   [:model [:maybe :string]]
   [:token-budget :any]
   [:turn-budget {:optional true} :any]])

(def LingValue
  "A Ling record, as `->ling` builds it. The six identity fields are always
   present; the rest appear only when the opts carry them."
  [:map
   [:id :string]
   [:cwd [:maybe :string]]
   [:presets [:maybe [:or [:sequential :string] [:set :string]]]]
   [:project-id [:maybe :string]]
   [:spawn-mode :keyword]
   [:model [:maybe :string]]])

(defn slave->ling-opts
  "Extract ling construction opts from a DataScript slave entity."
  [slave]
  (cond-> {:cwd (:slave/cwd slave)
           :presets (ordered-presets slave)
           :project-id (:slave/project-id slave)
           :spawn-mode (or (:ling/spawn-mode slave) :claude)
           :model (:ling/model slave)
           :token-budget (:ling/token-budget slave)}
    (:ling/turn-budget slave) (assoc :turn-budget (:ling/turn-budget slave))))

(m/=> slave->ling-opts [:=> [:cat [:maybe :map]] LingOpts])

(defn ordered-presets
  "Read declared order from the scalar EDN snapshot; sort old unordered rows."
  [slave]
  (let [legacy (:slave/presets slave)
        encoded (:slave/preset-order slave)
        decoded (when (string? encoded)
                  (try (clojure.edn/read-string encoded)
                       (catch Exception _ nil)))]
    (if (and (vector? decoded)
             (every? string? decoded)
             (= (set decoded) (set legacy)))
      decoded
      (vec (sort legacy)))))

(m/=> ordered-presets [:=> [:cat [:maybe :map]] Presets])

(declare ->ling)

(defn- compute-spawn-plan
  "Pure computation: derive all spawn decisions from ling state and opts.
   Returns a plan map with resolved mode, model, strategy, and context.
   :grant is the effective grant (wire form) decided at the tool boundary,
   or nil for an unrestricted child. :spawned-by is the request caller that
   asked for the spawn (\"\" for root-level), or nil when the spawn did not
   come through the tool boundary."
  [ling opts]
  (let [effective-model (or (:model opts) (:model ling))
        mode (lifecycle/resolve-effective-mode
              {:model effective-model
               :spawn-mode (or (:spawn-mode opts) (:spawn-mode ling))})
        {:keys [depth parent kanban-task-id]
         :or {depth 1}} opts]
    {:effective-model effective-model
     :mode mode
     :strat (lifecycle/resolve-strategy mode)
     :ctx (cond-> (assoc (lifecycle/ling-ctx ling) :model effective-model)
            (:provider opts) (assoc :provider (:provider opts)))
     :depth depth
     :parent parent
     :spawned-by (:spawned-by opts)
     :kanban-task-id kanban-task-id
     :presets (or (:presets opts) (:presets ling))
     :cwd (:cwd ling)
     :project-id (:project-id ling)
     :ling-id (:id ling)
     :token-budget (:token-budget ling)
     :turn-budget (:turn-budget ling)
     :max-budget-usd (or (:max-budget-usd opts) (:max-budget-usd ling))
     :grant (:grant opts)
     :task (:task opts)}))

(defn- load-presets-content
  "Load preset content from preset .md files for headless backends.
   Claude CLI lings load presets from .claude/agents/ automatically;
   headless backends need explicit injection. The presets ship under
   resources/presets; `presets.dir` in config or HIVE_MCP_PRESETS_DIR
   overrides that default.
   Returns concatenated preset markdown string, or nil."
  [preset-names]
  (r/rescue nil
    (when (seq preset-names)
      (when-let [get-from-file (requiring-resolve 'hive-mcp.presets.core/get-preset-from-file)]
        (let [preset-dir (or (r/rescue nil
                               (when-let [cfg-fn (requiring-resolve 'hive-mcp.config.core/get-service-value)]
                                 (cfg-fn :presets :dir :env "HIVE_MCP_PRESETS_DIR")))
                             (str (System/getProperty "user.dir") "/resources/presets"))
              contents (->> preset-names
                            (keep (fn [pname]
                                    (when-let [p (get-from-file preset-dir (name pname))]
                                      (:content p))))
                            vec)]
          (when (seq contents)
            (let [joined (str/join "\n\n---\n\n" contents)]
              (log/info "Loaded preset content for headless ling"
                        {:presets (vec preset-names)
                         :loaded-count (count contents)
                         :total-chars (count joined)})
              joined)))))))

(defn- dispatch-after-ready!
  "Wait for terminal readiness, then dispatch task. Runs in a future so
   spawn! returns immediately. Uses readiness polling (not fixed sleep)
   and retries dispatch once on failure.

   Fixes the spawn-then-dispatch workaround: task param on spawn now
   works atomically without requiring a separate dispatch call."
  [{:keys [slave-id mode cwd presets project-id effective-model enriched-task]}]
  (future
    (try
      (let [wait-fn @(requiring-resolve
                      'hive-mcp.tools.consolidated.workflow.readiness/wait-for-ling-ready)
            ready   (wait-fn slave-id mode)
            can-try? (or (:ready? ready) (:slave ready))]
        (if can-try?
          (let [task-ling (->ling slave-id {:cwd cwd
                                            :presets presets
                                            :project-id project-id
                                            :spawn-mode mode
                                            :model effective-model})
                result (r/rescue {} (.dispatch! task-ling {:task enriched-task}))]
            (if-let [err (::r/error (meta result))]
              ;; First attempt failed — retry once after 2s
              (do
                (log/warn "Spawn-dispatch: first attempt failed, retrying in 2s"
                          {:ling-id slave-id :error (:message err)})
                (Thread/sleep 2000)
                (let [retry (r/rescue {} (.dispatch! task-ling {:task enriched-task}))]
                  (if-let [err2 (::r/error (meta retry))]
                    (log/error "Spawn-dispatch: both attempts failed — task lost"
                               {:ling-id slave-id :error (:message err2)})
                    (log/info "Spawn-dispatch: succeeded on retry"
                              {:ling-id slave-id}))))
              (log/info "Spawn-dispatch: task dispatched after readiness wait"
                        {:ling-id slave-id
                         :elapsed-ms (:elapsed-ms ready)
                         :best-effort? (not (:ready? ready))})))
          (log/error "Spawn-dispatch: ling not ready, task will not be dispatched"
                     {:ling-id slave-id
                      :phase (:phase ready)
                      :elapsed-ms (:elapsed-ms ready)})))
      (catch Throwable t
        (log/error t "Spawn-dispatch: unexpected error"
                   {:ling-id slave-id})))))

(defn- headless-mode?
  [mode]
  (contains? (headless-reg/registered-headless) mode))

(defn- enrich-task
  [{:keys [task cwd kanban-task-id]}]
  (when task
    (let [ling-context-str (r/rescue nil
                             (when-let [ling-catchup (soft/resolve-soft 'hive-mcp.workflows.catchup-ling/ling-catchup)] (ling-catchup {:directory cwd
                               :task task
                               :kanban-task-id kanban-task-id})))]
      (if ling-context-str
        (str ling-context-str "\n\n---\n\n" task)
        task))))

(defn- resolve-headless-inputs
  [{:keys [mode ctx presets]}]
  (let [headless? (headless-mode? mode)]
    {:headless? headless?
     :preset-content (when (and headless? (seq presets))
                       (load-presets-content presets))
     :api-key (when headless?
                (lifecycle/resolve-api-key-for-provider
                 (or (:provider ctx) :openrouter)))}))

(defn- spawn-opts
  "Build the spawn-opts passed to strategy-spawn!. Backend-agnostic: opts pass
   through (incl. any :system-prompt), with resolved preset-content and api-key
   added. Each backend maps these to its own config.

   Identity: `credential` is the child's minted spawn credential (or nil).
   It rides as :credential, and :env-extra is rebuilt by
   `agent-identity/child-env`, so CLAUDE_SWARM_SLAVE_ID and
   HIVE_AGENT_CREDENTIAL are set by the server and a caller's env-extra
   cannot override either, whichever backend reads the env."
  ([opts enriched-task inputs] (spawn-opts opts enriched-task inputs nil nil))
  ([opts enriched-task {:keys [preset-content api-key]} ling-id credential]
   (cond-> (if enriched-task
             (assoc opts :task enriched-task)
             opts)
     (seq preset-content)
     (assoc :preset-content preset-content)
     api-key
     (assoc :api-key api-key)
     (or ling-id (seq (:env-extra opts)))
     (assoc :env-extra (agent-identity/child-env (:env-extra opts) ling-id credential))
     credential
     (assoc :credential credential))))

(defn- initial-slave-attrs
  [{:keys [depth parent presets cwd project-id kanban-task-id grant spawned-by]} enriched-task]
  (cond-> {:status (if enriched-task :working :idle)
           :depth depth
           :parent parent
           :presets presets
           :cwd cwd
           :project-id project-id
           :kanban-task-id kanban-task-id}
    (some? grant) (assoc :grant grant)
    (some? spawned-by) (assoc :spawned-by spawned-by)))

(def spawned-by-attr
  "Registry attribute naming the request caller that asked for a spawn: the
   caller id as `hive-mcp.tools.agent.spawn/spawner` read it, \"\" when that
   caller was root-level. Unlike :slave/parent it is never taken from a
   param, so enclave lineage (hive-cloak) walks it instead."
  :slave/spawned-by)

(def row-safe-keys
  "Row attrs kept when the row filter itself throws: routing and liveness
   only, nothing that names the work. Mirrors hive-agent's own set."
  #{:status :depth :parent})

(defn row-attrs
  "ATTRS of SLAVE-ID's registry row after ROW-FILTER, a
   (fn [ling-id attrs] -> attrs) or nil. nil (no swarm addon) is identity.
   A filter that throws, or answers anything but a map, fails CLOSED: the
   row keeps only `row-safe-keys`, so a broken filter can never let task
   text, cwd or project-id through."
  [row-filter slave-id attrs]
  (if-not row-filter
    attrs
    (let [out (try (row-filter slave-id attrs) (catch Throwable _ ::threw))]
      (if (map? out)
        out
        (do (log/warn "[spawn] row filter failed closed" {:slave-id slave-id})
            (select-keys attrs row-safe-keys))))))

(m/=> row-attrs [:=> [:cat [:maybe ifn?] :any :map] :map])

(defn- row-filter
  "hive-agent's registry row filter chain, resolved softly so core needs no
   swarm addon: nil when hive-agent (or its shout-filters ns) is absent."
  []
  (try (requiring-resolve 'hive-agent.events.shout-filters/row!)
       (catch Throwable _ nil)))

(defn- register-slave-row!
  "Persist a slave row through the spawn store, first making sure its parent
   can be referenced: a coordinator session named as parent gets its own row
   on demand. A recorded grant is written beside :slave/parent in the same
   registration, before the backend starts the ling, so its first tool call
   is already gated. So is :slave/spawned-by, the request caller that asked
   for the spawn: enclave membership follows it, never :slave/parent, which
   an explicit `parent` param sets to any agent.

   The row attrs pass hive-agent's row filters first (`row-attrs`), so an
   addon such as hive-darkmatter can replace identifying attrs before they
   reach the registry. The grant and the spawner are not filtered: they gate,
   they do not name.

   The filtered presets are also persisted as an ordered scalar snapshot via
   the spawn-store port before backend start, preserving declared precedence
   when the cardinality-many preset attribute reads back as a set."
  [store slave-id attrs]
  (spawn-store/ensure-coordinator-session! store (:parent attrs))
  (let [filtered (row-attrs (row-filter) slave-id (dissoc attrs :grant :spawned-by))
        presets (:presets filtered)
        res (spawn-store/add-slave! store slave-id filtered)]
    (when (seq presets)
      (spawn-store/update-slave! store slave-id
                                 {:slave/preset-order (pr-str (vec presets))}))
    (when-let [g (:grant attrs)]
      (spawn-store/update-slave! store slave-id {grant/slave-attr g}))
    (when (string? (:spawned-by attrs))
      (spawn-store/update-slave! store slave-id {spawned-by-attr (:spawned-by attrs)}))
    res))

(defn- register-requested-slave!
  [{:keys [ling-id] :as plan} enriched-task]
  (register-slave-row! (spawn-store/get-store)
                       ling-id
                       (initial-slave-attrs plan enriched-task)))

(defn- reconcile-spawned-slave!
  [{:keys [ling-id] :as plan} slave-id enriched-task]
  (when (not= slave-id ling-id)
    (let [store (spawn-store/get-store)]
      (spawn-store/remove-slave! store ling-id)
      (register-slave-row! store
                           slave-id
                           (assoc (initial-slave-attrs plan enriched-task)
                                  :requested-id ling-id)))))

(defn- provider-name
  "The explicit LLM provider of a spawn PLAN as a string, or nil."
  [plan]
  (let [p (get-in plan [:ctx :provider])]
    (cond
      (keyword? p) (name p)
      (and (string? p) (not (str/blank? p))) p
      :else nil)))

(defn- stamp-spawn-metadata!
  [{:keys [mode effective-model token-budget turn-budget] :as plan} slave-id headless?]
  (let [now (System/currentTimeMillis)
        provider (provider-name plan)]
    (spawn-store/update-slave! (spawn-store/get-store)
                               slave-id
                               (cond-> {:ling/spawn-mode mode
                                        :ling/model (or effective-model "claude")
                                        :slave/alive? true
                                        :slave/spawned-at now
                                        :slave/last-active-at now}
                                 (some? token-budget)
                                 (assoc :ling/token-budget token-budget)
                                 (some? turn-budget)
                                 (assoc :ling/turn-budget turn-budget)
                                 provider
                                 (assoc :ling/provider provider)
                                 headless?
                                 (assoc :ling/process-alive? true)))))

(defn- register-ling-resources!
  [slave-id]
  (r/rescue nil
    (when-let [make (requiring-resolve 'hive-agent.lifecycle.resources/make-ling-resources)]
      (make slave-id
            (fn [] (spawn-store/claims-for-slave (spawn-store/get-store)
                                                 slave-id))))))

(defn- publish-agent-spawn!
  [{:keys [cwd mode project-id effective-model]} slave-id headless?]
  (r/rescue nil
    (when-let [publish! (requiring-resolve 'hive-mcp.nats.bridge/publish-event!)]
      (publish! {:type       :agent-spawn
                 :agent-id   slave-id
                 :timestamp  (System/currentTimeMillis)
                 :data       {:cwd cwd
                              :mode mode
                              :project-id project-id
                              :headless? headless?
                              :model (or effective-model "claude")}}))))

(defn- publish-slave-spawned!
  [{:keys [depth parent cwd project-id]} slave-id]
  (r/rescue nil
    (when-let [publish-slave! (requiring-resolve 'hive-mcp.swarm.event-bridge/publish-slave-event!)]
      (publish-slave! {:type       :slave-spawned
                       :slave-id   slave-id
                       :name       slave-id
                       :depth      depth
                       :parent-id  parent
                       :cwd        cwd
                       :project-id project-id
                       :timestamp (System/currentTimeMillis)}))))

(defn- register-budget!
  [{:keys [max-budget-usd effective-model]} slave-id]
  (when (and max-budget-usd (pos? max-budget-usd))
    (r/rescue nil
      (when-let [register-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/register-budget!)]
        (register-fn slave-id max-budget-usd {:model (or effective-model "claude")})
        (log/info "Budget guardrail registered for ling"
                  {:ling-id slave-id :max-budget-usd max-budget-usd})))))

(defn- dispatch-when-ready!
  [{:keys [mode cwd presets project-id effective-model]} slave-id enriched-task headless?]
  (when (and enriched-task (not headless?))
    (dispatch-after-ready! {:slave-id slave-id :mode mode :cwd cwd
                            :presets presets :project-id project-id
                            :effective-model effective-model
                            :enriched-task enriched-task})))

(defn- apply-spawn-overlay
  "Generic spawn-opts extension seam. Strips the internal :spawn/request key
   from opts, then applies any extension registered under :spawn/opts-overlay
   as (f opts ctx) -> opts'. Fail-soft: nil or a throw falls back to the
   stripped opts. Applied once in spawn! so plan + executor both see merged opts."
  [opts ling]
  (let [req  (:spawn/request opts)
        opts (dissoc opts :spawn/request)]
    (if-let [f (ext/get-extension :spawn/opts-overlay)]
      (or (r/rescue nil
            (f opts {:request req
                     :cwd (:cwd ling)
                     :project-id (:project-id ling)
                     :task (:task opts)
                     :kanban-task-id (:kanban-task-id opts)}))
          opts)
      opts)))

(defn- execute-spawn-plan!
  "Execute spawn effects: catchup enrichment, store pre-registration, strategy
   spawn, store reconciliation, budget registration, and readiness-based dispatch.

   CRITICAL: For headless backends, strategy-spawn! triggers start! which fires
   the agentic loop immediately. The ling MUST exist in the spawn store before
   that happens, otherwise completion handlers hit a deregistration race (H2
   fix).

   The child's spawn credential is minted after registration, from the same
   parent, depth and grant the row records, and before the backend starts
   the child, so its first tool call can already present it."
  [plan opts]
  (start-preflight/ensure-startable! (:mode plan) (:cwd plan))
  (let [{:keys [strat ctx]} plan
        enriched-task (enrich-task plan)
        headless-inputs (resolve-headless-inputs plan)
        headless? (:headless? headless-inputs)
        _ (register-requested-slave! plan enriched-task)
        credential (agent-identity/mint-for-child {:agent-id  (:ling-id plan)
                                                   :parent-id (:parent plan)
                                                   :depth     (:depth plan)
                                                   :grant     (:grant plan)})
        spawn-opts (spawn-opts opts enriched-task headless-inputs (:ling-id plan) credential)]
    (let [slave-id (strategy/strategy-spawn! strat ctx spawn-opts)]
      (reconcile-spawned-slave! plan slave-id enriched-task)
      (stamp-spawn-metadata! plan slave-id headless?)
      (register-ling-resources! slave-id)
      (publish-agent-spawn! plan slave-id headless?)
      (publish-slave-spawned! plan slave-id)
      (register-budget! plan slave-id)
      (dispatch-when-ready! plan slave-id enriched-task headless?)
      slave-id)))

(defn dispatch-refusal
  "The refusal in a strategy-dispatch! ANSWER, or nil when it dispatched.
   A backend refuses without throwing by answering `false`, a map carrying
   `:dispatched false`, or a hive-dsl err Result. Any other answer (true,
   nil, {:dispatched true ...}) is a dispatch."
  [answer]
  (cond
    (false? answer) {:answer false}
    (and (map? answer) (false? (:dispatched answer))) answer
    (r/err? answer) answer
    :else nil))

;; Any answer is read; a refusal is one of the three refused shapes.
(m/=> dispatch-refusal
      [:=> [:cat :any]
       [:maybe [:or
                [:map [:answer [:= false]]]
                [:map [:dispatched [:= false]]]
                [:map [:error :any]]]]])

(defn- status-after-failed-dispatch
  "The :slave/status a failed dispatch leaves, from the status PRIOR-STATUS
   the ling had before the dispatch wrote :working and the failure E.

   - A backend that knows what the failure did to the ling names it in the
     exception's ex-data as :slave-status (e.g. :error when its session is
     gone, :idle when it refused a start); that status is used.
   - Any other failure (a refused answer, a thrown backend or bookkeeping
     error) started nothing on the ling, so it is as it was: PRIOR-STATUS.
     A failure to dispatch is not evidence the ling is broken; marking it
     :error would hide a healthy ling from the next dispatch."
  [prior-status e]
  (let [named (some-> (ex-data e) :slave-status)]
    (if (keyword? named) named prior-status)))

(defn- restore-status-after-failed-dispatch!
  "Set ling ID's :slave/status back after a failed dispatch (see
   `status-after-failed-dispatch`), but only while it still holds the
   :working this dispatch wrote: the write is a compare-and-set against
   :working, so a status another writer set meanwhile (a kill, a completion,
   an error the backend recorded) is never overwritten. No-op when the ling
   had no row before the dispatch. Never throws."
  [id prior-status e]
  (when-let [target (status-after-failed-dispatch prior-status e)]
    (when (not= target :working)
      (try
        (ds/transact! [[:db/cas [:slave/id id] :slave/status :working target]])
        (log/info "Restored ling status after failed dispatch"
                  {:ling-id id :status target})
        (catch Throwable t
          (log/info "Ling status changed during failed dispatch; left as is"
                    {:ling-id id :wanted target
                     :current (:slave/status (ds-queries/get-slave id))
                     :reason (ex-message t)}))))))

(defrecord Ling [id cwd presets project-id spawn-mode model provider token-budget kg-compress? sliding-window-size agents max-budget-usd]
  IAgent

  (spawn! [this opts]
    (let [opts (apply-spawn-overlay opts this)
          plan (compute-spawn-plan this opts)]
      (execute-spawn-plan! plan opts)))

  (dispatch! [this task-opts]
    (let [{:keys [task files _timeout-ms dispatch-context]} task-opts
          ctx (or dispatch-context
                  (when task (dispatch-ctx/ensure-context task)))
          resolved-task (if ctx
                          (:prompt (dispatch-ctx/resolve-context ctx))
                          task)
          task-id (str "task-" (System/currentTimeMillis) "-" (subs id 0 (min 8 (count id))))
          mode (or spawn-mode
                   (when-let [slave (ds-queries/get-slave id)]
                     (:ling/spawn-mode slave))
                   :claude)
          strat (lifecycle/resolve-strategy mode)]
      (let [prior-status (:slave/status (ds-queries/get-slave id))
            resolved-opts (cond-> (assoc task-opts :task resolved-task :task-id task-id)
                            ctx (assoc :dispatch-context ctx))]
        (ds-lings/update-slave! id {:slave/status :working})
        ;; Everything after the :working write runs inside the try, so no
        ;; failure (task bookkeeping, claims, strategy) leaves the ling shown
        ;; as :working when nothing is running.
        ;;
        ;; NOTE: intentionally do NOT publish :task-dispatched — the JVM-side
        ;; calls below already do every action handle-task-dispatched would do.
        ;; :task-failed / :slave-killed below ARE needed (queue + cleanup).
        (try
          (swarm-channel/record-dispatched-task! task-id)
          (ds-lings/add-task! task-id id {:status :dispatched
                                          :prompt resolved-task
                                          :files files})
          (when (seq files)
            (.claim-files! this files task-id))
          (let [answer (strategy/strategy-dispatch! strat (lifecycle/ling-ctx this) resolved-opts)]
            (when-let [refusal (dispatch-refusal answer)]
              (throw (ex-info "Backend refused the dispatch"
                              {:ling-id id :task-id task-id
                               :dispatch/refused? true :refusal refusal}))))
          (log/info "Task dispatched to ling" {:ling-id id :task-id task-id
                                               :mode mode :files files
                                               :context-type (when ctx
                                                               (dispatch-ctx/context-type ctx))})
          task-id
          ;; Hive axiom: `catch Throwable` on supervision boundaries so
          ;; AssertionError (from :pre/:post in DS helpers, e.g. lings/
          ;; {enter,exit}-critical-op!) surfaces to the task-failed path
          ;; instead of being silently swallowed and leaving the dispatch
          ;; caller parked. This is the ling/vterm dispatch hot-path —
          ;; an Error here used to propagate past the MCP handler and leave
          ;; :dispatched tasks wedged with no :task-failed event fired.
          (catch Throwable e
            (log/error "Failed to dispatch to ling"
                       {:ling-id id :task-id task-id :mode mode :error (ex-message e)})
            (restore-status-after-failed-dispatch! id prior-status e)
            (r/rescue nil
              (ds-lings/update-task! task-id {:status :failed
                                              :error (ex-message e)}))
            ;; :task-failed -> swarm/sync releases claims + processes queue.
            (r/rescue nil
              (when-let [publish-slave! (requiring-resolve 'hive-mcp.swarm.event-bridge/publish-slave-event!)]
                (publish-slave! {:type      :task-failed
                                 :slave-id  id
                                 :task-id   task-id
                                 :error     (ex-message e)
                                 :timestamp (System/currentTimeMillis)})))
            (throw (ex-info (str "Failed to dispatch to ling " id ": "
                                 (or (not-empty (ex-message e)) (.getName (class e)))
                                 (when-let [refusal (:refusal (ex-data e))]
                                   (str " " (pr-str refusal))))
                            {:ling-id id :task-id task-id :error (ex-message e)}
                            e)))))))

  (status [this]
    (let [ds-status (ds-queries/get-slave id)
          mode (or spawn-mode
                   (:ling/spawn-mode ds-status)
                   :claude)
          strat (lifecycle/resolve-strategy mode)]
      (strategy/strategy-status strat (lifecycle/ling-ctx this) ds-status)))

  (kill! [this]
    (let [{:keys [can-kill? blocking-ops]} (ds-lings/can-kill? id)
          mode (or spawn-mode
                   (when-let [slave (ds-queries/get-slave id)]
                     (:ling/spawn-mode slave))
                   :claude)]
      (if can-kill?
        (do
          ;; Release per-ling owned resources (channels, caches) + unregister.
          ;; Lazy-resolve the release fn so this is a no-op when the addon is absent.
          (r/rescue nil
            (when-let [release (requiring-resolve 'hive-agent.lifecycle.resources/release-ling-resources!)]
              (release id)))
          (.release-claims! this)
          (r/rescue nil
                    (when-let [deregister-fn (requiring-resolve 'hive-mcp.agent.hooks.budget/deregister-budget!)]
                      (deregister-fn id)))
          (let [strat (lifecycle/resolve-strategy mode)
                result (strategy/strategy-kill! strat (lifecycle/ling-ctx this))]
            (when (:killed? result)
              (ds-lings/remove-slave! id)
              ;; Publish agent-kill event via NATS backbone (non-fatal).
              (r/rescue nil
                (when-let [publish! (requiring-resolve 'hive-mcp.nats.bridge/publish-event!)]
                  (publish! {:type       :agent-kill
                             :agent-id   id
                             :timestamp  (System/currentTimeMillis)})))
              ;; :slave-killed -> channel.core+NATS for swarm cleanup handlers.
              (r/rescue nil
                (when-let [publish-slave! (requiring-resolve 'hive-mcp.swarm.event-bridge/publish-slave-event!)]
                  (publish-slave! {:type      :slave-killed
                                   :slave-id  id
                                   :timestamp (System/currentTimeMillis)}))))
            result))
        (do
          (log/warn "Cannot kill ling - critical ops in progress"
                    {:id id :blocking-ops blocking-ops})
          {:killed? false
           :reason :critical-ops-blocking
           :blocking-ops blocking-ops}))))

  (agent-type [_]
    :ling)

  (can-chain-tools? [_]
    true)

  (claims [_this]
    (let [all-claims (ds-queries/get-all-claims)]
      (->> all-claims
           (filter #(= id (:slave-id %)))
           (map :file)
           vec)))

  (claim-files! [this files task-id]
    ;; All or nothing, under the claim lock, through the span registry. The
    ;; per-file has-conflict?-then-claim this replaced was not atomic, and the
    ;; store's upsert let an unchecked claim take another ling's file. A
    ;; refusal parks this ling on the held keys (the release wakes it) and
    ;; sends each holder one yield request; the result says so.
    (when (seq files)
      (let [result (claim-negotiate/acquire-for-ling!
                    id files {:task-id task-id :cwd (:cwd this)})]
        (if (:acquired? result)
          (log/info "Files claimed" {:ling-id id :count (count files)})
          (log/warn "Files not claimed; parked until released"
                    {:ling-id id :task-id task-id :parked (:parked result)
                     :conflicts (mapv #(select-keys % [:file :qn :held-by :reason])
                                      (:conflicts result))}))
        result)))

  (release-claims! [_this]
    (let [released-count (ds-lings/release-claims-for-slave! id)]
      (log/info "Released claims" {:ling-id id :count released-count})
      released-count)))

(defn ->ling
  "Create a new Ling agent instance.
   Omitting :spawn-mode yields (lifecycle/default-spawn-mode)."
  [id opts]
  (let [model-val (:model opts)
        effective-spawn-mode (lifecycle/resolve-effective-mode
                              {:model model-val
                               :spawn-mode (:spawn-mode opts)})]
    (map->Ling (cond-> {:id id
                        :cwd (:cwd opts)
                        :presets (:presets opts [])
                        :project-id (:project-id opts)
                        :spawn-mode effective-spawn-mode
                        :model model-val}
                 (:provider opts)           (assoc :provider (:provider opts))
                 (:token-budget opts)       (assoc :token-budget (:token-budget opts))
                 (:turn-budget opts)        (assoc :turn-budget (:turn-budget opts))
                 (some? (:kg-compress? opts)) (assoc :kg-compress? (:kg-compress? opts))
                 (some? (:verbose? opts))   (assoc :verbose? (:verbose? opts))
                 (:llm-retries opts)        (assoc :llm-retries (:llm-retries opts))
                 (:resume opts)             (assoc :resume (:resume opts))
                 (:chat-run-id opts)        (assoc :chat-run-id (:chat-run-id opts))
                 (:sliding-window-size opts) (assoc :sliding-window-size (:sliding-window-size opts))
                 (:agents opts)             (assoc :agents (:agents opts))
                 (:max-budget-usd opts)     (assoc :max-budget-usd (:max-budget-usd opts))
                 (some? (:sandbox opts))    (assoc :sandbox (:sandbox opts))))))

(m/=> ->ling [:=> [:cat :string [:maybe :map]] LingValue])

(defn create-ling!
  "Create and spawn a new ling agent."
  [id opts]
  (let [ling (->ling id opts)]
    (.spawn! ling opts)))

;; The answer is the slave id the strategy's spawn! returned.
(m/=> create-ling! [:=> [:cat :string [:maybe :map]] [:maybe :string]])
