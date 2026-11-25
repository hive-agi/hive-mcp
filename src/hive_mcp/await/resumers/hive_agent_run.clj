(ns hive-mcp.await.resumers.hive-agent-run
  "The :hive-agent-run resumer: continue a parked hive-agent run from its
   chat point when its awaiter wakes.

   Owner: {:kind :hive-agent-run :run-id <chat-point run id> :cwd <abs dir>
           :ling-id? :model? :provider? :parent? :spawned-by? :project-id?
           :spawn-mode? :turn-budget? :token-budget? :time-budget-ms?
           :max-budget-usd? :sandbox? :grant? :presets? :kanban-task-id?
           :awaiter-id?}.

   `:run-id` is optional at CREATE: the ling does not know its chat-point run
   id before it parks, so it is bound later, either by `arm!` on the
   :parked event or from the park tool's own request. A resumer opened for
   an owner with no run-id refuses at open time with :await/owner-unbound,
   which is what a wake with no arming behind it looks like.

   On the wake it spawns one successor ling with resume {:run_id run-id
   :prompt <continuation + wake payload>}, the way hive-agent's ling-wave
   escalation-continuation continues a run on a fresh lease.

   The successor INHERITS the parked ling's authority (review B5): grant,
   sandbox, presets, budgets, kanban card, spawned-by and parent all travel
   in the owner, which is snapshotted from the parked ling when the awaiter
   is created. Anything the owner does not name is not carried, so an owner
   built by hand gets a plain child, never a silently widened one.

   The spawn path is reached through `spawn-fn` (requiring-resolve of
   hive-mcp.tools.agent.spawn/handle-spawn), so loading this namespace pulls
   in nothing of the agent stack and a test can stand a double in."
  (:require [clojure.string :as str]
            [hive-mcp.await.ports :as ports]
            [malli.core :as m]
            [hive-mcp.resilience.swallow :as swallow]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Owner
  "What this resumer needs from the parked run's record. :run-id and :cwd
   are required to RESUME; a create may omit :run-id, which `arm` binds."
  [:map
   [:kind [:= :hive-agent-run]]
   [:run-id {:optional true} [:maybe [:string {:min 1}]]]
   [:cwd {:optional true} [:maybe [:string {:min 1}]]]
   [:ling-id {:optional true} [:maybe :string]]
   [:model {:optional true} [:maybe :string]]
   [:provider {:optional true} [:maybe :string]]
   [:parent {:optional true} [:maybe :string]]
   [:spawned-by {:optional true} [:maybe :string]]
   [:project-id {:optional true} [:maybe :string]]
   [:spawn-mode {:optional true} [:maybe :string]]
   [:turn-budget {:optional true} [:maybe :map]]
   [:token-budget {:optional true} [:maybe :int]]
   [:time-budget-ms {:optional true} [:maybe :int]]
   [:max-budget-usd {:optional true} [:maybe number?]]
   [:sandbox {:optional true} [:maybe :boolean]]
   [:grant {:optional true} [:maybe :map]]
   [:presets {:optional true} [:maybe [:sequential :string]]]
   [:kanban-task-id {:optional true} [:maybe :string]]])

(defonce ^{:doc "Atom holding the fn spawn params -> MCP response; nil means
  hive-mcp.tools.agent.spawn/handle-spawn, resolved at wake time."}
  spawn-fn
  (atom nil))

(defn successor-name
  "Pure: the successor ling's name for `owner` woken by awaiter
   `awaiter-id` on `attempt`: the parked ling id (or \"await\") plus -w-, the
   first 8 characters of the awaiter id and -<attempt>. The attempt is part
   of the NAME so a re-driven wake cannot start a second successor while the
   first is still registered: the spawn refuses a duplicate name.

   Answers a string; a caller that already has the name keeps it."
  ([owner awaiter-id] (successor-name owner awaiter-id 1))
  ([owner awaiter-id attempt]
   (let [aid (str awaiter-id)
         n   (long (or attempt 1))]
     (str (or (not-empty (:ling-id owner)) "await")
          "-w-" (subs aid 0 (min 8 (count aid)))
          "-" n))))

(m/=> successor-name [:function [:=> [:cat :map :any] :string]
                      [:=> [:cat :map :any :any] :string]])

(defn- resolve-spawn
  "The spawn fn: the installed double, else handle-spawn resolved softly.
   Throws ex-info :await/resumer-unavailable when the agent stack is absent."
  []
  (or @spawn-fn
      (swallow/attempt (some-> (requiring-resolve 'hive-mcp.tools.agent.spawn/handle-spawn) deref))
      (throw (ex-info "await: hive-agent-run resumer cannot reach the spawn path"
                      {:error :await/resumer-unavailable}))))

(defn spawn-params
  "Pure: the `swarm agent spawn` params that continue `owner`'s run with
   `prompt`, for the wake of `awaiter-id` on `attempt`. A headless
   chat-point resume of :run-id at its tip, prompt appended. Every authority
   key the owner carries is replayed onto the successor, so a parked ling's
   grant, sandbox, presets, budgets, card and lineage survive the wake."
  ([owner awaiter-id prompt] (spawn-params owner awaiter-id prompt 1))
  ([owner awaiter-id prompt attempt]
   (cond-> {:type       "ling"
            :name       (successor-name owner awaiter-id attempt)
            :cwd        (:cwd owner)
            :spawn_mode (or (not-empty (:spawn-mode owner)) "hive-agent")
            :resume     {:run_id (:run-id owner) :prompt prompt}}
     (not-empty (:model owner))       (assoc :model (:model owner))
     (not-empty (:provider owner))    (assoc :provider (:provider owner))
     (not-empty (:parent owner))      (assoc :parent (:parent owner))
     (not-empty (:project-id owner))  (assoc :project_id (:project-id owner))
     (not-empty (:spawned-by owner))  (assoc :spawned_by (:spawned-by owner))
     (not-empty (:kanban-task-id owner)) (assoc :kanban_task_id (:kanban-task-id owner))
     (map? (:turn-budget owner))      (assoc :turn_budget (:turn-budget owner))
     (some? (:token-budget owner))    (assoc :token_budget (:token-budget owner))
     (some? (:time-budget-ms owner))  (assoc :time_budget_ms (:time-budget-ms owner))
     (some? (:max-budget-usd owner))  (assoc :max_budget_usd (:max-budget-usd owner))
     (some? (:sandbox owner))         (assoc :sandbox (:sandbox owner))
     (map? (:grant owner))            (assoc :grant (:grant owner))
     (seq (:presets owner))           (assoc :presets (vec (:presets owner))))))

(m/=> spawn-params [:function [:=> [:cat Owner :any [:maybe :string]] :map]
                    [:=> [:cat Owner :any [:maybe :string] :any] :map]])

(defn- answer-failed?
  "True when a spawn answer is a refusal or a deferral: an MCP :isError
   response, or {:success false ...} (heap pressure answers
   {:success false :deferred true} and is NOT :isError, which is how a wake
   used to be lost silently). Pure."
  [resp]
  (boolean
   (or (:isError resp)
       (and (map? resp) (contains? resp :success) (false? (:success resp))))))

(m/=> answer-failed? [:=> [:cat :any] :boolean])

(defn- answer-text
  "One line from a spawn answer, for the failure recorded on the awaiter."
  [resp]
  (let [txts (if (map? resp) (keep :text [resp]) (keep :text (if (sequential? resp) resp [])))]
    (when (seq txts) (str/join " " txts))))

(defmethod ports/open-resumer :hive-agent-run
  [owner]
  (when-let [explain (m/explain Owner owner)]
    (throw (ex-info "await: invalid hive-agent-run owner"
                    {:error   :await/invalid-owner
                     :owner   owner
                     :explain (pr-str (mapv #(select-keys % [:path :type]) (:errors explain)))})))
  (when (str/blank? (str (:run-id owner)))
    (throw (ex-info (str "await: this hive-agent-run owner has no :run-id yet."
                         " Arm the awaiter on the :parked event (or pass run_id to"
                         " the park tool) and it is bound then.")
                    {:error :await/owner-unbound :owner owner})))
  (when (str/blank? (str (:cwd owner)))
    (throw (ex-info "await: a hive-agent-run owner needs :cwd (the directory the run lives in)"
                    {:error :await/invalid-owner :owner owner})))
  (fn resume-run [{:keys [awaiter prompt]}]
    (let [attempt (or (:attempt awaiter) 1)
          cwd     (:cwd owner)]
      (when-not (.exists (java.io.File. ^String cwd))
        (throw (ex-info (str "await: the parked run's cwd no longer exists: " cwd)
                        {:error :await/cwd-missing :cwd cwd})))
      (let [params (spawn-params owner (:id awaiter) prompt attempt)
            resp   ((resolve-spawn) params)]
        (when (answer-failed? resp)
          (throw (ex-info (str "await: resume spawn refused: "
                               (or (answer-text resp) (pr-str resp)))
                          {:error :await/resume-refused
                           :spawn params
                           :deferred (true? (:deferred resp))})))
        {:spawned (:name params) :run-id (:run-id owner) :attempt attempt}))))
