(ns hive-mcp.await.resumers.hive-agent-run
  "The :hive-agent-run resumer: continue a parked hive-agent run from its
   chat point when its awaiter wakes.

   Owner: {:kind :hive-agent-run :run-id <chat-point run id> :cwd <abs dir>
           :ling-id? :model? :provider? :parent? :project-id? :turn-budget?
           :spawn-mode?}.
   On the wake it spawns one successor ling with resume {:run_id run-id
   :prompt <continuation + wake payload>}, the way hive-agent's ling-wave
   escalation-continuation continues a run on a fresh lease. The successor
   name is derived from the awaiter id, so a duplicate resume collides on
   the name instead of starting a second run.

   The spawn path is reached through `spawn-fn` (requiring-resolve of
   hive-mcp.tools.agent.spawn/handle-spawn), so loading this namespace pulls
   in nothing of the agent stack and a test can stand a double in."
  (:require [clojure.string :as str]
            [hive-mcp.await.ports :as ports]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Owner
  "What this resumer needs from the parked run's record."
  [:map
   [:kind [:= :hive-agent-run]]
   [:run-id [:string {:min 1}]]
   [:cwd [:string {:min 1}]]
   [:ling-id {:optional true} [:maybe :string]]
   [:model {:optional true} [:maybe :string]]
   [:provider {:optional true} [:maybe :string]]
   [:parent {:optional true} [:maybe :string]]
   [:project-id {:optional true} [:maybe :string]]
   [:spawn-mode {:optional true} [:maybe :string]]
   [:turn-budget {:optional true} [:maybe :map]]])

(defonce ^{:doc "Atom holding the fn spawn params -> MCP response; nil means
  hive-mcp.tools.agent.spawn/handle-spawn, resolved at wake time."}
  spawn-fn
  (atom nil))

(defn successor-name
  "Pure: the successor ling's name for `owner` woken by awaiter `awaiter-id`:
   the parked ling id (or \"await\") plus -w- and the first 8 characters of
   the awaiter id."
  [owner awaiter-id]
  (let [aid (str awaiter-id)]
    (str (or (not-empty (:ling-id owner)) "await") "-w-" (subs aid 0 (min 8 (count aid))))))

(defn- resolve-spawn
  "The spawn fn: the installed double, else handle-spawn resolved softly.
   Throws ex-info :await/resumer-unavailable when the agent stack is absent."
  []
  (or @spawn-fn
      (try (some-> (requiring-resolve 'hive-mcp.tools.agent.spawn/handle-spawn) deref)
           (catch Throwable _ nil))
      (throw (ex-info "await: hive-agent-run resumer cannot reach the spawn path"
                      {:error :await/resumer-unavailable}))))

(defn spawn-params
  "Pure: the `swarm agent spawn` params that continue `owner`'s run with
   `prompt`, for the wake of `awaiter-id`. A headless chat-point resume of
   :run-id at its tip, prompt appended; model, provider, parent, project and
   turn budget carried over when the owner names them."
  [owner awaiter-id prompt]
  (cond-> {:type       "ling"
           :name       (successor-name owner awaiter-id)
           :cwd        (:cwd owner)
           :spawn_mode (or (not-empty (:spawn-mode owner)) "hive-agent")
           :resume     {:run_id (:run-id owner) :prompt prompt}}
    (not-empty (:model owner))      (assoc :model (:model owner))
    (not-empty (:provider owner))   (assoc :provider (:provider owner))
    (not-empty (:parent owner))     (assoc :parent (:parent owner))
    (not-empty (:project-id owner)) (assoc :project_id (:project-id owner))
    (map? (:turn-budget owner))     (assoc :turn_budget (:turn-budget owner))))

(defmethod ports/open-resumer :hive-agent-run
  [owner]
  (when-let [explain (m/explain Owner owner)]
    (throw (ex-info "await: a hive-agent-run owner needs :run-id (chat-point run) and :cwd"
                    {:error   :await/invalid-owner
                     :owner   owner
                     :explain (pr-str (mapv #(select-keys % [:path :type]) (:errors explain)))})))
  (fn resume-run [{:keys [awaiter prompt]}]
    (let [params (spawn-params owner (:id awaiter) prompt)
          resp   ((resolve-spawn) params)]
      (when (:isError resp)
        (throw (ex-info (str "await: resume spawn refused: "
                             (str/join " " (keep :text (if (map? resp) [resp] resp))))
                        {:error :await/resume-refused :spawn params})))
      {:spawned (:name params) :run-id (:run-id owner)})))

(m/=> successor-name [:=> [:cat :map :any] :string])
(m/=> spawn-params [:=> [:cat Owner :any [:maybe :string]] :map])
