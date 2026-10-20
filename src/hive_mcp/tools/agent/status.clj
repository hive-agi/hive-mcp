(ns hive-mcp.tools.agent.status
  "Agent status query handler with DataScript and elisp fallback."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.agent.type-registry :as agent-type-registry]
            [hive-mcp.swarm.datascript.queries :as queries]
            [taoensso.timbre :as log]
            [hive-mcp.swarm.digest :as digest]
            [hive-mcp.hivemind.state :as hm-state]
            [hive-mcp.agent.ling.headless-registry :as headless-registry]
            [hive-spi.addon.headless :as headless]
            [malli.core :as m]
            [hive-mcp.tools.result-bridge :as rb]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn annotate-liveness
  "Rows with `:slave/liveness-age-ms` set from `status-fn`, a function of one
   row returning the backend status map (or nil). Only a non-negative number
   is copied; a row the backend does not know, a status without an age and a
   status function that throws leave the row untouched. Pure given
   `status-fn`.

   A registered headless backend (hive-agent's agentic loop) reports how long
   ago its loop last made progress as `:liveness-age-ms` in its
   `headless-status`. The registry row alone says `:working` for as long as a
   dispatch has not finished, so a ling whose loop stalled kept reading
   \"working\"; copying the age onto the row lets `format-agent` show it."
  [status-fn rows]
  (mapv (fn [row]
          (let [age (try (:liveness-age-ms (status-fn row))
                         (catch Exception e
                           (log/debug "liveness status failed:" (ex-message e))
                           nil))]
            (if (and (number? age) (not (neg? age)))
              (assoc row :slave/liveness-age-ms age)
              row)))
        rows))

(defn backend-status-fn
  "Boundary: a row -> the headless status of its backend, or nil when the
   row's `:ling/spawn-mode` names no registered backend. `backend-for` maps a
   spawn-mode keyword to an IHeadlessBackend (default: the headless registry).
   Stratified: `annotate-liveness` is the pure calculation, this is the
   boundary that resolves the status function from the headless registry."
  ([] (backend-status-fn headless-registry/get-headless-backend))
  ([backend-for]
   (fn [row]
     (when-let [backend (some-> (:ling/spawn-mode row) backend-for)]
       (headless/headless-status backend {:id (:slave/id row)} nil)))))

(defn handle-status
  "Get agent status, optionally filtered by agent_id, type, or project_id.
   Stale/zombie rows, and orphan rows with no live agent behind them, are
   hidden by default; pass include_stale=true for diagnostics.

   A ling on a registered headless backend also carries `:liveness-age-ms`,
   the time since its loop last made progress, so a stalled ling no longer
   reads as plain \"working\"."
  [{:keys [agent_id type project_id include_stale]}]
  (let [eid    (when (and agent_id (not= agent_id "coordinator")) agent_id)
        stale? (boolean include_stale)
        merge-elisp #(helpers/merge-with-elisp-lings % {:include-stale? stale?})
        status-fn (backend-status-fn)
        with-age  #(annotate-liveness status-fn %)]
    (try
      (cond
        eid
        (if-let [agent-data (queries/get-slave eid)]
          (mcp-json {:agent (helpers/format-agent (first (with-age [agent-data])))})
          (mcp-error (str "Agent not found: " eid)))
        type
        (let [agent-type (keyword type)
              depth (when (agent-type-registry/valid-type? agent-type)
                      (agent-type-registry/type-depth agent-type))
              all-agents (if project_id
                           (queries/get-slaves-by-project project_id :include-stale? stale?)
                           (if (= agent-type :ling)
                             (merge-elisp (queries/get-all-slaves :include-stale? stale?))
                             (queries/get-all-slaves :include-stale? stale?)))
              filtered (if depth
                         (filter #(= depth (:slave/depth %)) all-agents)
                         all-agents)]
          (mcp-json (helpers/format-agents (with-age filtered))))
        project_id
        (mcp-json (helpers/format-agents (with-age (queries/get-slaves-by-project project_id :include-stale? stale?))))
        :else
        (mcp-json (helpers/format-agents (with-age (merge-elisp (queries/get-all-slaves :include-stale? stale?))))))
      (catch Exception e
        (log/error "Failed to get agent status" {:error (ex-message e)})
        (mcp-error (str "Failed to get status: " (ex-message e)))))))

(defn handle-digest
  "Compact per-agent status rows projected from the hivemind shout ring.

   The PULL side of the audience change. Shouts now reach only their spawner
   and a per-turn burst collapses to one rollup, so the running picture is no
   longer pushed into everybody's context — this is where you ask for it.

   Params:
     :agent_id — restrict to that agent's OWN children
     :verbose  — also return the rendered one-line-per-agent text

   Rows: {:a agent :e last-event :m last-message :t task :turn n
          :parent spawner :idle-s seconds :shouts ring-depth}, freshest first."
  [{:keys [agent_id verbose]}]
  (try
    (let [registry @(:atom hm-state/agent-registry)
          rows (digest/roster (System/currentTimeMillis) registry
                              {:only-children-of agent_id})]
      (mcp-json
       (cond-> {:agents rows
                :count (count rows)
                :active (count (filter #(< (:idle-s %) 60) rows))}
         verbose (assoc :rendered (digest/render rows)))))
    (catch Exception e
      (log/warn "agent digest failed:" (.getMessage e))
      (mcp-error (str "Digest failed: " (.getMessage e))))))
