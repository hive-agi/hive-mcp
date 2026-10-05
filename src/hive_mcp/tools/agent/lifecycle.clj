(ns hive-mcp.tools.agent.lifecycle
  "Agent lifecycle handlers: interrupt, cleanup, claims, collect, broadcast."
  (:require [hive-mcp.tools.core :refer [mcp-error mcp-json]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.tools.agent.reconcile :as reconcile]
            [hive-spi.editor.services :as svc]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.swarm.registry :as registry]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.tools.swarm.collect :as swarm-collect]
            [hive-mcp.tools.swarm.status :as swarm-status]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [hive-mcp.swarm.lifecycle.restore-liveness :as restore]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn handle-interrupt
  "Interrupt the current query/task of a running agent-sdk ling."
  [{:keys [agent_id]}]
  (if (str/blank? agent_id)
    (mcp-error "agent_id is required for interrupt")
    (let [result (ling/interrupt-ling! agent_id)]
      (if (:success? result)
        (mcp-json result)
        (mcp-error (str "Interrupt failed: " (str/join ", " (:errors result))))))))

(defn- reap-orphaned-elisp-lings!
  "Drop from Emacs' slave table every ling reconciled `:orphaned`: its
   registry row is retired, so no loop in this JVM backs it. `kill-fn` takes
   a slave id and answers truthy when Emacs forgot it. Returns reaped ids."
  [elisp-lings probe kill-fn]
  (let [ghosts (->> (reconcile/reconcile probe elisp-lings {:include-stale? true})
                    (filter reconcile/orphaned-row?)
                    (map :slave/id))]
    (vec (for [id ghosts
               :when (try (kill-fn id)
                          (catch Exception e
                            (log/warn "Reaping orphaned ling failed" {:slave-id id :error (ex-message e)})
                            false))]
           (do (log/info "Reaped orphaned ling from Emacs" {:slave-id id})
               id)))))

(defn- emacs-forget!
  [slave-id]
  (:success (svc/invoke :vessel :dispatch {:op :swarm/kill :slave-id slave-id} 5000)))

(defn handle-cleanup
  "Reconcile DataScript registry with actual Emacs state, removing orphan
   agents, and drop Emacs-side lings orphaned by a JVM restart (their
   registry row is retired and no loop in this JVM backs them)."
  [_params]
  (try
    (let [ds-agents (queries/get-all-slaves)
          elisp-lings (or (helpers/query-elisp-lings) [])
          elisp-ids (set (map :slave/id elisp-lings))
          orphan-lings (->> ds-agents
                            (filter #(= 1 (:slave/depth %)))
                            (filter #(restore/missing-from-emacs? (:slave/id %) elisp-ids))
                            (map :slave/id))
          removed (doall
                   (for [slave-id orphan-lings]
                     (do
                       (log/info "Removing orphan ling from DataScript" {:slave-id slave-id})
                       (registry/remove-slave! slave-id)
                       slave-id)))
          reaped (reap-orphaned-elisp-lings! elisp-lings (reconcile/live-evidence) emacs-forget!)]
      (log/info "Cleanup completed" {:orphans-removed (count removed)
                                     :orphaned-reaped (count reaped)
                                     :ds-total (count ds-agents)
                                     :elisp-lings (count elisp-lings)})
      (mcp-json {:success true
                 :orphans-removed (count removed)
                 :removed-ids (vec removed)
                 :orphaned-reaped (count reaped)
                 :reaped-ids reaped
                 :ds-agents-before (count ds-agents)
                 :elisp-lings-found (count elisp-lings)}))
    (catch Exception e
      (log/error "Cleanup failed" {:error (ex-message e)})
      (mcp-error (str "Cleanup failed: " (ex-message e))))))

(defn handle-claims
  "Get file ownership claims for an agent or all agents."
  [{:keys [agent_id]}]
  (try
    (if agent_id
      (let [logic-claims (logic/get-all-claims)
            agent-claims (->> logic-claims
                              (filter #(= agent_id (:slave-id %)))
                              (mapv (fn [{:keys [file slave-id]}]
                                      {:file file :owner slave-id})))]
        (mcp-json {:agent-id agent_id
                   :claims agent-claims
                   :count (count agent-claims)}))
      (let [all-claims (logic/get-all-claims)
            formatted (->> all-claims
                           (mapv (fn [{:keys [file slave-id]}]
                                   {:file file :owner slave-id})))]
        (mcp-json {:claims formatted
                   :count (count formatted)
                   :by-owner (frequencies (map :owner formatted))})))
    (catch Exception e
      (log/error "Failed to get claims" {:error (ex-message e)})
      (mcp-error (str "Failed to get claims: " (ex-message e))))))

(defn latest-task-id
  "Pure: id of the most recently started task in `tasks` (swarm task rows
   {:task/id :task/started-at}), or nil."
  [tasks]
  (some->> (seq tasks)
           (sort-by #(some-> ^java.util.Date (:task/started-at %) .getTime) #(compare %2 %1))
           first
           :task/id))

(defn- registry-tasks-for
  "Boundary: the swarm registry's task rows for one agent."
  [agent-id]
  (queries/get-tasks-for-slave agent-id))

(defn handle-collect
  "Collect response from a dispatched task.

   task_id names the task; with agent_id alone, the agent's most recently
   started task is collected. `tasks-for` is the task-lookup port
   (fn [agent-id] -> task rows), the swarm registry by default."
  ([params] (handle-collect registry-tasks-for params))
  ([tasks-for {:keys [task_id agent_id] :as params}]
   (cond
     (not (str/blank? (str task_id)))
     (swarm-collect/handle-swarm-collect params)

     (str/blank? (str agent_id))
     (mcp-error "task_id or agent_id is required")

     :else
     (if-let [tid (latest-task-id (tasks-for agent_id))]
       (swarm-collect/handle-swarm-collect (assoc params :task_id tid))
       (mcp-error (str "No dispatched task recorded for agent " agent_id
                       "; pass task_id, or read the run with transcript report"))))))

(defn handle-broadcast
  "Broadcast a prompt to all active lings."
  [{:keys [prompt] :as params}]
  (if (empty? prompt)
    (mcp-error "prompt is required")
    (swarm-status/handle-swarm-broadcast params)))
