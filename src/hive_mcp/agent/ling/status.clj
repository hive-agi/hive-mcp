(ns hive-mcp.agent.ling.status
  "Status/query operations for Ling agents: lookups from DataScript and
   interrupt. Split from `hive-mcp.agent.ling` (hotspot #20)."
  (:require [hive-mcp.agent.ling.lifecycle :as lifecycle]
            [hive-mcp.agent.ling.spawn :as spawn]
            [hive-mcp.agent.ling.strategy :as strategy]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn get-ling
  "Get a ling by ID as a Ling record from DataScript."
  [id]
  (when-let [slave (ds-queries/get-slave id)]
    (spawn/->ling id (spawn/slave->ling-opts slave))))

(m/=> get-ling [:=> [:cat :any] [:maybe spawn/LingValue]])

(defn list-lings
  "List all lings, optionally filtered by project-id."
  [& [project-id]]
  (let [slaves (if project-id
                 (ds-queries/get-slaves-by-project project-id)
                 (ds-queries/get-all-slaves))]
    (->> slaves
         (filter #(= 1 (:slave/depth %)))
         (map #(spawn/->ling (:slave/id %) (spawn/slave->ling-opts %))))))

(m/=> list-lings [:=> [:cat [:? [:maybe :string]]] [:sequential spawn/LingValue]])

(defn get-ling-for-task
  "Get the ling assigned to a kanban task."
  [kanban-task-id]
  (when-let [slave (ds-queries/get-slave-by-kanban-task kanban-task-id)]
    (spawn/->ling (:slave/id slave) (spawn/slave->ling-opts slave))))

(m/=> get-ling-for-task [:=> [:cat :any] [:maybe spawn/LingValue]])

(defn interrupt-ling!
  "Interrupt the current query/task of a running ling."
  [ling-id]
  (if-let [ling (get-ling ling-id)]
    (let [mode (or (:spawn-mode ling)
                   (when-let [slave (ds-queries/get-slave ling-id)]
                     (:ling/spawn-mode slave))
                   :claude)
          strat (lifecycle/resolve-strategy mode)]
      (strategy/strategy-interrupt! strat (lifecycle/ling-ctx ling)))
    {:success? false
     :ling-id ling-id
     :errors [(str "Ling not found: " ling-id)]}))

;; A known ling answers whatever its strategy's interrupt answers; an unknown
;; one answers {:success? false :ling-id :errors}.
(m/=> interrupt-ling! [:=> [:cat :any] :map])
