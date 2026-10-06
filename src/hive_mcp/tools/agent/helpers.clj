(ns hive-mcp.tools.agent.helpers
  "Shared helper functions for agent tool handlers."
  (:require [hive-mcp.tools.swarm.core :as swarm-core]
            [hive-mcp.agent.type-registry :as agent-type-registry]
            [hive-spi.editor.services :as svc]
            [taoensso.timbre :as log]
            [clojure.data.json :as json]
            [hive-mcp.tools.agent.reconcile :as reconcile]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn generate-agent-id
  "Generate unique agent ID with type prefix."
  [agent-type]
  (str (name agent-type) "-" (java.util.UUID/randomUUID)))

(defn format-agent
  "Format agent data for response, exposing unverified restore liveness."
  [agent-data]
  (when agent-data
    (let [base {:id (:slave/id agent-data)
                :status (:slave/status agent-data)
                :type (agent-type-registry/depth->agent-type (:slave/depth agent-data))
                :cwd (:slave/cwd agent-data)
                :project-id (:slave/project-id agent-data)}]
      (cond-> base
        (:slave/parent agent-data) (assoc :parent (:slave/parent agent-data))
        (:slave/grant agent-data) (assoc :grant (:slave/grant agent-data))
        (:slave/presets agent-data) (assoc :presets (:slave/presets agent-data))
        (:slave/created-at agent-data) (assoc :created-at (:slave/created-at agent-data))
        (:slave/liveness agent-data) (assoc :liveness (:slave/liveness agent-data))
        (:slave/orphan-reason agent-data) (assoc :reason (:slave/orphan-reason agent-data))
        (:slave/last-event-at agent-data) (assoc :last-event-at (:slave/last-event-at agent-data))))))

(defn format-agents
  "Format a list of agents for response."
  [agents]
  (let [formatted (->> agents
                       (map format-agent)
                       (remove nil?)
                       vec)]
    {:agents formatted
     :count (count formatted)
     :by-type (frequencies (map :type formatted))
     :by-status (frequencies (map :status formatted))}))

(defn query-elisp-lings
  "Query elisp for lings that may not be in DataScript. nil means unavailable,
   while an empty sequence means a successful, empty membership query."
  []
  (when (swarm-core/swarm-addon-available?)
    (let [{:keys [success result timed-out]}
          (svc/invoke :vessel :dispatch {:op :swarm/list-lings} 3000)]
      (when (and success (not timed-out))
        (try
          (let [parsed (json/read-str result :key-fn keyword)]
            (when (sequential? parsed)
              (->> parsed
                   (map (fn [ling]
                          {:slave/id (or (:slave-id ling) (:slave_id ling))
                           :slave/name (:name ling)
                           :slave/status (keyword (or (:status ling) "idle"))
                           :slave/depth 1
                           :slave/cwd (:cwd ling)
                           :slave/project-id (:project-id ling)
                           :slave/presets (:presets ling)}))
                   (filter :slave/id))))
          (catch Exception e
            (log/debug "Failed to parse elisp lings:" (ex-message e))
            nil))))))

(defn merge-with-elisp-lings
  "Merge DataScript agents with elisp lings, DataScript taking precedence.
   Elisp orphan rows are reconciled against `:probe` (an ILivenessEvidence,
   default: live registries); dead orphans appear only with `:include-stale?`."
  ([ds-agents] (merge-with-elisp-lings ds-agents {}))
  ([ds-agents {:keys [probe include-stale? elisp-lings]}]
   (try
     (let [elisp-lings (or elisp-lings (query-elisp-lings) [])
           ds-ids      (set (map :slave/id ds-agents))
           elisp-only  (remove #(ds-ids (:slave/id %)) elisp-lings)
           new-lings   (reconcile/reconcile (or probe (reconcile/live-evidence))
                                            elisp-only
                                            {:include-stale? include-stale?})]
       (log/debug "Merging agents: DataScript=" (count ds-agents)
                  "elisp-only=" (count new-lings))
       (concat ds-agents new-lings))
     (catch Exception e
       (log/warn "Failed to merge elisp lings (returning DataScript only):" (ex-message e))
       ds-agents))))
