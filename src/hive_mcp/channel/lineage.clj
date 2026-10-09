(ns hive-mcp.channel.lineage
  "Who spawned a shout's author, for rows that do not say.

   A ling's runtime telemetry (`bb-ling turn N`) is shouted without a
   :parent-id even when the ling has one, so the audience rules saw it as a
   root-level, project-scoped row and every coordinator sharing the project
   tree read it (HIVEMIND-PIGGYBACK-LEAK, kanban 20261008232949-5b0f7835).
   `attach-parents` fills the parent from the swarm registry before routing.

   `attach-parents` is pure; `registry-parent-of` is the boundary."
  (:require [clojure.string :as str]
            [hive-dsl.result :refer [rescue]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn attach-parents
  "MSGS with each row's missing :parent-id filled from (parent-of agent-id).
   A row that names a parent keeps it; a lookup that answers nil or blank adds
   nothing. Pure in `parent-of`."
  [parent-of msgs]
  (mapv (fn [{:keys [agent-id parent-id] :as msg}]
          (if (or (some? parent-id) (nil? agent-id))
            msg
            (let [p (parent-of agent-id)]
              (if (and (string? p) (not (str/blank? p)))
                (assoc msg :parent-id p)
                msg))))
        msgs))

(defn learned-parents
  "KNOWN agent -> parent map grown by every row in MSGS that names its parent.
   Pure."
  [known msgs]
  (into known
        (keep (fn [{:keys [agent-id parent-id]}]
                (when (and (string? agent-id) (string? parent-id) (not (str/blank? parent-id)))
                  [agent-id parent-id])))
        msgs))

(defonce ^{:private true
           :doc "agent-id -> parent, remembered from rows and registry hits, so a
                 ling's rows still route to its coordinator after the registry
                 has forgotten the ling."}
  known-parents
  (atom {}))

(defn registry-parent-of
  "agent-id -> spawning agent id: the live swarm registry, else the parent last
   seen for that agent. Memoized for one read; nil when neither knows."
  []
  (let [get-slave (rescue nil (requiring-resolve 'hive-mcp.swarm.datascript.queries/get-slave))]
    (memoize
     (fn [agent-id]
       (let [p (when get-slave (rescue nil (:slave/parent (get-slave agent-id))))]
         (if (and (string? p) (not (str/blank? p)))
           (do (swap! known-parents assoc agent-id p) p)
           (get @known-parents agent-id)))))))

(defn with-parents
  "MSGS with missing parents filled, after learning parents from every row in
   SEEN (the whole buffer, so a parent named once is remembered)."
  [seen msgs]
  (swap! known-parents learned-parents seen)
  (attach-parents (registry-parent-of) msgs))
