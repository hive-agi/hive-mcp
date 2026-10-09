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

(defn registry-parent-of
  "agent-id -> spawning agent id from the swarm registry, memoized for one
   read. nil for an unknown agent or when the registry is absent."
  []
  (let [get-slave (rescue nil (requiring-resolve 'hive-mcp.swarm.datascript.queries/get-slave))]
    (memoize
     (fn [agent-id]
       (when get-slave
         (rescue nil (:slave/parent (get-slave agent-id))))))))
