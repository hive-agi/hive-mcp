(ns hive-mcp.agent.ling.spawn-store
  "Spawn-time Ling registration port.

   The default implementation delegates to the current swarm store, but callers
   depend on this protocol so spawn orchestration is not coupled to a concrete
   DataScript backend."
  (:require [hive-mcp.protocols.registry :as reg]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.channel.audience :as audience]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol ISpawnStore
  (add-slave! [this slave-id attrs])
  (remove-slave! [this slave-id])
  (update-slave! [this slave-id updates])
  (claims-for-slave [this slave-id]))

(defrecord DataScriptSpawnStore []
  ISpawnStore
  (add-slave! [_ slave-id attrs]
    (ds-lings/add-slave! slave-id attrs))

  (remove-slave! [_ slave-id]
    (ds-lings/remove-slave! slave-id))

  (update-slave! [_ slave-id updates]
    (ds-lings/update-slave! slave-id updates))

  (claims-for-slave [_ slave-id]
    (->> (ds-queries/get-all-claims)
         (filter #(= slave-id (:slave-id %)))
         (map :file)
         vec)))

(defonce ^:private slot
  (reg/single-slot {:validate #(satisfies? ISpawnStore %)
                    :initial (->DataScriptSpawnStore)}))

(defn set-store!
  "Install a spawn registration store. Intended for addons/tests that provide a
   non-DataScript implementation."
  [store]
  (reg/install! slot store))

(defn get-store
  []
  (reg/current slot))

(defn ensure-coordinator-session!
  "Give a coordinator SESSION (`coordinator:<session>`) a depth-0 row in
   `store` when it has none, so a spawn parented to it can be written:
   `:slave/parent` is a lookup ref and resolves only against an existing
   row. A bare `coordinator`, a ling id, nil, or a session that already has
   a row is left alone. -> the id when a row was added, else nil."
  [store parent-id]
  (when (and (string? parent-id)
             (audience/coordinator-session parent-id)
             (nil? (ds-queries/get-slave parent-id)))
    (add-slave! store parent-id {:status :idle :depth 0})
    parent-id))

(defn reset-store!
  "Restore the default swarm-backed store. Intended for tests."
  []
  (set-store! (->DataScriptSpawnStore)))
