(ns hive-mcp.agent.ling.spawn-store
  "Spawn-time Ling registration port.

   The protocol and its slot live in hive-spi.swarm.spawn-store; the aliases
   below are the same objects. This namespace holds the DataScript default,
   installed on first `get-store` when nothing else is."
  (:require [hive-spi.swarm.spawn-store :as spi]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-mcp.channel.audience :as audience]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ISpawnStore spi/ISpawnStore)
(def add-slave! spi/add-slave!)
(def remove-slave! spi/remove-slave!)
(def update-slave! spi/update-slave!)
(def claims-for-slave spi/claims-for-slave)

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

(defn set-store!
  "Install a spawn registration store. Intended for addons/tests that provide a
   non-DataScript implementation."
  [store]
  (spi/set-store! store))

(defn get-store
  "The installed spawn registration store, installing the DataScript default
   when none is."
  []
  (or (spi/get-store)
      (spi/set-store! (->DataScriptSpawnStore))))

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
