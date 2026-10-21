(ns hive-mcp.swarm.bootstrap.protocol
  "ISwarmBootstrap — abstraction over the source-of-truth used to rehydrate
   the in-memory swarm registry at startup and to persist slave identity
   across process restarts.

   Design (SOLID/DDD/FP):
   - ISP: 4 methods, single concern (bootstrap + durable slave identity).
   - DIP: hive-mcp.swarm.sync depends on THIS, not on emacsclient or datahike.
   - OCP: new backends added as records; no edits to consumers.
   - DDD: 'bootstrap' is the Repository boundary for the Slave aggregate's
     persistent projection — NOT a general-purpose CRUD store. Transient
     state (tasks, claims) stays in the in-memory Datascript registry.
   - FP: methods return data; side effects confined to records; -close!
     runs in halt order.

   Compat shim; the protocol lives in hive-spi.swarm.bootstrap."
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(:require [hive-spi.swarm.bootstrap :as spi]
          [malli.core :as m]))

(def ISwarmBootstrap spi/ISwarmBootstrap)

(def -load-slaves spi/-load-slaves)
(def -snapshot-slave! spi/-snapshot-slave!)
(def -forget-slave! spi/-forget-slave!)
(def -close! spi/-close!)

(def ^:private null-bootstrap
  "A generator witness for Bootstrap: loads nothing, keeps nothing."
  (reify spi/ISwarmBootstrap
    (-load-slaves [_] [])
    (-snapshot-slave! [this _ _] this)
    (-forget-slave! [this _] this)
    (-close! [_] nil)))

(def Bootstrap
  "Any ISwarmBootstrap implementation (the factory's, a record, a reify)."
  (m/schema [:fn {:gen/elements [null-bootstrap]}
             #(satisfies? spi/ISwarmBootstrap %)]))

(def SlaveRow
  "One slave as a bootstrap source hands it back for re-registration."
  [:map
   [:slave-id :string]
   [:name {:optional true} [:maybe :string]]
   [:status {:optional true} [:maybe [:or :keyword :string]]]
   [:depth {:optional true} [:maybe :int]]
   [:cwd {:optional true} [:maybe :string]]
   [:project-id {:optional true} [:maybe :string]]
   [:parent-id {:optional true} [:maybe :string]]
   [:spawn-mode {:optional true} [:maybe :keyword]]
   [:process-pid {:optional true} [:maybe :int]]])
