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

(:require [hive-spi.swarm.bootstrap :as spi]))

(def ISwarmBootstrap spi/ISwarmBootstrap)

(def -load-slaves spi/-load-slaves)
(def -snapshot-slave! spi/-snapshot-slave!)
(def -forget-slave! spi/-forget-slave!)
(def -close! spi/-close!)
