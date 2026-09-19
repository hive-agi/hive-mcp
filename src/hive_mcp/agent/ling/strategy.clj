(ns hive-mcp.agent.ling.strategy
  "ILingStrategy protocol for mode-specific spawn/dispatch/status/kill operations.

   Re-export: the protocol lives in hive-spi.swarm.ling-strategy, shared with
   hive-agent. The aliases below are the same protocol and method objects."
  (:require [hive-spi.swarm.ling-strategy :as spi]))

(def ILingStrategy spi/ILingStrategy)
(def strategy-spawn! spi/strategy-spawn!)
(def strategy-dispatch! spi/strategy-dispatch!)
(def strategy-status spi/strategy-status)
(def strategy-kill! spi/strategy-kill!)
(def strategy-interrupt! spi/strategy-interrupt!)
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

