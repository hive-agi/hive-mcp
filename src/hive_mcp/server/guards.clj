(ns hive-mcp.server.guards
  "Process-role guards for the swarm.

   Compat shim: the guards live in hive-spi.swarm.guards, shared by hive-mcp
   and hive-agent so both read the same coordinator-running state. Every
   hive-mcp caller requires hive-spi.swarm.guards directly; this namespace
   remains for external requirers (hive-claude). The dynamic vars are not
   re-exported: bind or read them on hive-spi.swarm.guards."
  (:require [hive-spi.swarm.guards :as spi]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def child-ling? spi/child-ling?)
(def coordinator? spi/coordinator?)
(def get-role spi/get-role)
(def ling-depth spi/ling-depth)
(def child-ling-env spi/child-ling-env)
(def set-enforcement-mode! spi/set-enforcement-mode!)
(def enable-guards! spi/enable-guards!)
(def disable-guards! spi/disable-guards!)
(def guard-status spi/guard-status)
(def coordinator-running? spi/coordinator-running?)
(def mark-coordinator-running! spi/mark-coordinator-running!)
(def mark-coordinator-stopped! spi/mark-coordinator-stopped!)

(def ^{:macro true :arglists '([msg & body])} when-not-coordinator
  @#'spi/when-not-coordinator)
