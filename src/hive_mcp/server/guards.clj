(ns hive-mcp.server.guards
  "Process-role guards for the swarm.

   Compat shim: the guards live in hive-spi.swarm.guards, shared by hive-mcp
   and hive-agent so both read the same coordinator-running state. Every
   hive-mcp caller requires hive-spi.swarm.guards directly; this namespace
   remains for external requirers (hive-claude). The dynamic vars are not
   re-exported: bind or read them on hive-spi.swarm.guards.

   Each re-export is a delegating `defn` that calls through the hive-spi var,
   never a `def` of its value, so a reload or redef of the owner is seen by
   every caller of this name (card 20261116190025-31139c15)."
  (:require [hive-spi.swarm.guards :as spi]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn child-ling? [] (spi/child-ling?))
(defn coordinator? [] (spi/coordinator?))
(defn get-role [] (spi/get-role))
(defn ling-depth [] (spi/ling-depth))
(defn child-ling-env [] (spi/child-ling-env))
(defn set-enforcement-mode! [mode] (spi/set-enforcement-mode! mode))
(defn enable-guards! [] (spi/enable-guards!))
(defn disable-guards! [] (spi/disable-guards!))
(defn guard-status [] (spi/guard-status))
(defn coordinator-running? [] (spi/coordinator-running?))
(defn mark-coordinator-running! [] (spi/mark-coordinator-running!))
(defn mark-coordinator-stopped! [] (spi/mark-coordinator-stopped!))

(def ^{:macro true :arglists '([msg & body])} when-not-coordinator
  @#'spi/when-not-coordinator)

;; =============================================================================
;; Contracts
;; =============================================================================
;; This shim is the surface its external requirers call, and the contract pins
;; what they receive.

(def ChildEnv
  [:map {:closed true}
   ["HIVE_MCP_ROLE" [:= "child-ling"]]
   ["HIVE_LING_DEPTH" [:re #"^\d+$"]]])

(def GuardStatus
  [:map
   [:enabled? :boolean]
   [:mode [:enum :warn :block]]
   [:child-ling? :boolean]
   [:role :string]
   [:ling-depth :int]
   [:agent-id [:maybe :string]]
   [:coordinator-running? :boolean]])

(m/=> child-ling? [:=> [:cat] :boolean])
(m/=> coordinator? [:=> [:cat] :boolean])
(m/=> get-role [:=> [:cat] :string])
(m/=> ling-depth [:=> [:cat] :int])
(m/=> child-ling-env [:=> [:cat] ChildEnv])
(m/=> set-enforcement-mode! [:=> [:cat :any] :nil])
(m/=> enable-guards! [:=> [:cat] :nil])
(m/=> disable-guards! [:=> [:cat] :nil])
(m/=> guard-status [:=> [:cat] GuardStatus])
(m/=> coordinator-running? [:=> [:cat] :boolean])
(m/=> mark-coordinator-running! [:=> [:cat] :nil])
(m/=> mark-coordinator-stopped! [:=> [:cat] :nil])
