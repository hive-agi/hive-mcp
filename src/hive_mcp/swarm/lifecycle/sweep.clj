(ns hive-mcp.swarm.lifecycle.sweep
  "Boot-time liveness sweep for swarm slave registry.

   Compat shim: moved to hive-agent.swarm.lifecycle.sweep in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-mcp.swarm.datascript.schema :as s]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.lifecycle.sweep" sym))

(defn- impl!
  "impl, after making sure the host adapters fill the swarm port slots.
   The adapters are resolved late: a static require would close a load
   cycle through the events adapter. A failure here (for instance a call
   made while that cycle is still loading) leaves the ports on their
   noops until a later call installs them."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

(defn check-pid-alive?
  {:arglists '([pid])}
  [& args]
  (apply (impl! 'check-pid-alive?) args))

;; Contracts transcribe hive-agent.swarm.lifecycle.sweep, where the behaviour
;; lives.
(def LivenessSignal
  "hive-system.process.liveness's LivenessSignal ADT value."
  [:map [:adt/type [:= :LivenessSignal]]
   [:adt/variant [:enum :liveness/alive :liveness/dead :liveness/unknown]]])

(def SweepResult
  "{:checked N :zombified M :alive K :unknown U}."
  [:map [:checked s/Count] [:zombified s/Count] [:alive s/Count] [:unknown s/Count]])

;; Any pid is read: nil or a non-integer is :liveness/unknown.
(m/=> check-pid-alive? [:=> [:cat :any] LivenessSignal])

(def default-stale-threshold-ms @(impl 'default-stale-threshold-ms))

(defn sweep-on-boot!
  {:arglists '([conn] [conn stale-threshold-ms])}
  [& args]
  (apply (impl! 'sweep-on-boot!) args))

(m/=> sweep-on-boot! [:function
                      [:=> [:cat s/Conn] SweepResult]
                      [:=> [:cat s/Conn s/Threshold] SweepResult]])

(defn sweep-once!
  {:arglists '([conn] [conn stale-threshold-ms])}
  [& args]
  (apply (impl! 'sweep-once!) args))

(m/=> sweep-once! [:function
                   [:=> [:cat s/Conn] SweepResult]
                   [:=> [:cat s/Conn s/Threshold] SweepResult]])
