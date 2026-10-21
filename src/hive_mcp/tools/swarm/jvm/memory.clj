(ns hive-mcp.tools.swarm.jvm.memory
  "Memory usage monitoring and threshold checking for resource guard.

   Compat shim: moved to hive-agent.swarm.tools.jvm.memory in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.tools.jvm.memory" sym))

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

;; Value schemas for the shim contracts. They transcribe the docstrings and
;; bodies of hive-agent.swarm.tools.jvm.memory, where the behaviour lives.

(def MemInfo
  "RAM usage read from /proc/meminfo, in MB, with the used share rounded to
   a whole percent."
  [:map
   [:total-mb [:int {:min 0}]]
   [:used-mb :int]
   [:available-mb [:int {:min 0}]]
   [:percent-used :int]])

(def MemError
  "The answer when /proc/meminfo cannot be read. The message is the
   exception's, which may be nil."
  [:map [:error [:maybe :string]]])

(def MemReading
  "What the threshold checks read: a used percent and the MB still free."
  [:map [:percent-used number?] [:available-mb number?]])

(def MemoryStatus
  "Memory health against the threshold."
  [:enum :healthy :warning :critical])

(def Recommendation
  "The four sentences `spawn-recommendation` chooses between."
  [:enum "Safe to spawn new processes"
   "Memory high - consider enabling auto_cleanup"
   "Memory high - set cleanup_dry_run=false to actually kill orphans"
   "Capacity reached - wait for running tasks to complete"])

(defn get-memory-usage
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-memory-usage) args))

(m/=> get-memory-usage [:=> [:cat] [:or MemInfo MemError]])

(defn memory-high?
  {:arglists '([mem-info threshold-percent min-available-mb])}
  [& args]
  (apply (impl! 'memory-high?) args))

(m/=> memory-high? [:=> [:cat MemReading number? number?] :boolean])

(defn memory-status
  {:arglists '([mem-info threshold-percent min-available-mb])}
  [& args]
  (apply (impl! 'memory-status) args))

(m/=> memory-status [:=> [:cat MemReading number? number?] MemoryStatus])

(defn spawn-recommendation
  {:arglists '([can-spawn auto-cleanup cleanup-dry])}
  [& args]
  (apply (impl! 'spawn-recommendation) args))

;; The three flags are read for truthiness only; callers pass booleans or nil.
(m/=> spawn-recommendation
      [:=> [:cat [:maybe :boolean] [:maybe :boolean] [:maybe :boolean]] Recommendation])
