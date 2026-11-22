(ns hive-mcp.tools.swarm.jvm
  "JVM process management and resource guard tools for orphan cleanup and OOM prevention.

   Compat shim: moved to hive-agent.swarm.tools.jvm in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]
            [hive-mcp.tools.swarm.core :as core]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.tools.jvm" sym))

(defn- impl!
  "impl, after the host adapters are installed (see delegate/ensure-adapters!)."
  [sym]
  (delegate/ensure-adapters!)
  (impl sym))

(defn handle-jvm-cleanup
  {:arglists '([{:keys [min_age_minutes dry_run keep_types swarm_only true_orphans_only]}])}
  [& args]
  (apply (impl! 'handle-jvm-cleanup) args))

(defn handle-resource-guard
  {:arglists '([{:keys [ram_threshold min_available_mb auto_cleanup cleanup_dry_run]}])}
  [& args]
  (apply (impl! 'handle-resource-guard) args))

;; =============================================================================
;; Contracts
;; =============================================================================
;;
;; Both shims delegate to the hive-agent handler of the same name, an MCP
;; handler answering one text response.

(m/=> handle-jvm-cleanup [:=> [:cat [:maybe :map]] core/SwarmResponse])

(m/=> handle-resource-guard [:=> [:cat [:maybe :map]] core/SwarmResponse])
