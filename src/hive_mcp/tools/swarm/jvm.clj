(ns hive-mcp.tools.swarm.jvm
  "JVM process management and resource guard tools for orphan cleanup and OOM prevention.

   Compat shim: moved to hive-agent.swarm.tools.jvm in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (requiring-resolve (symbol "hive-agent.swarm.tools.jvm" (name sym))))

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

(defn handle-jvm-cleanup
  {:arglists '([{:keys [min_age_minutes dry_run keep_types swarm_only true_orphans_only]}])}
  [& args]
  (apply (impl! 'handle-jvm-cleanup) args))

(defn handle-resource-guard
  {:arglists '([{:keys [ram_threshold min_available_mb auto_cleanup cleanup_dry_run]}])}
  [& args]
  (apply (impl! 'handle-resource-guard) args))
