(ns hive-mcp.tools.swarm.jvm.summary
  "Pure summary builders for JVM management operations.

   Compat shim: moved to hive-agent.swarm.tools.jvm.summary in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (requiring-resolve (symbol "hive-agent.swarm.tools.jvm.summary" (name sym))))

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

(defn build-cleanup-summary
  {:arglists '([all-procs classified all-classified orphans killed-pids {:keys [dry-run swarm-only min-age true-orphans-only]}])}
  [& args]
  (apply (impl! 'build-cleanup-summary) args))

(defn build-resource-guard-summary
  {:arglists '([can-spawn initial-mem final-mem threshold min-available initial-high? final-high? {:keys [auto-clean cleanup-dry]} cleanup-data])}
  [& args]
  (apply (impl! 'build-resource-guard-summary) args))
