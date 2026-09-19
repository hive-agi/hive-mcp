(ns hive-mcp.tools.swarm.jvm.parser
  "Process output parsing for JVM process management via multimethod dispatch by OS.

   Compat shim: moved to hive-agent.swarm.tools.jvm.parser in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (requiring-resolve (symbol "hive-agent.swarm.tools.jvm.parser" (name sym))))

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

(defn parse-etime-to-minutes
  {:arglists '([etime])}
  [& args]
  (apply (impl! 'parse-etime-to-minutes) args))

(def parse-process-line @(impl 'parse-process-line))

(def parse-process-line-extended @(impl 'parse-process-line-extended))
