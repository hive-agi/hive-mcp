(ns hive-mcp.tools.swarm.jvm.classifier
  "JVM process discovery, classification, and swarm environment detection.

   Compat shim: moved to hive-agent.swarm.tools.jvm.classifier in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (requiring-resolve (symbol "hive-agent.swarm.tools.jvm.classifier" (name sym))))

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

(defn classify-jvm-process
  {:arglists '([{:keys [cmd pid], :as proc}])}
  [& args]
  (apply (impl! 'classify-jvm-process) args))

(defn classify-type
  {:arglists '([cmd])}
  [& args]
  (apply (impl! 'classify-type) args))

(defn discover-and-classify
  {:arglists '([])}
  [& args]
  (apply (impl! 'discover-and-classify) args))

(defn find-jvm-processes
  {:arglists '([])}
  [& args]
  (apply (impl! 'find-jvm-processes) args))

(defn get-all-process-parents
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-all-process-parents) args))

(defn get-process-swarm-info
  {:arglists '([pid])}
  [& args]
  (apply (impl! 'get-process-swarm-info) args))
