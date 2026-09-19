(ns hive-mcp.swarm.logic.predicates
  "Pure core.logic predicates and relations for swarm coordination.

   Compat shim: moved to hive-agent.swarm.logic.predicates in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (requiring-resolve (symbol "hive-agent.swarm.logic.predicates" (name sym))))

(defn claims
  {:arglists '([& args])}
  [& args]
  (apply (impl 'claims) args))

(defn depends-on
  {:arglists '([& args])}
  [& args]
  (apply (impl 'depends-on) args))

(defn file-conflicto
  {:arglists '([file-path requesting-slave conflicting-slave])}
  [& args]
  (apply (impl 'file-conflicto) args))

(defn reachable-fromo
  {:arglists '([source target])}
  [& args]
  (apply (impl 'reachable-fromo) args))

(defn slave
  {:arglists '([& args])}
  [& args]
  (apply (impl 'slave) args))

(defn task
  {:arglists '([& args])}
  [& args]
  (apply (impl 'task) args))

(defn task-completedo
  {:arglists '([task-id])}
  [& args]
  (apply (impl 'task-completedo) args))

(defn task-files
  {:arglists '([& args])}
  [& args]
  (apply (impl 'task-files) args))

(defn task-pendingo
  {:arglists '([task-id])}
  [& args]
  (apply (impl 'task-pendingo) args))

(defn would-deadlocko
  {:arglists '([task-a task-b])}
  [& args]
  (apply (impl 'would-deadlocko) args))
