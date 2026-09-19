(ns hive-mcp.swarm.logic
  "Logic programming engine for swarm hivemind coordination.

   Compat shim: moved to hive-agent.swarm.logic in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.logic" sym))

(defn add-claim!
  {:arglists '([file-path slave-id])}
  [& args]
  (apply (impl 'add-claim!) args))

(defn add-slave!
  {:arglists '([slave-id status])}
  [& args]
  (apply (impl 'add-slave!) args))

(defn add-task-file!
  {:arglists '([task-id file-path])}
  [& args]
  (apply (impl 'add-task-file!) args))

(defn check-file-conflicts
  {:arglists '([requesting-slave files])}
  [& args]
  (apply (impl 'check-file-conflicts) args))

(defn check-would-deadlock
  {:arglists '([task-id dep-task-id])}
  [& args]
  (apply (impl 'check-would-deadlock) args))

(defn db-stats
  {:arglists '([])}
  [& args]
  (apply (impl 'db-stats) args))

(defn get-all-claims
  {:arglists '([])}
  [& args]
  (apply (impl 'get-all-claims) args))

(defn get-claim-for-file
  {:arglists '([file-path])}
  [& args]
  (apply (impl 'get-claim-for-file) args))

(defn get-files-for-task
  {:arglists '([task-id])}
  [& args]
  (apply (impl 'get-files-for-task) args))

(defn get-logic-db-atom
  {:arglists '([])}
  [& args]
  (apply (impl 'get-logic-db-atom) args))

(defn release-claim-for-file!
  {:arglists '([file-path])}
  [& args]
  (apply (impl 'release-claim-for-file!) args))

(defn release-claims-for-slave!
  {:arglists '([slave-id])}
  [& args]
  (apply (impl 'release-claims-for-slave!) args))

(defn release-claims-for-task!
  {:arglists '([task-id])}
  [& args]
  (apply (impl 'release-claims-for-task!) args))

(defn remove-claim!
  {:arglists '([file-path slave-id])}
  [& args]
  (apply (impl 'remove-claim!) args))

(defn reset-db!
  {:arglists '([])}
  [& args]
  (apply (impl 'reset-db!) args))

(defn slave-exists?
  {:arglists '([slave-id])}
  [& args]
  (apply (impl 'slave-exists?) args))
