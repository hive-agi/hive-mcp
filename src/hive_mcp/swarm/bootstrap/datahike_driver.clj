;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.bootstrap.datahike-driver
  "Late-bound facade over datahike.api for the swarm bootstrap store.

   Compat shim: moved to hive-agent.swarm.bootstrap.datahike-driver in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent.")

(defn- impl [sym]
  (requiring-resolve (symbol "hive-agent.swarm.bootstrap.datahike-driver" (name sym))))

(defn create-database {:arglists '([& args])} [& args] (apply (impl 'create-database) args))
(defn connect {:arglists '([& args])} [& args] (apply (impl 'connect) args))
(defn transact {:arglists '([& args])} [& args] (apply (impl 'transact) args))
(defn q {:arglists '([& args])} [& args] (apply (impl 'q) args))
(defn pull {:arglists '([& args])} [& args] (apply (impl 'pull) args))
(defn db {:arglists '([& args])} [& args] (apply (impl 'db) args))
(defn release {:arglists '([& args])} [& args] (apply (impl 'release) args))
(defn database-exists? {:arglists '([& args])} [& args] (apply (impl 'database-exists?) args))
