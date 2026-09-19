(ns hive-mcp.swarm.digest
  "Compact swarm status rows projected from the hivemind shout ring.

   Compat shim: moved to hive-agent.swarm.digest in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.digest" sym))

(defn agent-row
  {:arglists '([now agent-id entry] [now agent-id entry cap])}
  [& args]
  (apply (impl 'agent-row) args))

(def default-message-cap @(impl 'default-message-cap))

(defn render
  {:arglists '([rows])}
  [& args]
  (apply (impl 'render) args))

(defn roster
  {:arglists '([now registry] [now registry {:keys [only-children-of message-cap]}])}
  [& args]
  (apply (impl 'roster) args))
