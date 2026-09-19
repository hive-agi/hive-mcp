(ns hive-mcp.hivemind.state
  "Hivemind state atoms and direct accessors.

   Compat shim: moved to hive-agent.swarm.hivemind.state in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.hivemind.state" sym))

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

(defn add-swarm-prompt!
  {:arglists '([slave-id prompt-text session-id timestamp])}
  [& args]
  (apply (impl! 'add-swarm-prompt!) args))

(def agent-registry @(impl 'agent-registry))

(defn clear-agent-messages!
  {:arglists '([agent-id])}
  [& args]
  (apply (impl! 'clear-agent-messages!) args))

(defn clear-agent-registry!
  {:arglists '([])}
  [& args]
  (apply (impl! 'clear-agent-registry!) args))

(defn clear-ling-results!
  {:arglists '([])}
  [& args]
  (apply (impl! 'clear-ling-results!) args))

(defn get-pending-ling-results
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-pending-ling-results) args))

(defn get-swarm-prompts
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-swarm-prompts) args))

(def ling-results @(impl 'ling-results))

(defn mark-ling-reviewed!
  {:arglists '([agent-id])}
  [& args]
  (apply (impl! 'mark-ling-reviewed!) args))

(def pending-asks @(impl 'pending-asks))

(def pending-swarm-prompts @(impl 'pending-swarm-prompts))

(defn record-ling-result!
  {:arglists '([agent-id result])}
  [& args]
  (apply (impl! 'record-ling-result!) args))

(defn remove-ling-result!
  {:arglists '([agent-id])}
  [& args]
  (apply (impl! 'remove-ling-result!) args))

(defn remove-swarm-prompt!
  {:arglists '([slave-id])}
  [& args]
  (apply (impl! 'remove-swarm-prompt!) args))
