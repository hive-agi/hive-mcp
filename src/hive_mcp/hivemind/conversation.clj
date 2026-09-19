(ns hive-mcp.hivemind.conversation
  "Inter-Ling Conversation Protocol — pure protocol layer (Calculations).

   Compat shim: moved to hive-agent.swarm.hivemind.conversation in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.hivemind.conversation" sym))

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

(defn ask-envelope
  {:arglists '([{:keys [from to question ask-id project-id data timestamp], :or {timestamp (now-ms)}}])}
  [& args]
  (apply (impl! 'ask-envelope) args))

(defn await-response!
  {:arglists '([ask-id & {:keys [timeout-ms], :or {timeout-ms default-ask-timeout-ms}}])}
  [& args]
  (apply (impl! 'await-response!) args))

(defn build-ask!
  {:arglists '([{:keys [from to question project-id data timeout-ms], :as opts}])}
  [& args]
  (apply (impl! 'build-ask!) args))

(defn cancel-ask!
  {:arglists '([ask-id])}
  [& args]
  (apply (impl! 'cancel-ask!) args))

(defn clear-pending!
  {:arglists '([])}
  [& args]
  (apply (impl! 'clear-pending!) args))

(def default-ask-timeout-ms @(impl 'default-ask-timeout-ms))

(defn deliver-response!
  {:arglists '([ask-id answer])}
  [& args]
  (apply (impl! 'deliver-response!) args))

(defn pending-ask
  {:arglists '([ask-id])}
  [& args]
  (apply (impl! 'pending-ask) args))

(def pending-asks @(impl 'pending-asks))

(defn pending-count
  {:arglists '([])}
  [& args]
  (apply (impl! 'pending-count) args))

(defn register-ask!
  {:arglists '([{:keys [ask-id from to question]}])}
  [& args]
  (apply (impl! 'register-ask!) args))

(defn respond-envelope
  {:arglists '([{:keys [from to ask-id answer project-id data timestamp], :or {timestamp (now-ms)}}])}
  [& args]
  (apply (impl! 'respond-envelope) args))

(defn tell-envelope
  {:arglists '([{:keys [from to message project-id data timestamp], :or {timestamp (now-ms)}}])}
  [& args]
  (apply (impl! 'tell-envelope) args))
