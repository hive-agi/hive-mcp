(ns hive-mcp.agora.schema
  "DataScript schema and CRUD for Agora multi-ling dialogue system.

   Compat shim: moved to hive-agent.swarm.agora.schema in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (requiring-resolve (symbol "hive-agent.swarm.agora.schema" (name sym))))

(def dialogue-statuses @(impl 'dialogue-statuses))

(def turn-signals @(impl 'turn-signals))

(def schema @(impl 'schema))

(defn create-conn
  {:arglists '([])}
  [& args]
  (apply (impl 'create-conn) args))

(defn get-conn
  {:arglists '([])}
  [& args]
  (apply (impl 'get-conn) args))

(defn reset-conn!
  {:arglists '([])}
  [& args]
  (apply (impl 'reset-conn!) args))

(defn ensure-conn
  {:arglists '([])}
  [& args]
  (apply (impl 'ensure-conn) args))

(defn now
  {:arglists '([])}
  [& args]
  (apply (impl 'now) args))

(defn gen-id
  {:arglists '([])}
  [& args]
  (apply (impl 'gen-id) args))

(defn config->edn
  {:arglists '([config])}
  [& args]
  (apply (impl 'config->edn) args))

(defn edn->config
  {:arglists '([edn-str])}
  [& args]
  (apply (impl 'edn->config) args))

(defn create-dialogue!
  {:arglists '([{:keys [id name participants config], :or {config {:threshold 0.8, :timeout-ms 300000}}}])}
  [& args]
  (apply (impl 'create-dialogue!) args))

(defn get-dialogue
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-dialogue) args))

(defn update-dialogue-status!
  {:arglists '([dialogue-id new-status])}
  [& args]
  (apply (impl 'update-dialogue-status!) args))

(defn list-dialogues
  {:arglists '([] [status])}
  [& args]
  (apply (impl 'list-dialogues) args))

(defn add-turn!
  {:arglists '([dialogue-id {:keys [sender receiver message signal in-reply-to task-ref], :or {signal :propose}}])}
  [& args]
  (apply (impl 'add-turn!) args))

(defn get-turn
  {:arglists '([turn-id])}
  [& args]
  (apply (impl 'get-turn) args))

(defn get-turns
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-turns) args))

(defn get-turns-by-sender
  {:arglists '([dialogue-id sender-id])}
  [& args]
  (apply (impl 'get-turns-by-sender) args))

(defn count-signals
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'count-signals) args))

(defn check-consensus
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'check-consensus) args))
