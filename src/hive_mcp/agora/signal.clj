(ns hive-mcp.agora.signal
  "Data-driven signal schema for Agora dialogues.

   Compat shim: moved to hive-agent.swarm.agora.signal in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.agora.signal" sym))

(def signal-types @(impl 'signal-types))

(def equilibrium-signals @(impl 'equilibrium-signals))

(def disruption-signals @(impl 'disruption-signals))

(def neutral-signals @(impl 'neutral-signals))

(def all-valid-signals @(impl 'all-valid-signals))

(def default-strength @(impl 'default-strength))

(def default-evidence @(impl 'default-evidence))

(defn signal-from-map
  {:arglists '([{:keys [type signal strength confidence target evidence message], :as input}])}
  [& args]
  (apply (impl 'signal-from-map) args))

(defn signal-from-legacy
  {:arglists '([message])}
  [& args]
  (apply (impl 'signal-from-legacy) args))

(defn valid-signal?
  {:arglists '([signal-map])}
  [& args]
  (apply (impl 'valid-signal?) args))

(defn signal-type
  {:arglists '([signal-map])}
  [& args]
  (apply (impl 'signal-type) args))

(defn equilibrium-contribution
  {:arglists '([signal-or-type])}
  [& args]
  (apply (impl 'equilibrium-contribution) args))

(defn format-signal-prefix
  {:arglists '([signal-type message])}
  [& args]
  (apply (impl 'format-signal-prefix) args))

(defn signal->json-schema
  {:arglists '([])}
  [& args]
  (apply (impl 'signal->json-schema) args))

(defn parse-signal
  {:arglists '([input] [input default-message])}
  [& args]
  (apply (impl 'parse-signal) args))
