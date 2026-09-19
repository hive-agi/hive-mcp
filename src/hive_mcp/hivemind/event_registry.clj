(ns hive-mcp.hivemind.event-registry
  "Single source of truth for hivemind event types and their properties.

   Compat shim: moved to hive-agent.swarm.hivemind.event-registry in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.hivemind.event-registry" sym))

(def all-event-type-strings @(impl 'all-event-type-strings))

(def all-event-types @(impl 'all-event-types))

(def event-type->slave-status @(impl 'event-type->slave-status))

(defn format-icon
  {:arglists '([event-type])}
  [& args]
  (apply (impl 'format-icon) args))

(defn mcp-enum
  {:arglists '([])}
  [& args]
  (apply (impl 'mcp-enum) args))

(defn event-type-schema
  "JSON schema for an event_type tool parameter described by DESCRIPTION.
   Carries the :enum of known event types when the swarm addon is loaded, and
   omits it otherwise, so a host without hive-agent still builds its tool
   schemas at load."
  [description]
  (cond-> {:type "string" :description description}
    (delegate/available? "hive-agent.swarm.hivemind.event-registry")
    (assoc :enum (mcp-enum))))

(def mcp-event-types @(impl 'mcp-event-types))

(def registry @(impl 'registry))

(defn severity
  {:arglists '([event-type])}
  [& args]
  (apply (impl 'severity) args))

(def severity-levels @(impl 'severity-levels))

(defn slave-status
  {:arglists '([event-type])}
  [& args]
  (apply (impl 'slave-status) args))

(def terminal-event-types @(impl 'terminal-event-types))

(defn terminal?
  {:arglists '([event-type])}
  [& args]
  (apply (impl 'terminal?) args))

(defn valid-event-type?
  {:arglists '([t])}
  [& args]
  (apply (impl 'valid-event-type?) args))

(defn valid-transition?
  {:arglists '([from-event to-event])}
  [& args]
  (apply (impl 'valid-transition?) args))
