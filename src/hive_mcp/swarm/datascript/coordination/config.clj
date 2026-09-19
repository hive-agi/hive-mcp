(ns hive-mcp.swarm.datascript.coordination.config
  "Typed config for hive-mcp.swarm.datascript.coordination.* via hive-di defconfig.

   Compat shim: moved to hive-agent.swarm.datascript.coordination.config in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (requiring-resolve (symbol "hive-agent.swarm.datascript.coordination.config" (name sym))))

(defn ->coordination-config
  {:arglists '([kw__31536__auto__])}
  [& args]
  (apply (impl '->coordination-config) args))

(def CoordinationConfig @(impl 'CoordinationConfig))

(def CoordinationConfig-fields @(impl 'CoordinationConfig-fields))

(def CoordinationConfig-schema @(impl 'CoordinationConfig-schema))

(def CoordinationConfigMalli @(impl 'CoordinationConfigMalli))

(defn coordination-config
  {:arglists '([] [overrides])}
  [& args]
  (apply (impl 'coordination-config) args))

(defn coordination-config?
  {:arglists '([x__31535__auto__])}
  [& args]
  (apply (impl 'coordination-config?) args))

(defn resolve-CoordinationConfig
  {:arglists '([] [overrides] [overrides opts])}
  [& args]
  (apply (impl 'resolve-CoordinationConfig) args))
