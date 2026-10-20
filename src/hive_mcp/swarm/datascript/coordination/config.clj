(ns hive-mcp.swarm.datascript.coordination.config
  "Typed config for hive-mcp.swarm.datascript.coordination.* via hive-di defconfig.

   Compat shim: moved to hive-agent.swarm.datascript.coordination.config in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.coordination.config" sym))

(defn ->coordination-config
  {:arglists '([kw__31536__auto__])}
  [& args]
  (apply (impl '->coordination-config) args))

;; Contracts transcribe the defconfig-generated vars of
;; hive-datascript.swarm.coordination.config.
(def ^:private Resolved
  "The resolved config map: both thresholds, integer milliseconds."
  [:map {:closed true} [:coordinator-stale-ms :int] [:claim-stale-ms :int]])

(def ^:private Overrides
  "Per-field overrides; a value the resolver cannot coerce is answered with a
   resolution failure, not refused here."
  [:maybe [:map-of :keyword :any]])

(def ^:private ConfigResult
  "The resolver's Result: {:ok Resolved} or an {:error ...} map."
  [:or [:map [:ok Resolved]] [:map [:error :keyword]]])

;; The ADT constructor: a known variant keyword answers the variant value;
;; any other keyword answers nil.
(m/=> ->coordination-config
      [:=> [:cat :keyword]
       [:maybe [:map [:adt/type [:= :CoordinationConfig]] [:adt/variant :keyword]]]])

(def CoordinationConfig @(impl 'CoordinationConfig))

(def CoordinationConfig-fields @(impl 'CoordinationConfig-fields))

(def CoordinationConfig-schema @(impl 'CoordinationConfig-schema))

(def CoordinationConfigMalli @(impl 'CoordinationConfigMalli))

(defn coordination-config
  {:arglists '([] [overrides])}
  [& args]
  (apply (impl 'coordination-config) args))

;; Throws on a resolution failure, so the answer is always resolved.
(m/=> coordination-config [:function
                           [:=> [:cat] Resolved]
                           [:=> [:cat Overrides] Resolved]])

(defn coordination-config?
  {:arglists '([x__31535__auto__])}
  [& args]
  (apply (impl 'coordination-config?) args))

(m/=> coordination-config? [:=> [:cat :any] :boolean])

(defn resolve-CoordinationConfig
  {:arglists '([] [overrides] [overrides opts])}
  [& args]
  (apply (impl 'resolve-CoordinationConfig) args))

(m/=> resolve-CoordinationConfig [:function
                                  [:=> [:cat] ConfigResult]
                                  [:=> [:cat Overrides] ConfigResult]
                                  [:=> [:cat Overrides [:maybe :map]] ConfigResult]])
