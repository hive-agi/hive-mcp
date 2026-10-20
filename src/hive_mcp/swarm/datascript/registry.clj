(ns hive-mcp.swarm.datascript.registry
  "DataScript implementation of all ISP-segregated swarm protocols.

   Compat shim: moved to hive-datascript.swarm.registry in hive-datascript.
   The records live there; this namespace exposes their constructors, the
   default instances and their accessors. See hive-mcp.swarm.delegate for
   behavior without the library."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [hive-spi.swarm.protocol :as proto]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.registry" sym))

(defn ->DataScriptRegistry {:arglists '([])} [& args] (apply (impl '->DataScriptRegistry) args))

;; The swarm protocol ports the records implement (hive-spi.swarm.protocol).
;; Each generates an empty reify, so the schema holds without the library.
(def SwarmRegistry
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (reify proto/ISwarmRegistry))}
   (fn [x] (satisfies? proto/ISwarmRegistry x))])

(def ClaimStore
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (reify proto/IClaimStore))}
   (fn [x] (satisfies? proto/IClaimStore x))])

(def CriticalOps
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (reify proto/ICriticalOps))}
   (fn [x] (satisfies? proto/ICriticalOps x))])

(def Coordination
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (reify proto/ICoordination))}
   (fn [x] (satisfies? proto/ICoordination x))])

(def SwarmDb
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (reify proto/ISwarmDb))}
   (fn [x] (satisfies? proto/ISwarmDb x))])

(m/=> ->DataScriptRegistry [:=> [:cat] SwarmRegistry])
(defn ->DataScriptClaimStore {:arglists '([])} [& args] (apply (impl '->DataScriptClaimStore) args))

(m/=> ->DataScriptClaimStore [:=> [:cat] ClaimStore])
(defn ->DataScriptCriticalOps {:arglists '([])} [& args] (apply (impl '->DataScriptCriticalOps) args))

(m/=> ->DataScriptCriticalOps [:=> [:cat] CriticalOps])
(defn ->DataScriptCoordination {:arglists '([])} [& args] (apply (impl '->DataScriptCoordination) args))

(m/=> ->DataScriptCoordination [:=> [:cat] Coordination])
(defn ->DataScriptDb {:arglists '([])} [& args] (apply (impl '->DataScriptDb) args))

(m/=> ->DataScriptDb [:=> [:cat] SwarmDb])

(def default-registry @(impl 'default-registry))
(def default-claim-store @(impl 'default-claim-store))
(def default-critical-ops @(impl 'default-critical-ops))
(def default-coordination @(impl 'default-coordination))
(def default-db @(impl 'default-db))

(defn get-default-registry {:arglists '([])} [& args] (apply (impl 'get-default-registry) args))

(m/=> get-default-registry [:=> [:cat] SwarmRegistry])
(defn get-default-claim-store {:arglists '([])} [& args] (apply (impl 'get-default-claim-store) args))

(m/=> get-default-claim-store [:=> [:cat] ClaimStore])
(defn get-default-critical-ops {:arglists '([])} [& args] (apply (impl 'get-default-critical-ops) args))

(m/=> get-default-critical-ops [:=> [:cat] CriticalOps])
(defn get-default-coordination {:arglists '([])} [& args] (apply (impl 'get-default-coordination) args))

(m/=> get-default-coordination [:=> [:cat] Coordination])
(defn get-default-db {:arglists '([])} [& args] (apply (impl 'get-default-db) args))

(m/=> get-default-db [:=> [:cat] SwarmDb])
