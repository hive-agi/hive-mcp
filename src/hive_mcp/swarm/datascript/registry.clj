(ns hive-mcp.swarm.datascript.registry
  "DataScript implementation of all ISP-segregated swarm protocols.

   Compat shim: moved to hive-datascript.swarm.registry in hive-datascript.
   The records live there; this namespace exposes their constructors, the
   default instances and their accessors. See hive-mcp.swarm.delegate for
   behavior without the library."
  (:require [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-datascript.swarm.registry" sym))

(defn ->DataScriptRegistry {:arglists '([])} [& args] (apply (impl '->DataScriptRegistry) args))
(defn ->DataScriptClaimStore {:arglists '([])} [& args] (apply (impl '->DataScriptClaimStore) args))
(defn ->DataScriptCriticalOps {:arglists '([])} [& args] (apply (impl '->DataScriptCriticalOps) args))
(defn ->DataScriptCoordination {:arglists '([])} [& args] (apply (impl '->DataScriptCoordination) args))
(defn ->DataScriptDb {:arglists '([])} [& args] (apply (impl '->DataScriptDb) args))

(def default-registry @(impl 'default-registry))
(def default-claim-store @(impl 'default-claim-store))
(def default-critical-ops @(impl 'default-critical-ops))
(def default-coordination @(impl 'default-coordination))
(def default-db @(impl 'default-db))

(defn get-default-registry {:arglists '([])} [& args] (apply (impl 'get-default-registry) args))
(defn get-default-claim-store {:arglists '([])} [& args] (apply (impl 'get-default-claim-store) args))
(defn get-default-critical-ops {:arglists '([])} [& args] (apply (impl 'get-default-critical-ops) args))
(defn get-default-coordination {:arglists '([])} [& args] (apply (impl 'get-default-coordination) args))
(defn get-default-db {:arglists '([])} [& args] (apply (impl 'get-default-db) args))
