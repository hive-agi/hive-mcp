;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.bootstrap.datahike
  "DatahikeBootstrap - persistent slave projection backed by a dedicated
   Datahike store at `data/swarm/datahike` (separate from the KG store).

   Compat shim: moved to hive-agent.swarm.bootstrap.datahike in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]
            [hive-mcp.swarm.bootstrap.protocol :as bp]))

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.bootstrap.datahike" sym))

(def ConnAtom
  "The lazy connection cell: nil until the first load or snapshot connects."
  [:fn {:gen/fmap (fn [_] (atom nil)) :gen/schema :nil}
   #(instance? clojure.lang.IAtom %)])

(def Config
  "A Datahike store config, as make-datahike-bootstrap builds it from a path."
  [:map [:store [:map [:backend :keyword] [:path :string]]]])

(defn ->DatahikeBootstrap
  {:arglists '([conn-atom cfg])}
  [& args]
  (apply (impl '->DatahikeBootstrap) args))

(m/=> ->DatahikeBootstrap [:=> [:cat ConnAtom Config] bp/Bootstrap])

(defn map->DatahikeBootstrap
  {:arglists '([m])}
  [& args]
  (apply (impl 'map->DatahikeBootstrap) args))

(m/=> map->DatahikeBootstrap [:=> [:cat [:map [:conn-atom ConnAtom] [:cfg Config]]] bp/Bootstrap])

(defn make-datahike-bootstrap
  {:arglists '([] [{:keys [db-path] :or {db-path default-db-path}}])}
  [& args]
  (apply (impl 'make-datahike-bootstrap) args))

;; construction is lazy: nothing touches disk until the first load or snapshot
(m/=> make-datahike-bootstrap [:function
                               [:=> [:cat] bp/Bootstrap]
                               [:=> [:cat [:map [:db-path {:optional true} :string]]] bp/Bootstrap]])
