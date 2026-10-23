;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.bootstrap.datahike-driver
  "Late-bound facade over datahike.api for the swarm bootstrap store.

   Compat shim: moved to hive-agent.swarm.bootstrap.datahike-driver in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.bootstrap.datahike-driver" sym))

(defn create-database {:arglists '([& args])} [& args] (apply (impl 'create-database) args))
(defn connect {:arglists '([& args])} [& args] (apply (impl 'connect) args))
(defn transact {:arglists '([& args])} [& args] (apply (impl 'transact) args))
(defn q {:arglists '([& args])} [& args] (apply (impl 'q) args))
(defn pull {:arglists '([& args])} [& args] (apply (impl 'pull) args))
(defn db {:arglists '([& args])} [& args] (apply (impl 'db) args))
(defn release {:arglists '([& args])} [& args] (apply (impl 'release) args))
(defn database-exists? {:arglists '([& args])} [& args] (apply (impl 'database-exists?) args))

;; the one driver fn with a stable shape: a store config in, a boolean out
(m/=> database-exists? [:=> [:cat [:map [:store [:map [:backend :keyword] [:path :string]]]]] :boolean])

;; The rest transcribe datahike.api's own published signatures
;; (datahike.api.specification): the arities and the data-shaped arguments
;; are stated; the opaque datahike values (Connection, DB) are named by what
;; the bootstrap relies on, and results whose shape the caller's query or
;; pull pattern chooses stay open.

(def ^:private Config
  "A datahike config map (its :store names the backend)."
  [:map [:store {:optional true} :map]])

(def ^:private Connection
  "A datahike Connection: an IDeref over the current DB value."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (atom {}))}
   #(instance? clojure.lang.IDeref %)])

(def ^:private Db
  "A datahike DB value (opaque)."
  [:some {:gen/schema :map}])

(def ^:private TxData
  "Transaction data, or the {:tx-data ...} argument map."
  [:or [:sequential :any] [:map [:tx-data [:sequential :any]]]])

(m/=> create-database [:function [:=> [:cat] Config] [:=> [:cat Config] Config]])
(m/=> connect [:function
               [:=> [:cat] Connection]
               [:=> [:cat Config] Connection]
               [:=> [:cat Config :map] Connection]])
(m/=> transact [:=> [:cat Connection TxData] [:map [:tx-data [:sequential :any]]]])
(m/=> q [:=> [:cat [:or [:vector :any] :map :string] [:* :any]] :any])
(m/=> pull [:function
            [:=> [:cat Db :map] [:maybe :map]]
            [:=> [:cat Db [:vector :any] :any] [:maybe :map]]])
(m/=> db [:=> [:cat Connection] Db])
(m/=> release [:function [:=> [:cat Connection] :nil] [:=> [:cat Connection :any] :nil]])
