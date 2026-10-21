(ns hive-mcp.knowledge-graph.connection.avet
  "Port: walk one attribute's datoms in VALUE order (the AVET index).

   `attr-values-ascending` answers a lazy seq of [eid value] pairs for every
   datom of `attr`, ascending by value (ties ascending by eid). Consumers that
   only need the N smallest values (e.g. the hourly edge decay reading the
   least-recently-verified edges) realize O(N) datoms instead of every entity.

   Stores may implement `IAttrValueIndex` directly (recording test stores do).
   Every other IKGStore gets the default below, which reads the store's
   `db-snapshot` with the index fn of the Datalog engine that produced it:

     datascript.db.* -> datascript.core/datoms  :avet  (attr needs :db/index)
     datahike.db.*   -> datahike.api/datoms     :avet  (attr needs :db/index)
     datalevin.db.*  -> datalevin.core/datoms   :ave   (every attr indexed)

   Dispatch is on the SNAPSHOT's class, not the store's, so a store wrapper
   (stub, recording) answers with the engine it actually delegates to. An
   unknown engine falls back to a Datalog read sorted in memory: correct, but
   O(datoms of attr)."
  (:require [hive-spi.kg.protocol :as spi]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IAttrValueIndex
  (attr-values-ascending [store attr]
    "Lazy seq of [eid value] for every datom of attr, ascending by value."))

(def ^:private engine-index
  "Snapshot class namespace -> [datoms fn symbol, value-first index keyword]."
  {"datascript.db" ['datascript.core/datoms :avet]
   "datahike.db"   ['datahike.api/datoms :avet]
   "datalevin.db"  ['datalevin.core/datoms :ave]})

(defn engine-of
  "Namespace part of a db value's class name, e.g. \"datahike.db\"; nil for nil."
  [db]
  (when db
    (let [n (.getName (class db))
          i (.lastIndexOf n ".")]
      (when (pos? i) (subs n 0 i)))))

(defn- datalog-fallback
  [store attr]
  (->> (spi/query store '[:find ?e ?v :in $ ?a :where [?e ?a ?v]] [attr])
       (sort-by (juxt second first))))

(defn- snapshot-values-ascending
  [store attr]
  (let [db (spi/db-snapshot store)]
    (if-let [[fn-sym index] (get engine-index (engine-of db))]
      (map (fn [d] [(:e d) (:v d)]) ((requiring-resolve fn-sym) db index attr))
      (if db (datalog-fallback store attr) ()))))

(extend-protocol IAttrValueIndex
  Object
  (attr-values-ascending [store attr]
    (snapshot-values-ascending store attr)))

(m/=> engine-of [:=> [:cat :any] [:maybe :string]])
