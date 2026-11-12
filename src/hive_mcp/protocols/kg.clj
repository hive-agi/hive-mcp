(ns hive-mcp.protocols.kg
  "Re-exports the KG SPI (hive-spi.kg.protocol) and owns active-store slot
   management. Backends implement hive-spi.kg.protocol/IKGStore directly.

   The data-path methods (transact!, query, entity, entid, pull-entity,
   eids-by-attr, db-snapshot) are metered as [:store/kg <backend> <method>]
   when storage metering is enabled (hive-mcp.protocols.store-metrics)."
  (:require [hive-spi.kg.protocol :as spi]
            [hive-mcp.protocols.registry :as reg]
            [hive-mcp.protocols.store-metrics :as sm]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; --- Re-export the SPI protocols + methods (interned so ns-qualified
;;     hive-mcp.protocols.kg/* access resolves; :refer would not). ---
(def IKGStore           spi/IKGStore)
(def IPersistentKGStore spi/IPersistentKGStore)
(def ITemporalKGStore   spi/ITemporalKGStore)

;; Protocol methods are defn delegates, not `def` aliases: an alias captures the
;; method fn at load time and misses any impl registered later by `extend`.
(defn ensure-conn! "Delegates to `spi/ensure-conn!`." [store]
  (spi/ensure-conn! store))

(defn transact!
  "`hive-spi.kg.protocol/transact!`, metered as [:store/kg <backend> :transact!]."
  [store tx-data]
  (sm/kg-timed store :transact! (spi/transact! store tx-data)))

(defn query
  "`hive-spi.kg.protocol/query`, metered as [:store/kg <backend> :query]."
  ([store q] (sm/kg-timed store :query (spi/query store q)))
  ([store q inputs] (sm/kg-timed store :query (spi/query store q inputs))))

(defn entity
  "`hive-spi.kg.protocol/entity`, metered as [:store/kg <backend> :entity]."
  [store eid]
  (sm/kg-timed store :entity (spi/entity store eid)))

(defn entid
  "`hive-spi.kg.protocol/entid`, metered as [:store/kg <backend> :entid]."
  [store lookup-ref]
  (sm/kg-timed store :entid (spi/entid store lookup-ref)))

(defn pull-entity
  "`hive-spi.kg.protocol/pull-entity`, metered as [:store/kg <backend> :pull-entity]."
  [store pattern eid]
  (sm/kg-timed store :pull-entity (spi/pull-entity store pattern eid)))

(defn eids-by-attr
  "`hive-spi.kg.protocol/eids-by-attr`, metered as [:store/kg <backend> :eids-by-attr]."
  [store attr]
  (sm/kg-timed store :eids-by-attr (spi/eids-by-attr store attr)))

(defn db-snapshot
  "`hive-spi.kg.protocol/db-snapshot`, metered as [:store/kg <backend> :db-snapshot]."
  [store]
  (sm/kg-timed store :db-snapshot (spi/db-snapshot store)))

(defn reset-conn! "Delegates to `spi/reset-conn!`." [store]
  (spi/reset-conn! store))
(defn close! "Delegates to `spi/close!`." [store]
  (spi/close! store))
(defn delete-database! "Delegates to `spi/delete-database!`." [store confirm]
  (spi/delete-database! store confirm))
(defn history-db "Delegates to `spi/history-db`." [store]
  (spi/history-db store))
(defn as-of-db "Delegates to `spi/as-of-db`." [store tx-or-time]
  (spi/as-of-db store tx-or-time))
(defn since-db "Delegates to `spi/since-db`." [store tx-or-time]
  (spi/since-db store tx-or-time))

(def kg-store?         spi/kg-store?)
(def persistent-store? spi/persistent-store?)
(def temporal-store?   spi/temporal-store?)

(def ->NoopKGStore    spi/->NoopKGStore)
(def map->NoopKGStore spi/map->NoopKGStore)
(def noop-store       spi/noop-store)

;; --- Active-store slot management (host runtime state, not a contract). ---
(defonce ^:private slot
  (reg/single-slot {:validate #(satisfies? spi/IKGStore %)
                    :on-empty #(throw (ex-info "No graph store configured. Call set-store! first."
                                               {:hint "Initialize with datascript-store, datalevin-store, or datahike-store"}))
                    :teardown spi/close!}))

(defn set-store!
  "Set the active graph store implementation."
  [store]
  (reg/install! slot store))

(def KGStore
  "Any value satisfying hive-spi.kg.protocol/IKGStore; generates the noop store."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (spi/noop-store))}
   #(satisfies? spi/IKGStore %)])

;; The input is loose: the slot's :validate assert rejects a non-store.
(m/=> set-store! [:=> [:cat :any] KGStore])

(defn get-store
  "Get the active graph store, or throw if none set."
  []
  (reg/current slot))

(m/=> get-store [:=> [:cat] KGStore])

(defn store-set?
  "Check if a store has been configured."
  []
  (reg/present? slot))

(m/=> store-set? [:=> [:cat] :boolean])

(defn clear-store!
  "Clear the active store."
  []
  (reg/clear! slot))

(m/=> clear-store! [:=> [:cat] :nil])

(defn active-temporal?
  "True when the active store supports temporal queries."
  []
  (and (store-set?)
       (spi/temporal-store? (get-store))))

(m/=> ensure-conn! [:=> [:cat :any] :any])
(m/=> reset-conn! [:=> [:cat :any] :any])
(m/=> close! [:=> [:cat :any] :any])
(m/=> delete-database! [:=> [:cat :any :any] :any])
(m/=> history-db [:=> [:cat :any] :any])
(m/=> as-of-db [:=> [:cat :any :any] :any])
(m/=> since-db [:=> [:cat :any :any] :any])

;; Metered data-path methods: the store input is loose (protocol dispatch
;; rejects a non-store); answers are backend values.
(m/=> transact! [:=> [:cat :any :any] :any])
(m/=> query [:function [:=> [:cat :any :any] :any] [:=> [:cat :any :any :any] :any]])
(m/=> entity [:=> [:cat :any :any] :any])
(m/=> entid [:=> [:cat :any :any] :any])
(m/=> pull-entity [:=> [:cat :any :any :any] :any])
(m/=> eids-by-attr [:=> [:cat :any :any] :any])
(m/=> db-snapshot [:=> [:cat :any] :any])

(m/=> active-temporal? [:=> [:cat] :boolean])
