(ns hive-mcp.protocols.memory
  "IMemoryStore protocol family, re-exported from hive-spi.memory.ports.

   The protocols live in the hive-spi SPI leaf so storage backends implement
   them without compile-depending on hive-mcp. Every historical
   hive-mcp.protocols.memory/* name still resolves here: each protocol is a
   `def` alias of the ports protocol, each METHOD is a defn delegating to the
   ports var at call time. Predicates call `satisfies?` on the canonical ports
   vars, never on the local aliases.

   Registry (register-store!/get-store/set-store!) + id utils stay here."
  (:require [clojure.string]
            [hive-mcp.memory.ids :as ids]
            [hive-spi.memory.ports :as ports]
            [malli.core :as m]
            [hive-spi.memory.registry :as sreg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; Protocol re-exports: each protocol a def alias, each METHOD a defn
;;; ============================================================================
;;;
;;; A `def` alias of a protocol method freezes the fn built at alias time; its
;;; dispatch cache never sees an impl `extend`ed after this ns loaded. A defn
;;; resolves the ports var per call. Memory 20260728135559-5c7368e6.

(def IMemoryStore ports/IMemoryStore)

(defn connect!
  "See `hive-spi.memory.ports/connect!`."
  [store config]
  (ports/connect! store config))

(defn disconnect!
  "See `hive-spi.memory.ports/disconnect!`."
  [store]
  (ports/disconnect! store))

(defn connected?
  "See `hive-spi.memory.ports/connected?`."
  [store]
  (ports/connected? store))

(defn health-check
  "See `hive-spi.memory.ports/health-check`."
  [store]
  (ports/health-check store))

(defn add-entry!
  "See `hive-spi.memory.ports/add-entry!`."
  [store entry]
  (ports/add-entry! store entry))

(defn get-entry
  "See `hive-spi.memory.ports/get-entry`."
  [store id]
  (ports/get-entry store id))

(defn update-entry!
  "See `hive-spi.memory.ports/update-entry!`."
  [store id updates]
  (ports/update-entry! store id updates))

(defn delete-entry!
  "See `hive-spi.memory.ports/delete-entry!`."
  [store id]
  (ports/delete-entry! store id))

(defn query-entries
  "See `hive-spi.memory.ports/query-entries`."
  [store opts]
  (ports/query-entries store opts))

(defn search-similar
  "See `hive-spi.memory.ports/search-similar`."
  [store query-text opts]
  (ports/search-similar store query-text opts))

(defn supports-semantic-search?
  "See `hive-spi.memory.ports/supports-semantic-search?`."
  [store]
  (ports/supports-semantic-search? store))

(defn cleanup-expired!
  "See `hive-spi.memory.ports/cleanup-expired!`."
  [store]
  (ports/cleanup-expired! store))

(defn entries-expiring-soon
  "See `hive-spi.memory.ports/entries-expiring-soon`."
  [store days opts]
  (ports/entries-expiring-soon store days opts))

(defn find-duplicate
  "See `hive-spi.memory.ports/find-duplicate`."
  [store entry-type content-hash opts]
  (ports/find-duplicate store entry-type content-hash opts))

(defn store-status
  "See `hive-spi.memory.ports/store-status`."
  [store]
  (ports/store-status store))

(defn reset-store!
  "See `hive-spi.memory.ports/reset-store!`."
  [store]
  (ports/reset-store! store))

;;; ============================================================================
;;; Store Registry (Multi-Store)
;;; ============================================================================
;;;
;;; The registry maps named keys to IMemoryStore instances. The :default slot
;;; backs all legacy callers of `(get-store)`. Additional slots can host
;;; independent stores (e.g. cartography-scoped backends) without disturbing
;;; existing code.

(def register-store!
  "Register `store` under `key` in the multi-store registry.
   Returns the registered store."
  sreg/register-store!)

(def unregister-store!
  "Remove the store at `key`. No-op if absent. Does NOT disconnect
   the underlying store; callers are responsible for lifecycle."
  sreg/unregister-store!)

(def registered-stores
  "Return the current registry map {key -> store}. Read-only snapshot."
  sreg/registered-stores)

(def reset-registry!
  "Clear all entries from the registry. Intended for tests.
   Does NOT disconnect underlying stores."
  sreg/reset-registry!)

(def get-store
  "Get a memory store from the registry.
   0-arity: return the :default store, throw if none registered.
   1-arity: return the store registered under `key`, throw if absent."
  sreg/get-store)

(def set-store!
  "Legacy single-store setter. Routes to the :default slot of the
   multi-store registry."
  sreg/set-store!)

(def store-set?
  "Check if a default memory store has been configured."
  sreg/store-set?)

(defn reset-active-store!
  "Disconnect the :default store and drop it from the registry. Leaves other
   registry entries untouched. Never deletes store data."
  []
  (when-let [store (:default (registered-stores))]
    (try
      (disconnect! store)
      (catch Exception _)))
  (unregister-store! :default)
  nil)

;;; ============================================================================
;;; Lifecycle Convenience Functions
;;; ============================================================================

(def connect-active-store!
  "Connect the active store with the given config."
  sreg/connect-active-store!)

(defn active-store-healthy?
  "Check if the active store is connected and healthy. Returns nil when no
   store is registered, false when the health check throws."
  []
  (when (store-set?)
    (try
      (:healthy? (health-check (get-store)))
      (catch Exception _ false))))

(defn active-store-status
  "Get comprehensive status of the active store: `store-status` merged with
   `health-check`. Returns nil when no store is registered."
  []
  (when (store-set?)
    (let [store (get-store)]
      (merge (store-status store)
             (try (health-check store)
                  (catch Exception e
                    {:healthy? false :errors [(.getMessage e)]}))))))

;;; --- IMemoryStoreWithAnalytics ---

(def IMemoryStoreWithAnalytics ports/IMemoryStoreWithAnalytics)

(defn log-access!
  "See `hive-spi.memory.ports/log-access!`."
  [store id]
  (ports/log-access! store id))

(defn record-feedback!
  "See `hive-spi.memory.ports/record-feedback!`."
  [store id feedback]
  (ports/record-feedback! store id feedback))

(defn get-helpfulness-ratio
  "See `hive-spi.memory.ports/get-helpfulness-ratio`."
  [store id]
  (ports/get-helpfulness-ratio store id))

(defn analytics-store?
  "Check if the store supports analytics tracking."
  [store]
  (satisfies? ports/IMemoryStoreWithAnalytics store))

;;; --- IMemoryStoreBatch (batched reads) ---

(def IMemoryStoreBatch ports/IMemoryStoreBatch)

(defn get-entries
  "See `hive-spi.memory.ports/get-entries`."
  [store ids]
  (ports/get-entries store ids))

(defn batch-read-store?
  "Check if the store can fetch many entries in one backend round-trip."
  [store]
  (satisfies? ports/IMemoryStoreBatch store))

;;; --- IMemoryStoreMetadataWrite (no-embed metadata writes) ---

(def IMemoryStoreMetadataWrite ports/IMemoryStoreMetadataWrite)

(defn update-metadata!
  "See `hive-spi.memory.ports/update-metadata!`."
  [store id updates]
  (ports/update-metadata! store id updates))

(defn metadata-write-store?
  "Check if the store supports the no-embed metadata write surface."
  [store]
  (satisfies? ports/IMemoryStoreMetadataWrite store))

;;; --- IMemoryStoreWithStaleness ---

(def IMemoryStoreWithStaleness ports/IMemoryStoreWithStaleness)

(defn update-staleness!
  "See `hive-spi.memory.ports/update-staleness!`."
  [store id staleness-opts]
  (ports/update-staleness! store id staleness-opts))

(defn get-stale-entries
  "See `hive-spi.memory.ports/get-stale-entries`."
  [store threshold opts]
  (ports/get-stale-entries store threshold opts))

(defn propagate-staleness!
  "See `hive-spi.memory.ports/propagate-staleness!`."
  [store source-id depth]
  (ports/propagate-staleness! store source-id depth))

(defn staleness-store?
  "Check if the store supports staleness tracking."
  [store]
  (satisfies? ports/IMemoryStoreWithStaleness store))

(defn batch-store?
  "Check if the store supports batched reads."
  [store]
  (satisfies? ports/IMemoryStoreBatch store))

(defn get-entries-projected
  "Batch-fetch entries by IDs, then trim to `output-fields` when provided.
   Projection is applied client-side so all IMemoryStoreBatch impls benefit
   without needing per-backend changes. When `output-fields` is nil,
   returns full entries (backward compat).

   output-fields: seq of field-name strings (e.g. [\"id\" \"type\" \"tags\"])."
  ([store ids]
   (get-entries store ids))
  ([store ids {:keys [output-fields]}]
   (let [entries (get-entries store ids)]
     (if (seq output-fields)
       (let [ks (set (map keyword output-fields))]
         (mapv #(select-keys % ks) entries))
       entries))))

;;; --- IMemoryStoreWithRouting (multi-container routing) ---

(def IMemoryStoreWithRouting ports/IMemoryStoreWithRouting)

(defn target-collection-for
  "See `hive-spi.memory.ports/target-collection-for`."
  [store entry]
  (ports/target-collection-for store entry))

(defn relocate-entry!
  "See `hive-spi.memory.ports/relocate-entry!`."
  [store id]
  (ports/relocate-entry! store id))

(defn routing-store?
  "Check if the store supports container-routing introspection + relocation."
  [store]
  (satisfies? ports/IMemoryStoreWithRouting store))

;;; --- IMemoryStoreTemporal (bitemporal queries) ---

(def IMemoryStoreTemporal ports/IMemoryStoreTemporal)

(defn asof-entry
  "See `hive-spi.memory.ports/asof-entry`."
  [store id timestamp]
  (ports/asof-entry store id timestamp))

(defn history-entry
  "See `hive-spi.memory.ports/history-entry`."
  [store id]
  (ports/history-entry store id))

(defn asof-query
  "See `hive-spi.memory.ports/asof-query`."
  [store criteria timestamp]
  (ports/asof-query store criteria timestamp))

(defn between-query
  "See `hive-spi.memory.ports/between-query`."
  [store criteria t1 t2]
  (ports/between-query store criteria t1 t2))

(defn temporal-store?
  "Check if the store supports bitemporal queries."
  [store]
  (satisfies? ports/IMemoryStoreTemporal store))

;;; IMemoryStoreLiveness lives in its own ns to keep this file from
;;; needing reloads. See `hive-mcp.protocols.memory-liveness`.

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(def content-hash
  "Compute SHA-256 hash of normalized content."
  ids/content-hash)

(def generate-id
  "Generate a unique timestamped ID for memory entries."
  ids/generate-id)

(def iso-timestamp
  "Return current ISO 8601 timestamp."
  ids/iso-timestamp)

(def MemoryStore
  "Any value satisfying the canonical hive-spi.memory.ports/IMemoryStore protocol."
  [:fn #(satisfies? ports/IMemoryStore %)])

(m/=> get-store [:function
                 [:=> [:cat] MemoryStore]
                 [:=> [:cat :keyword] MemoryStore]])

(m/=> store-set? [:=> [:cat] :boolean])
