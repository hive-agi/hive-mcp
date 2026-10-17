(ns hive-mcp.tools.migrate.kanban.store
  "Store-port adapters for the kanban migrator: the target WRITER and the
   target READER both go through the registry store's port
   (`proto/get-store :kanban` in the default wiring), so the store's
   decorator chain applies to both directions.

   Writer: `add-entry!`. With the seal's cipher layer on :kanban a migrated
   entry lands sealed, exactly like an entry written by the kanban tool.
   This replaces a writer that built qdrant points itself and called
   clj-qdrant `upsert-points` directly, around the chain: anything migrated
   that way landed unsealed and could persist an :embed-text.

   Reader: `get-entries` (one round-trip, when the store is batch-capable)
   or `get-entry` per id. The registry store is the DECORATED one (seal
   ShapeLayer/CipherLayer/VaultLayer records over the backend), which has
   no backend fields such as :client-atom, so a reader that reached into
   the backend found nothing and answered :qdrant/disconnected for every
   batch. Reading through the port also returns content OPENED: the raw
   base store would hand back ciphertext, which `classify-outcome` would
   take for an empty card and overwrite.

   Derived fields (`pure/derived-fields`) are dropped at both boundaries;
   the target store derives its own embedding from :content."
  (:require [hive-dsl.result :as r]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.pure :as pure]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Writer
;; =============================================================================

(defn- write-one
  "Add ENTRY to STORE through the port; one result row. A throw is that
   entry's failure, never the batch's."
  [store entry]
  (let [id (:id entry)]
    (try
      (pure/write-result id (proto/add-entry! store (pure/strip-derived entry)))
      (catch Throwable t
        (log/warn t "kanban migrator add-entry! failed" {:id id})
        {:id id :ok? false :error (or (.getMessage t) (str (class t)))}))))

(defrecord StoreEntryWriter [store]
  ports/IEntryWriter
  (write-entries [_ entries]
    (cond
      (empty? entries) (r/ok [])
      (nil? store)     (r/err :store/missing {:n (count entries)})
      :else            (r/ok (mapv #(write-one store %) entries)))))

(defn make-entry-writer
  "Writer over STORE, any IMemoryStore (the decorated registry store in
   production, a hive-spi stub in tests)."
  [store]
  (->StoreEntryWriter store))

;; =============================================================================
;; Reader
;; =============================================================================

(defn- read-failure?
  "True when a read answered a failure map (an open circuit's gate, a
   classified backend failure) instead of entries. Entries carry :id."
  [x]
  (and (map? x) (not (contains? x :id))
       (or (false? (:success? x)) (some? (:error x)) (seq (:errors x)))))

(defn- fetch
  "Entries for IDS through STORE's port: one get-entries call when the store
   is batch-capable, else get-entry per id. A failure map comes back as is."
  [store ids]
  (if (proto/batch-read-store? store)
    (proto/get-entries store ids)
    (reduce (fn [acc id]
              (let [e (proto/get-entry store id)]
                (if (read-failure? e) (reduced e) (cond-> acc e (conj e)))))
            [] ids)))

(defn entries-by-id
  "Pure: map of id -> entry for the ENTRIES whose :id is one of IDS, derived
   fields stripped. Anything without such an :id is dropped."
  [entries ids]
  (let [wanted (set ids)]
    (reduce (fn [acc e]
              (let [id (:id e)]
                (cond-> acc
                  (and (map? e) (contains? wanted id))
                  (assoc id (pure/strip-derived e)))))
            {}
            entries)))

(defrecord StoreEntryReader [store]
  ports/IEntryReader
  (read-by-ids [_ ids]
    (cond
      (empty? ids) (r/ok {})
      (nil? store) (r/err :store/missing {:n (count ids)})
      :else
      (try
        (let [res (fetch store (vec ids))]
          (if (or (read-failure? res) (not (sequential? res)))
            (r/err :store/read-failed {:n (count ids) :answer res})
            (r/ok (entries-by-id res ids))))
        (catch Throwable t
          (r/err :store/read-failed {:n (count ids) :message (.getMessage t)}))))))

(defn make-entry-reader
  "Reader over STORE through its port, so a decorated store answers with
   its content opened."
  [store]
  (->StoreEntryReader store))
