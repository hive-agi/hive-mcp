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
            [taoensso.timbre :as log]
            [hive-spi.memory.decorate :as decorate]
            [hive-spi.memory.ports :as sp]
            [malli.core :as m]))

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

;; =============================================================================
;; Existence probe
;; =============================================================================

(defn- scan-capable? [s] (satisfies? sp/IMemoryStoreScan s))

(defrecord StoreIdProbe [store]
  ;; Which ids the target holds, read from the BACKEND store under every
  ;; decorator (`decorate/base-store`): a visibility layer hides a row from
  ;; the reader but not from the backend's id listing. Only `scan-ids` is
  ;; called there, never a read: no content, sealed or not, leaves the
  ;; backend through this probe. One call lists the whole target; the use
  ;; case makes it at most once per run and keeps the set.
  ports/IIdProbe
  (held-ids [_]
    (if (nil? store)
      (r/err :store/missing {})
      (let [base (decorate/base-store store)]
        (if-not (scan-capable? base)
          (r/err :store/no-id-probe {:store (str (class base))})
          (try
            (r/ok (set (sp/scan-ids base {:include-expired? true})))
            (catch Throwable t
              (r/err :store/id-probe-failed {:message (.getMessage t)}))))))))

(defn make-id-probe
  "Existence probe over STORE (the decorated registry store in production):
   answers which ids the backend under every decorator holds, by id listing
   only."
  [store]
  (->StoreIdProbe store))

;; =============================================================================
;; Contracts
;; =============================================================================

(def MemoryStore
  "Any IMemoryStore (the decorated registry store in production), or nil:
   the adapters only close over it, and the port tests build them over nil
   to check the record type. The generator need not answer calls."
  [:maybe
   [:fn {:gen/elements [(reify proto/IMemoryStore (connected? [_] true))]}
    #(satisfies? proto/IMemoryStore %)]])

(def Durability
  [:fn {:gen/elements [(->StoreDurability nil)]} #(satisfies? ports/ITargetDurability %)])

(def EntryReader
  [:fn {:gen/elements [(->StoreEntryReader nil)]} #(satisfies? ports/IEntryReader %)])

(def EntryWriter
  [:fn {:gen/elements [(->StoreEntryWriter nil)]} #(satisfies? ports/IEntryWriter %)])

(def IdProbe
  [:fn {:gen/elements [(->StoreIdProbe nil)]} #(satisfies? ports/IIdProbe %)])

(m/=> entries-by-id
  [:=> [:cat [:maybe [:sequential :any]] [:maybe [:or [:sequential :any] [:set :any]]]]
   [:map-of :any :map]])

(m/=> make-durability [:=> [:cat MemoryStore] Durability])

(m/=> make-entry-reader [:=> [:cat MemoryStore] EntryReader])

(m/=> make-entry-writer [:=> [:cat MemoryStore] EntryWriter])

(m/=> make-id-probe [:=> [:cat MemoryStore] IdProbe])

;; =============================================================================
;; Durability gate
;; =============================================================================

(defrecord StoreDurability [store]
  ;; Asked through the DECORATED store's port (`connected?`, which the seal
  ;; layers forward to the backend), never the backend's fields. A store
  ;; that is not connected may still answer add-entry! with the id, out of
  ;; an in-memory fallback (hive-qdrant does): that is no durable write.
  ;; A store that cannot answer at all is refused too.
  ports/ITargetDurability
  (check-durable [_]
    (if (nil? store)
      (r/err :store/missing {})
      (try
        (if (true? (proto/connected? store))
          (r/ok {:connected? true})
          (r/err :target/not-durable
                 {:message (str "the migration target store is not connected: its writes"
                                " would land only in memory. Connect it, then run again.")
                  :store   (str (class (decorate/base-store store)))}))
        (catch Throwable t
          (r/err :target/not-durable
                 {:message (str "the migration target store cannot say whether it is connected: "
                                (or (.getMessage t) (str (class t))))
                  :store   (str (class store))}))))))

(defn make-durability
  "Durability gate over STORE (the decorated registry store in production):
   ok only while the store reports itself connected."
  [store]
  (->StoreDurability store))
