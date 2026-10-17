(ns hive-mcp.tools.migrate.kanban.store
  "Store-port writer for the kanban migrator. Writes go through the
   registry store's `add-entry!` (`proto/get-store :kanban` in the default
   wiring), so the store's decorator chain applies: with the seal's cipher
   layer on :kanban a migrated entry lands sealed, exactly like an entry
   written by the kanban tool.

   This replaces a writer that built qdrant points itself and called
   clj-qdrant `upsert-points` directly, around the chain: anything migrated
   that way landed unsealed and could persist an :embed-text.

   Derived fields (`pure/derived-fields`) are dropped before the call; the
   target store derives its own embedding from :content."
  (:require [hive-dsl.result :as r]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.pure :as pure]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
