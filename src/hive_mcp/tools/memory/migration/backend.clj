(ns hive-mcp.tools.memory.migration.backend
  "Backend-to-backend migration (e.g. Chroma <-> Proximum).
   Reads entries from a source IMemoryStore and re-indexes into a target."
  (:require [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.dns.result :refer [rescue]]
            [taoensso.timbre :as log]
            [hive-mcp.vectordb.resilience :refer [with-resilience]]
            [hive-spi.memory.contract :as contract]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn write-status
  "Classify the answer R of a target-store add-entry! as :migrated, :queued
   (accepted for replay, not yet durable) or :failed (refused). A write that
   returns without throwing is not thereby a success: the store may answer a
   refusal map, which hive-spi.memory.contract/failure? recognises."
  [r]
  (cond
    (contract/failure? r) :failed
    (contract/queued? r)  :queued
    :else                 :migrated))

(defn migrate-backend!
  "Migrate all entries from one IMemoryStore backend to another.

   Reads entries from source-store in batches, re-indexes each into target-store
   via add-entry! (which re-embeds via the configured EmbeddingProvider).

   Arguments:
     source-store - IMemoryStore instance to read from
     target-store - IMemoryStore instance to write to
     opts         - Optional map:
       :batch-size   - Entries per query batch (default: 500)
       :max-entries  - Total cap (default: 50000)
       :dry-run?     - Count without writing (default: false)
       :project-id   - Filter to specific project (default: all)
       :on-progress  - (fn [stats]) callback per batch

   A write the target refuses (hive-spi.memory.contract/failure?) counts as
   :failed, one it only accepted for replay counts as :queued; neither counts
   as :migrated. :errors counts exceptions thrown while reading or writing.

   Returns:
     {:migrated int :skipped int :errors int :failed int :queued int
      :total-source int}"
  [source-store target-store & [{:keys [batch-size max-entries dry-run? project-id on-progress]
                                 :or {batch-size 500 max-entries 50000 dry-run? false}}]]
  (log/info "migrate-backend! starting" {:dry-run? dry-run? :project-id project-id
                                         :batch-size batch-size :max-entries max-entries})
  (let [entry-types ["axiom" "decision" "convention" "principle" "note" "snippet"]
        stats (atom {:migrated 0 :skipped 0 :errors 0 :failed 0 :queued 0 :total-source 0})]
    (doseq [entry-type entry-types]
      (let [query-opts (cond-> {:type entry-type :limit batch-size :include-expired? true}
                         project-id (assoc :project-id project-id))
            entries (with-resilience
                      (mem-proto/query-entries source-store query-opts))]
        (swap! stats update :total-source + (count entries))
        (doseq [entry entries
                :while (< (:migrated @stats) max-entries)]
          (if dry-run?
            (swap! stats update :migrated inc)
            (let [result (rescue :error
                                 (let [existing (with-resilience
                                                  (mem-proto/get-entry target-store (:id entry)))]
                                   (if existing
                                     :skipped
                                     (write-status
                                      (with-resilience
                                        (mem-proto/add-entry! target-store entry))))))]
              (case result
                :migrated (swap! stats update :migrated inc)
                :skipped  (swap! stats update :skipped inc)
                :failed   (swap! stats update :failed inc)
                :queued   (swap! stats update :queued inc)
                :error    (swap! stats update :errors inc)))))
        (when on-progress
          (on-progress @stats))))
    (let [result @stats]
      (log/info "migrate-backend! complete" result)
      result)))
