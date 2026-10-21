(ns hive-mcp.tools.memory.crud.reembed
  "Reembed handler for memory entries.

   Re-vectorize an entry without rewriting content. Useful when:
     - The embedding model changed and old vectors are stale.
     - The vector index was rebuilt and an entry needs refreshing.
     - KG edges shifted and the semantic neighborhood needs to catch up.

   Implementation note: any call to IMemoryStore/update-entry! routes through
   the backend's index-memory-entry! path, which always re-embeds the merged
   document. So re-embedding without content rewrite is a no-content update
   (we touch :updated only) that nevertheless triggers a full re-index.

   Preserves entry id → all KG edges (keyed by id) survive untouched. Tags,
   duration, abstraction-level, project-id, content-hash all flow through the
   merge unchanged.

   Acceptance origin: kanban 20260427004720-6447110d."
  (:require [hive-mcp.tools.memory.core :refer [with-store]]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.core :as core :refer [mcp-json mcp-error]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.dns.result :refer [rescue-log]]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.tools.memory.crud.deferred :as deferred]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- now-iso []
  (str (java.time.Instant/now)))

(defn- publish-reembed! [id existing]
  (rescue-log (str "[memory] reembed publish " id) nil
    (when-let [publish-fn (requiring-resolve 'hive-mcp.channel.core/publish!)]
      (publish-fn {:type :memory-reembedded
                   :id id
                   :memory-type (:type existing)
                   :tags (:tags existing)
                   :project-id (:project-id existing)}))))

(defn- reembed-one!
  "Drain a deferred id through the existing store add path, or reembed a
   previously indexed entry through update-entry!. A deferred entry drains
   into the store slot it was parked from (kanban parks :kanban), not the
   caller's. Remove the outbox record only after the store confirms the
   write. Returns nil when not found."
  [store id]
  (let [pending (deferred/lookup id)
        existing (or pending (mem-proto/get-entry store id))]
    (when existing
      (let [updated (if pending
                      (let [target (if-let [k (deferred/store-key-of pending)]
                                     (mem-proto/get-store k)
                                     store)
                            entry (deferred/->store-entry pending)
                            stored-id (mem-proto/add-entry! target entry)]
                        (when-not (= id stored-id)
                          (throw (ex-info "Deferred reembed did not persist"
                                          {:id id :result stored-id})))
                        (deferred/remove! id)
                        (or (mem-proto/get-entry target id) entry))
                      (mem-proto/update-entry! store id {:updated (now-iso)}))]
        (log/info "Memory reembed:" id)
        (publish-reembed! id existing)
        {:id id :reembedded true :updated updated :existing existing}))))

(defn handle-reembed
  "Re-embed a single memory entry by id. No content change required.

   Params:
     :id     — required, entry to re-embed
     :force  — accepted; currently every reembed forces re-index (the
               backend's update-entry! always re-vectorizes). Kept for
               forward compatibility once a content-hash skip lands."
  [{:keys [id]}]
  (cond
    (or (nil? id) (str/blank? id))
    (mcp-error "id is required (non-blank string)")

    :else
    (with-store
      (let [store  (mem-proto/get-store)
            result (reembed-one! store id)]
        (if (nil? result)
          (mcp-json {:error "Entry not found" :id id})
          (mcp-json (assoc (fmt/entry->json-alist (:updated result))
                           :reembedded true)))))))

(defn- batch-op-result
  "Reembed one id. A failure is never swallowed: it is logged at ERROR with
   the entry's accumulated attempt count and cause, stamped onto the parked
   record (so a record stuck in the outbox says how long and why), and
   reported as :status :error with the message. The record stays parked."
  [store id]
  (try
    (if (reembed-one! store id)
      {:id id :status :reembedded}
      {:id id :status :not-found})
    (catch Exception e
      (let [parked (deferred/record-failure! id e (now-iso))]
        (log/error "Reembed failed; entry stays in the outbox:" id
                   "attempts:" (:deferred/attempts parked)
                   "first-failed-at:" (:deferred/first-failed-at parked)
                   "cause:" (ex-message e))
        {:id id :status :error :error (ex-message e)
         :attempts (:deferred/attempts parked)}))))

(defn- tally-statuses
  [results]
  (let [status->count (frequencies (map :status results))]
    {:total      (count results)
     :reembedded (get status->count :reembedded 0)
     :not-found  (get status->count :not-found 0)
     :errors     (get status->count :error 0)}))

(defn handle-batch-reembed
  "Re-embed ids sequentially; when :ids is omitted, drain the durable pending
   outbox. Each failed id stays parked for a subsequent retry."
  [{:keys [ids]}]
  (let [ids (or ids (deferred/pending-ids))]
    (if (empty? ids)
      (mcp-json {:total 0 :reembedded 0 :not-found 0 :errors 0 :results []})
      (with-store
        (let [store (mem-proto/get-store)
              results (mapv #(batch-op-result store %) ids)
              summary (tally-statuses results)]
          (mcp-json (assoc summary :results results)))))))

(defn drain-pending!
  "Housekeeping entry point: reembed every parked id once, so an entry whose
   embed failed is re-vectorized without anyone calling memory reembed. A
   failure leaves the id parked for the next sweep and is logged at ERROR
   (the per-id cause is logged by `batch-op-result`; the sweep logs the
   tally and how many entries are still unvectorized). Returns the tally,
   with :stuck (ids still parked after this sweep) when any remain."
  []
  (let [ids (deferred/pending-ids)]
    (if (empty? ids)
      {:total 0 :reembedded 0 :not-found 0 :errors 0}
      (let [store (mem-proto/get-store)
            results (mapv #(batch-op-result store %) ids)
            tally (tally-statuses results)
            stuck (vec (keep #(when (= :error (:status %)) (:id %)) results))]
        (when (seq stuck)
          (log/error "Reembed sweep left" (count stuck) "of" (:total tally)
                     "entries unvectorized; they stay in the outbox:" stuck))
        (cond-> tally (seq stuck) (assoc :stuck stuck))))))

;;; =============================================================================
;;; Contracts
;;; =============================================================================

(def ReembedTally
  "The drain tally: every count non-negative and summing to :total, plus
   :stuck (the ids still parked) when the sweep left any."
  [:map
   [:total [:int {:min 0}]]
   [:reembedded [:int {:min 0}]]
   [:not-found [:int {:min 0}]]
   [:errors [:int {:min 0}]]
   [:stuck {:optional true} [:vector :string]]])

(m/=> handle-reembed [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> handle-batch-reembed [:=> [:cat [:maybe :map]] core/McpResponse])

(m/=> drain-pending! [:=> [:cat] ReembedTally])
