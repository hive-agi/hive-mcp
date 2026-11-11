(ns hive-mcp.tools.memory.reconcile
  "`memory reconcile` verb: report vector-index ids absent from the entry
   store and entry-store ids absent from the vector index.

   Read-only by construction. The verb asks an `IReconcileSources` port for
   the two id listings and renders `hive-mcp.memory.reconcile/report`. It
   has no purge or repair mode in this slice, so it performs no store write.

   Sources: an IReconcileSources installed with `set-sources!` wins. With
   none installed, the active store is reconciled through its own complete
   id scan (`IMemoryStoreScan`, see `store-sources`): that is the live
   Milvus path, where each vector sits in its entry's row. Only a store with
   no scan answers {:status \"not-wired\"}.

   Params:
     :limit  cap on each listed id vector (default 100, min 1)"
  (:require [hive-mcp.memory.reconcile :as reconcile]
            [hive-mcp.memory.reconcile-sources :as rsrc]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-spi.memory.ports :as ports]
            [hive-mcp.tools.core :as tcore :refer [mcp-json mcp-error]]
            [hive-mcp.dns.result :refer [rescue-log]]
            [malli.core :as m]
            [hive-mcp.tools.memory.schema :as ms]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-limit
  "How many ids each listing of the report carries unless :limit says so."
  100)

(defonce ^:private -sources (atom nil))

(defn set-sources!
  "Install SOURCES (an IReconcileSources) as the id listings the verb reads.
   Called from a composition root or a test. Returns SOURCES."
  [sources]
  (reset! -sources sources)
  sources)

(defn clear-sources!
  "Remove the installed sources. Returns nil."
  []
  (reset! -sources nil)
  nil)

(defn- effective-limit
  "LIMIT as a positive integer, falling back to `default-limit`."
  [limit]
  (let [n (cond (integer? limit) limit
                (string? limit)  (parse-long limit)
                :else            nil)]
    (if (and n (pos? n)) n default-limit)))

(defn reconcile-response
  "The response map for SOURCES and LIMIT. Pure over the port: a nil SOURCES
   is the not-wired stub answer, anything else is the truncated report."
  [sources limit]
  (if (nil? sources)
    {:status "not-wired"
     :message "no reconcile sources installed and the active store cannot scan its ids"}
    (assoc (reconcile/truncate-report (reconcile/report sources)
                                      (effective-limit limit))
           :status "ok")))

(defn store-sources
  "Sources derived from STORE when it can enumerate its ids
   (`IMemoryStoreScan`), else nil. The vectors of such a store live in the
   entry row, so both sides read the same complete scan
   (`reconcile-sources/scan-sources`). Read-only."
  [store]
  (when (and store (satisfies? ports/IMemoryStoreScan store))
    (rsrc/scan-sources #(ports/scan-ids store {}))))

(defn- active-store
  "The :default memory store, or nil when none is registered."
  []
  (when (mem-proto/store-set?) (mem-proto/get-store)))

(defn handle-reconcile
  "Report drift between the entry store and the vector index. Read-only.
   Installed sources win; otherwise the active store's own id scan is used
   when it has one, and the answer stays not-wired only when it has none."
  [{:keys [limit]}]
  (rescue-log "[memory] reconcile" (mcp-error "reconcile failed")
    (mcp-json (reconcile-response (or @-sources (store-sources (active-store))) limit))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> set-sources! [:=> [:cat [:maybe ms/ReconcileSources]] [:maybe ms/ReconcileSources]])
(m/=> clear-sources! [:=> [:cat] :nil])

(m/=> reconcile-response
      [:=> [:cat [:maybe ms/ReconcileSources] [:any {:gen/schema [:maybe [:or :int [:enum "3" "x" "0"]]]}]]
       ms/ReconcileResponse])

(m/=> store-sources [:=> [:cat :any] [:maybe ms/ReconcileSources]])

(m/=> handle-reconcile [:=> [:cat ms/McpParams] tcore/McpResponse])
