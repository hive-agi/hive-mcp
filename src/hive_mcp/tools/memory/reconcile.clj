(ns hive-mcp.tools.memory.reconcile
  "`memory reconcile` verb: report vector-index ids absent from the entry
   store and entry-store ids absent from the vector index.

   Read-only by default. The verb asks an `IReconcileSources` port for the
   two id listings and renders `hive-mcp.memory.reconcile/report`. The one
   write is the opt-in purge of ORPHAN VECTOR records (`purge-response`):
   it needs purge=true AND i_mean_it=true, it goes only through the
   sources' vector-side `IOrphanVectorPurge` port, and it never writes the
   entry store (the source of truth). purge=true alone is a dry run that
   counts the candidates.

   Sources: an IReconcileSources installed with `set-sources!` wins. With
   none installed, the active store is reconciled through its own complete
   id scan (`IMemoryStoreScan`, see `store-sources`): that is the live
   Milvus path, where each vector sits in its entry's row. Only a store with
   no scan answers {:status \"not-wired\"}.

   Params:
     :limit      cap on each listed id vector (default 100, min 1)
     :purge      true (or \"true\") asks for the orphan-vector purge
     :i_mean_it  must be literal true with :purge, or the purge is a dry run"
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

(defn- flag?
  "True for a boolean true or the string \"true\"."
  [v]
  (or (true? v) (= "true" v)))

(def ^:private partial-listing-reason
  "a listing stopped before its end, so the orphan list may be partial; nothing deleted")

(defn- complete-listing?
  "False only for paged sources (`reconcile-sources/paged-sources`) whose
   last read was cut at :max-pages. Other sources are complete or throw
   (`scan-sources`), so they count as complete."
  [sources]
  (if (instance? hive_mcp.memory.reconcile_sources.PagedSources sources)
    (rsrc/exhausted? sources)
    true))

(defn purge-response
  "The response for a purge request over SOURCES, LIMIT and CONFIRM?
   (true only when the caller passed i_mean_it true).

   Deletes only vector-index records, and only through the sources'
   `IOrphanVectorPurge` port; the entry store is read, never written.
   :purge :mode says what happened:
     \"unsupported\" the sources have no vector-side purge (the default
                   scan of a co-located store: there an orphan vector
                   cannot exist), nothing deleted
     \"refused\"     a paged listing did not reach its end, so the orphan
                   list may be partial, nothing deleted
     \"dry-run\"     CONFIRM? is not true: the candidates are counted and
                   nothing is deleted
     \"applied\"     the store ids were read AGAIN and only candidates still
                   absent from them were deleted (`reconcile/purge-plan`);
                   the ones the store now holds are counted as :kept-count
   Rows with no usable id are never purged (they have no id to delete by);
   :skipped-nil-ids counts them."
  [sources limit confirm?]
  (let [rep        (reconcile/report sources)
        candidates (:orphan-vector-ids rep)
        base       {:candidate-count (count candidates)
                    :skipped-nil-ids (:nil-vector-ids rep)
                    :purged          0}
        purge      (cond
                     (not (satisfies? reconcile/IOrphanVectorPurge sources))
                     (assoc base :mode "unsupported"
                            :reason "these sources have no vector-side purge; nothing deleted")

                     (not (complete-listing? sources))
                     (assoc base :mode "refused" :reason partial-listing-reason)

                     (not confirm?)
                     (assoc base :mode "dry-run"
                            :reason "pass purge=true and i_mean_it=true to delete these vector records")

                     :else
                     (let [fresh (reconcile/store-ids sources)]
                       ;; The fresh store read must be complete too: a cut
                       ;; listing would put ids the store holds in the plan.
                       (if-not (complete-listing? sources)
                         (assoc base :mode "refused" :reason partial-listing-reason)
                         (let [plan (reconcile/purge-plan fresh candidates)
                               n    (if (seq plan) (reconcile/purge-vector-ids! sources plan) 0)]
                           (assoc base :mode "applied"
                                  :planned-count (count plan)
                                  :kept-count (- (count candidates) (count plan))
                                  :purged (if (and (integer? n) (not (neg? n))) n (count plan)))))))]
    (assoc (reconcile/truncate-report rep (effective-limit limit))
           :status "ok"
           :purge purge)))

(defn handle-reconcile
  "Report drift between the entry store and the vector index. Installed
   sources win; otherwise the active store's own id scan is used when it has
   one, and the answer stays not-wired only when it has none.

   Read-only unless :purge is true. Even then it is a dry run unless
   :i_mean_it is literally true; see `purge-response`."
  [{:keys [limit purge i_mean_it]}]
  (rescue-log "[memory] reconcile" (mcp-error "reconcile failed")
    (let [sources (or @-sources (store-sources (active-store)))]
      (mcp-json (if (and sources (flag? purge))
                  (purge-response sources limit (true? i_mean_it))
                  (reconcile-response sources limit))))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> set-sources! [:=> [:cat [:maybe ms/ReconcileSources]] [:maybe ms/ReconcileSources]])
(m/=> clear-sources! [:=> [:cat] :nil])

(m/=> reconcile-response
      [:=> [:cat [:maybe ms/ReconcileSources] [:any {:gen/schema [:maybe [:or :int [:enum "3" "x" "0"]]]}]]
       ms/ReconcileResponse])

(m/=> store-sources [:=> [:cat :any] [:maybe ms/ReconcileSources]])

(m/=> purge-response
      [:=> [:cat ms/ReconcileSources [:any {:gen/schema [:maybe [:or :int [:enum "3" "x"]]]}] :boolean]
       ms/ReconcilePurgeResponse])

(m/=> handle-reconcile [:=> [:cat ms/McpParams] tcore/McpResponse])
