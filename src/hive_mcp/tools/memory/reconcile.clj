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
     :i_mean_it  must be boolean true with :purge, or the purge is a dry run
                 (a JSON true; or, routed through the `multi` tool, a string
                 its boolean decoder reads as true: \"true\", \"yes\", \"1\")

   Purge guards, all failing closed (mode \"refused\", nothing deleted): the
   sources must report both listings complete (`IListingCompleteness`), the
   store listing and its fresh re-read must not be empty while candidates
   exist, and the plan must stay within `reconcile/max-purge-fraction` of
   the vector ids."
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

(def ^:private no-completeness-reason
  "these sources do not report whether their listings are complete (no IListingCompleteness port), so the orphan list cannot be trusted; nothing deleted")

(defn- listing-refusal
  "Why the listings of SOURCES cannot back a purge, or nil when they can.
   Fails CLOSED: a purge proceeds only when SOURCES implement
   `reconcile/IListingCompleteness` and answer literally true, i.e. both the
   store and the vector listing were read to their end. Sources that cannot
   say are refused, never assumed complete."
  [sources]
  (cond
    (not (satisfies? reconcile/IListingCompleteness sources))
    no-completeness-reason

    (not (true? (reconcile/listings-complete? sources)))
    partial-listing-reason

    :else nil))

(defn- apply-purge!
  "The one deleting step, reached only after every guard of `purge-response`
   passed and the caller confirmed. Reads the store ids AGAIN, re-checks that
   listing (complete, not empty, plan within the safety cap) and only then
   asks the vector-side port to delete `reconcile/purge-plan`. Answers the
   :purge block: \"refused\" with nothing deleted, or \"applied\".

   :purged is the count the port answered, or nil when the port did not
   answer a non-negative integer (then :reason says so; the count is unknown,
   it is not assumed to be the planned one). :purged-ids are the ids handed
   to the port, capped at CAP."
  [sources vectors candidates base cap]
  (let [fresh (reconcile/store-ids sources)
        why   (listing-refusal sources)
        plan  (when-not why (reconcile/purge-plan fresh candidates))
        why   (or why (reconcile/purge-refusal fresh vectors plan))]
    (if why
      (assoc base :mode "refused" :reason why)
      (let [n      (if (seq plan) (reconcile/purge-vector-ids! sources plan) 0)
            known? (and (integer? n) (not (neg? n)))]
        (cond-> (assoc base :mode "applied"
                       :planned-count (count plan)
                       :kept-count (- (count candidates) (count plan))
                       :purged (when known? n)
                       :purged-ids (vec (take cap plan)))
          (not known?)
          (assoc :reason (str "the purge port did not report how many records it deleted; "
                              (count plan) " were requested, the deleted count is unknown")))))))

(defn purge-response
  "The response for a purge request over SOURCES, LIMIT and CONFIRM?
   (true only when the handler received i_mean_it as boolean true).

   Deletes only vector-index records, and only through the sources'
   `IOrphanVectorPurge` port; the entry store is read, never written.
   :purge :mode says what happened:
     \"unsupported\" the sources have no vector-side purge (the default
                   scan of a co-located store: there an orphan vector
                   cannot exist), nothing deleted
     \"refused\"     a guard failed and nothing is deleted; :reason says
                   which: the sources do not report listing completeness
                   (no `IListingCompleteness`), a listing did not reach its
                   end, the store listing (or its fresh re-read) is empty
                   while orphan candidates exist, or the plan is more than
                   `reconcile/max-purge-fraction` of the vector ids
     \"dry-run\"     CONFIRM? is not true: the candidates are counted and
                   nothing is deleted; the guards above already ran, so a
                   dry run that would be refused says \"refused\"
     \"applied\"     the store ids were read AGAIN, the guards re-checked,
                   and only candidates still absent from the store were
                   deleted (`reconcile/purge-plan`); the ones the store now
                   holds are counted as :kept-count
   A listing that throws is never read as an empty one: the exception
   leaves this function and nothing is deleted.
   The response's :orphan-vector-ids are the candidates listed BEFORE any
   delete (:purge :orphan-vector-ids-as-of); :purge :purged-ids names what
   was handed to the port. :purge :purged is nil when the port did not
   report a count. Rows with no usable id are never purged (they have no
   id to delete by); :skipped-nil-ids counts them."
  [sources limit confirm?]
  (let [store      (reconcile/store-ids sources)
        vectors    (reconcile/vector-ids sources)
        rep        (reconcile/diff-ids store vectors)
        candidates (:orphan-vector-ids rep)
        cap        (effective-limit limit)
        base       {:candidate-count (count candidates)
                    :skipped-nil-ids (:nil-vector-ids rep)
                    :orphan-vector-ids-as-of "before-purge"
                    :purged          0}
        purge      (if-not (satisfies? reconcile/IOrphanVectorPurge sources)
                     (assoc base :mode "unsupported"
                            :reason "these sources have no vector-side purge; nothing deleted")
                     (if-let [why (or (listing-refusal sources)
                                      (reconcile/purge-refusal store vectors candidates))]
                       (assoc base :mode "refused" :reason why)
                       (if-not confirm?
                         (assoc base :mode "dry-run"
                                :reason "pass purge=true and i_mean_it=true to delete these vector records")
                         (apply-purge! sources vectors candidates base cap))))]
    (assoc (reconcile/truncate-report rep cap)
           :status "ok"
           :purge purge)))

(defn handle-reconcile
  "Report drift between the entry store and the vector index. Installed
   sources win; otherwise the active store's own id scan is used when it has
   one, and the answer stays not-wired only when it has none.

   Read-only unless :purge is true (or \"true\"; it only selects the purge
   report). Even then it is a dry run unless :i_mean_it reaches this handler
   as BOOLEAN true: a string, \"true\" included, is not a confirmation here.
   The `multi` tool decodes params against the memory tool's schema before
   calling (`hive-mcp.multi.param-coerce`), and i_mean_it is declared
   boolean there, so a string its boolean decoder reads as true (\"true\",
   \"yes\", \"1\", any case) arrives here as boolean true and does confirm.
   This handler cannot tell that from a JSON true. See `purge-response`."
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
