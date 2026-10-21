(ns hive-mcp.tools.memory.reconcile
  "`memory reconcile` verb: report vector-index ids absent from the entry
   store and entry-store ids absent from the vector index.

   Read-only by construction. The verb asks an `IReconcileSources` port for
   the two id listings and renders `hive-mcp.memory.reconcile/report`. It
   has no purge or repair mode in this slice, so it performs no store write.

   The live adapter (store listing plus vector-index listing) is not wired
   yet: until a composition root installs sources with `set-sources!`, the
   verb answers {:status \"not-wired\"} rather than guessing. That is the
   stub boundary of the first slice; tests install a port stub.

   Params:
     :limit  cap on each listed id vector (default 100, min 1)"
  (:require [hive-mcp.memory.reconcile :as reconcile]
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
     :message "no reconcile sources installed; the live store/vector adapter is a follow-up slice"}
    (assoc (reconcile/truncate-report (reconcile/report sources)
                                      (effective-limit limit))
           :status "ok")))

(defn handle-reconcile
  "Report drift between the entry store and the vector index. Read-only."
  [{:keys [limit]}]
  (rescue-log "[memory] reconcile" (mcp-error "reconcile failed")
    (mcp-json (reconcile-response @-sources limit))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> set-sources! [:=> [:cat [:maybe ms/ReconcileSources]] [:maybe ms/ReconcileSources]])
(m/=> clear-sources! [:=> [:cat] :nil])

(m/=> reconcile-response
      [:=> [:cat [:maybe ms/ReconcileSources] [:any {:gen/schema [:maybe [:or :int [:enum "3" "x" "0"]]]}]]
       ms/ReconcileResponse])

(m/=> handle-reconcile [:=> [:cat ms/McpParams] tcore/McpResponse])
