(ns hive-mcp.tools.catchup.hierarchy
  "Hierarchy project-id resolution + single-query hierarchy fetch.

   Computes the visible project-id set for scope filtering and pulls the
   hierarchy's metadata rows in ONE `{:project-ids [...]}` query. The
   per-project fairness cap the old one-RPC-per-descendant fan-out (H12)
   provided is applied in memory after the fetch. Pure infrastructure: no
   cache, no side effects besides the backend query."
  (:require [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.resilience.store :refer [with-resilience]]
            [hive-mcp.tools.catchup.shapes :as shapes]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def metadata-projection
  "Phase-1 scan: 6-field metadata projection. Drops content + embedding +
   document. Hydrate-buckets does a second batch-get via IMemoryStoreBatch
   for the trimmed survivor set (~180 rows) with full default-read-fields.
   Full projection on 300-row hierarchy scan stalls past 180s;
   metadata-only lands at ~40s cold.

   PUBLIC — shared with axiom_cache + bundle modules."
  ["id" "type" "tags" "created" "expires" "project_id"])

(def bundle-hierarchy-limit
  "Overall cap on hierarchy entries pulled in one shot. Metadata-only scan at
   ~7s fixed + 54ms/row → 300 rows ≈ 23s. Post-projection-prune 300 is enough
   headroom over the ~265 per-type caps.

   PUBLIC — used by chunked-hierarchy-fetch default branch + bundle."
  300)

(def ^:private hierarchy-chunk-size
  "LEGACY (pre-H12): IN-list chunk size for the `project-ids in [...]` scan.
   Retained so `chunk-count` keeps a stable signature for bundle.clj's
   per-chunk-limit math. Under H12 the per-descendant fan-out ignores it;
   only `chunk-count` still consults it for the legacy partition shape."
  15)

(def ^:private per-descendant-limit
  "Per-descendant entry cap. Most descendants hold a handful of entries;
   50 keeps the fan-out total well under `bundle-hierarchy-limit` × N
   without truncating an unusually rich descendant. Empirically the per-type
   bundle caps (100 axioms, 50 decisions, 50 conventions) bound the
   downstream consumption anyway, so the cap protects against runaway
   metadata transport on a single noisy branch."
  50)

(defn compute-hierarchy-project-ids
  "Compute the full set of visible project IDs for DB-level filtering."
  [project-id]
  (let [in-project? (and project-id (not= project-id "global"))]
    (when in-project?
      (let [visible (project-scope/visible-scopes project-id)
            descendants (project-scope/descendant-scopes project-id)
            all-ids (distinct (concat visible descendants))]
        (vec (remove #(= "global" %) all-ids))))))

;; =============================================================================
;; Single-query hierarchy fetch + in-memory per-project cap
;; =============================================================================

(def ^:private max-query-limit
  "Upper bound on a single backend query's row limit (Milvus caps
   offset+limit at 16384)."
  16384)

(defn single-query-limit
  "Row limit for the single hierarchy query over `n` projects: the same
   total the per-descendant fan-out could return (`per-descendant-limit`
   rows per project), bounded by `max-query-limit`."
  [n]
  (min max-query-limit (* per-descendant-limit (max 1 n))))

(defn cap-per-project
  "Keep at most `cap` entries per `:project-id`, newest `:created` first,
   and return them as one vector ordered newest-first. Restores, in memory,
   the per-descendant fairness the old one-RPC-per-project fan-out gave."
  [cap entries]
  (->> entries
       (group-by :project-id)
       vals
       (mapcat #(take cap (sort-by :created (fn [a b] (compare b a)) %)))
       (sort-by :created (fn [a b] (compare b a)))
       vec))

(defn chunked-hierarchy-fetch
  "One IN-list query for the whole hierarchy, replacing the H12
   per-descendant fan-out (one RPC per descendant: ~190 RPCs, 8.3 s per
   catchup on the live workspace). The backend gets
   `{:project-ids (vec hierarchy-ids)}` once, with a limit sized so every
   project still fits its `per-descendant-limit` share, and the same
   metadata projection. The per-project cap is then applied in memory by
   `cap-per-project` (newest first), so one noisy descendant still cannot
   crowd the others out. `per-chunk-limit` is accepted for signature
   compatibility with bundle.clj and ignored."
  [store hierarchy-ids _per-chunk-limit]
  (if (empty? hierarchy-ids)
    []
    (let [ids (vec hierarchy-ids)]
      (cap-per-project
        per-descendant-limit
        (or (with-resilience
              (mem-proto/query-entries store
                {:project-ids   ids
                 :limit         (single-query-limit (count ids))
                 :output-fields metadata-projection}))
            [])))))

(defn chunk-count
  "Number of parallel chunks the fan-out will produce for `hierarchy-ids`.
   Exposed so bundle.clj can compute per-chunk limits without duplicating
   the partition-all arithmetic."
  [hierarchy-ids]
  (max 1 (count (partition-all hierarchy-chunk-size hierarchy-ids))))

(m/=> single-query-limit [:=> [:cat [:int {:min 0 :max 100000}]] [:int {:min 1 :max 16384}]])

(m/=> cap-per-project [:=> [:cat pos-int? [:sequential shapes/Entry]] [:vector shapes/Entry]])

;; =============================================================================
;; Contracts
;; =============================================================================

(def HierarchyIds
  "Project ids for the hierarchy fan-out, or nil."
  [:maybe [:sequential [:string {:gen/schema [:enum "p" "child-a" "child-b"]}]]])

(m/=> compute-hierarchy-project-ids [:=> [:cat shapes/ProjectId] [:maybe [:vector :string]]])
(m/=> chunked-hierarchy-fetch
      [:=> [:cat mem-proto/MemoryStore HierarchyIds [:maybe :int]] [:vector shapes/Entry]])
(m/=> chunk-count
      [:=> [:cat [:maybe [:or [:sequential :any] [:set :any]]]] pos-int?])
