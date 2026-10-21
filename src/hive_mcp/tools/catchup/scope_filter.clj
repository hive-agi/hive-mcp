(ns hive-mcp.tools.catchup.scope-filter
  "Pure filter/sort helpers for catchup scope reasoning.

   No I/O, no caches — given entries + scope data, return filtered entries.
   Deliberately keyword-only so the caller can pre-compute
   `full-hierarchy-scope-tags` once and share it across branches."
  (:require [hive-mcp.project.scope :as project-scope]
            [hive-mcp.dns.result :refer [rescue]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn distinct-by
  "Return distinct elements from coll by the value of (f item)."
  [f coll]
  (let [seen (volatile! #{})]
    (filterv (fn [item]
               (let [key (f item)]
                 (if (contains? @seen key)
                   false
                   (do (vswap! seen conj key) true))))
             coll)))

(defn newest-first
  "Sort entries by :created timestamp, newest first."
  [entries]
  (sort-by :created #(compare %2 %1) entries))

(defn filter-by-tags
  "Filter entries to only those containing all specified tags."
  [entries tags]
  (if (seq tags)
    (filter (fn [entry]
              (let [entry-tags (set (:tags entry))]
                (every? #(contains? entry-tags %) tags)))
            entries)
    entries))

(defn in-project?
  "True iff PROJECT-ID names a project, i.e. is neither nil nor \"global\"."
  [project-id]
  (boolean (and project-id (not= project-id "global"))))

(defn compute-full-scope-tags
  "Compute full hierarchy scope tags for in-memory safety-net filtering."
  [project-id]
  (cond-> (project-scope/full-hierarchy-scope-tags project-id)
    (in-project? project-id) (disj "scope:global")))

(def ^:private global-scope-tag "scope:global")

(defn- scope-tag?
  "True iff T is a scope tag: `scope:global` or `scope:project:*`."
  [t]
  (let [t (str t)]
    (or (= global-scope-tag t) (.startsWith ^String t "scope:project:"))))

(defn hcr-visible?
  "True iff ENTRY is visible from the hierarchy given as VISIBLE-SCOPE-TAGS
   (the `scope:project:*` tags of the visible projects) and VISIBLE-IDS (the
   visible project ids, \"global\" included).

   A scope tag is authoritative. An entry carrying any scope tag is visible
   iff it carries `scope:global` or a tag in VISIBLE-SCOPE-TAGS, whatever its
   :project-id. An entry with no scope tag is visible iff its :project-id is
   in VISIBLE-IDS."
  [entry visible-scope-tags visible-ids]
  (let [tags        (map str (:tags entry))
        scope-tags  (if (set? visible-scope-tags) visible-scope-tags (set visible-scope-tags))
        visible-ids (if (set? visible-ids) visible-ids (set visible-ids))]
    (if (some scope-tag? tags)
      (boolean (some #(or (= global-scope-tag %) (contains? scope-tags %)) tags))
      (contains? visible-ids (:project-id entry)))))

(defn scope-filter-entries
  "ENTRIES for which `hcr-visible?` holds under SCOPE-TAGS and VISIBLE-IDS."
  [entries scope-tags visible-ids]
  (let [scope-tags  (set scope-tags)
        visible-ids (set visible-ids)]
    (filter #(hcr-visible? % scope-tags visible-ids) entries)))

(defn hcr-view
  "The HCR view of PROJECT-ID: {:in-project? :scope-tags :visible-ids}.
   HIERARCHY-IDS are the project ids visible from PROJECT-ID; :visible-ids is
   that set plus \"global\". SCOPE-TAGS default to `compute-full-scope-tags`."
  ([project-id hierarchy-ids]
   (hcr-view project-id hierarchy-ids (compute-full-scope-tags project-id)))
  ([project-id hierarchy-ids scope-tags]
   {:in-project? (in-project? project-id)
    :scope-tags  (set scope-tags)
    :visible-ids (conj (set hierarchy-ids) "global")}))

(defn hcr-gate
  "ENTRIES visible under VIEW (see `hcr-view`), as a vector. A view at global
   scope (:in-project? false) passes ENTRIES through unchanged."
  [view entries]
  (vec (if (:in-project? view)
         (scope-filter-entries entries (:scope-tags view) (:visible-ids view))
         entries)))

(defn gate-branches
  "BRANCHES (key -> rows) with every row set gated under VIEW: keys in
   ALWAYS-GATED through `scope-filter-entries` at every scope, the others
   through `hcr-gate`. Returns key -> vector of rows."
  [view always-gated branches]
  (into {}
        (map (fn [[k rows]]
               [k (if (contains? always-gated k)
                    (vec (scope-filter-entries rows (:scope-tags view) (:visible-ids view)))
                    (hcr-gate view rows))]))
        branches))

(defn scope-filter-entries-strict
  "Strict variant of `scope-filter-entries` — keeps only entries whose
   tags explicitly intersect `scope-tags`. Does NOT fall back to
   `:project-id ∈ visible-ids`.

   Step 10 tripwire (memory `20260504173159-46dc47f1`): apply this to
   wrap/session entries once step-6 guarantees explicit `scope:project:*`
   tags at write-time. Any future writer regression that drops the tag
   will surface as missing-from-catchup rather than as silent bleed via
   the project-id fallback."
  [entries scope-tags]
  (filter (fn [entry]
            (let [entry-tags (set (or (:tags entry) []))]
              (boolean (some entry-tags scope-tags))))
          entries))

(defn scope-pierce-entries
  "The axiom, catchup-priority and session-summary entries of ENTRIES, the
   kinds a global-pierce fetch carries into a project's catchup. Selection
   only: the caller still gates the result with `hcr-gate`. Returns nil when
   caller is at global scope (no piercing needed).

   Entry :type may be a keyword (:axiom) or a string (\"axiom\") depending on the
   store path; `name` normalises both."
  [entries project-id]
  (when (in-project? project-id)
    (filter (fn [entry]
              (let [entry-tags (set (or (:tags entry) []))
                    entry-type (name (or (:type entry) ""))]
                (or (= entry-type "axiom")
                    (contains? entry-tags "catchup-priority")
                    (contains? entry-tags "session-summary"))))
            entries)))

(defn entry-expiring-soon?
  "Check if entry expires within 7 days."
  [entry]
  (when-let [exp (:expires entry)]
    (rescue false
            (let [exp-time (java.time.ZonedDateTime/parse exp)
                  now (java.time.ZonedDateTime/now)
                  week-later (.plusDays now 7)]
              (.isBefore exp-time week-later)))))