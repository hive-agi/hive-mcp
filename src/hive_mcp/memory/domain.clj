(ns hive-mcp.memory.domain
  "ScopeFilter ADT — closed sum type for memory scope filtering.

   Replaces stringly-typed scope params ('all', 'global', nil, project-id)
   with an exhaustive ADT. Parse once at IO boundary, dispatch via adt-case.

   Variants:
     :scope/all     — no filtering (show everything)
     :scope/global  — only scope:global entries
     :scope/project — specific project (+ hierarchy)
     :scope/auto    — auto-derived from directory"
  (:require [malli.core :as m]
            [hive-dsl.adt :refer [defadt adt-case]]
            [hive-mcp.knowledge-graph.scope :as kg-scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defadt ScopeFilter
  "Scope filter for memory queries — closed sum type."
  :scope/all
  :scope/global
  [:scope/project {:project-id string?}]
  [:scope/auto    {:project-id string?}])

;; =============================================================================
;; Boundary Parsing
;; =============================================================================

(defn parse-scope
  "Parse raw scope string + project-id into a ScopeFilter ADT value.
   Called once at IO boundary (handle-query, handle-search-semantic)."
  [scope-str project-id]
  (cond
    (= scope-str "all")    (scope-filter :scope/all)
    (= scope-str "global") (scope-filter :scope/global)
    (some? scope-str)       (scope-filter :scope/project {:project-id scope-str})
    :else                   (scope-filter :scope/auto {:project-id project-id})))

(m/=> parse-scope [:=> [:cat [:maybe :string] [:maybe :string]] [:map [:adt/type :keyword] [:adt/variant :keyword]]])

;; =============================================================================
;; Projection Helpers
;; =============================================================================

(defn scope->project-ids
  "Compute visible project-ids for DB-level filtering.
   Returns [string?] or nil (nil = no filtering)."
  [sf include-descendants?]
  (adt-case ScopeFilter sf
    :scope/all     nil
    :scope/global  ["global"]
    :scope/project (let [pid (:project-id sf)
                         visible (kg-scope/visible-scopes pid)
                         descendants (when include-descendants?
                                       (kg-scope/descendant-scopes pid))]
                     (vec (distinct (concat visible descendants))))
    :scope/auto    (let [pid (:project-id sf)
                         in-project? (and pid (not= pid "global"))
                         visible (kg-scope/visible-scopes pid)
                         filtered (if in-project?
                                    (vec (remove #(= "global" %) visible))
                                    visible)
                         descendants (when include-descendants?
                                       (kg-scope/descendant-scopes pid))]
                     (vec (distinct (concat filtered descendants))))))

(m/=> scope->project-ids [:=> [:cat [:map [:adt/type :keyword] [:adt/variant :keyword]] :boolean] [:maybe [:vector :string]]])

(defn scope->tag-set
  "Compute visible scope tags for in-memory filtering.
   Returns #{string?} or nil (nil = no filtering / pass all)."
  [sf include-descendants?]
  (adt-case ScopeFilter sf
    :scope/all     nil
    :scope/global  #{"scope:global"}
    :scope/project (if include-descendants?
                     (kg-scope/full-hierarchy-scope-tags (:project-id sf))
                     (kg-scope/visible-scope-tags (:project-id sf)))
    :scope/auto    (if include-descendants?
                     (kg-scope/full-hierarchy-scope-tags (:project-id sf))
                     (kg-scope/visible-scope-tags (:project-id sf)))))

(m/=> scope->tag-set [:=> [:cat [:map [:adt/type :keyword] [:adt/variant :keyword]] :boolean] [:maybe [:set :string]]])

(defn scope->effective
  "Compute [effective-pid in-project?] for search dispatch.
   Used by search-semantic* to determine filtering mode."
  [sf]
  (adt-case ScopeFilter sf
    :scope/all     [nil false]
    :scope/global  ["global" false]
    :scope/project [(:project-id sf) true]
    :scope/auto    (let [pid (:project-id sf)]
                     [pid (and pid (not= pid "global"))])))

(defn scope->search-project-ids
  "Project-ids a semantic search may return hits from, as [string?] or nil
   (nil = no project filter).

   Takes the ScopeFilter SF and an options map:
     :include-descendants?  also admit child projects (HCR)
     :strict?               an EXPLICIT project scope admits only that project
                            (+ descendants), not its ancestors and global
     :visible-scopes        fn pid -> ancestors-inclusive scopes
     :descendant-scopes     fn pid -> child scopes
   The two fns default to the knowledge-graph scope helpers; tests inject
   stubs. An explicit \"global\" scope is only ever the global entries.
   An auto-derived scope keeps the hierarchy (a child sees its parent): it was
   never something the caller named, so it must not narrow to one project.
   Explicit scope used to behave like auto, which is why scope=\"topic:x\"
   returned off-scope hits from the parent and global scopes
   (kanban 20260519145016-4561f6bc)."
  [sf {:keys [include-descendants? strict? visible-scopes descendant-scopes]}]
  (let [visible     (or visible-scopes kg-scope/visible-scopes)
        descendants (or descendant-scopes kg-scope/descendant-scopes)
        expand      (fn [base pid]
                      (vec (distinct (concat base
                                             (when include-descendants?
                                               (descendants pid))))))]
    (adt-case ScopeFilter sf
      :scope/all     nil
      :scope/global  ["global"]
      :scope/project (let [pid (:project-id sf)]
                       (if strict?
                         (expand [pid] pid)
                         (expand (visible pid) pid)))
      :scope/auto    (let [pid (:project-id sf)]
                       (when (and pid (not= pid "global"))
                         (expand (visible pid) pid))))))

(m/=> scope->search-project-ids [:=> [:cat [:map [:adt/type :keyword] [:adt/variant :keyword]] :map] [:maybe [:vector :string]]])

(m/=> scope->effective [:=> [:cat [:map [:adt/type :keyword] [:adt/variant :keyword]]] [:tuple [:maybe :string] [:maybe :boolean]]])
