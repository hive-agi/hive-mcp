(ns hive-mcp.tools.kanban.predicates
  "Pure predicates and enums for kanban task semantics.

   Extracted from hive-mcp.tools.memory-kanban so transitions and
   event handlers can stay pure."
  (:require [clojure.string :as str]
            [hive-mcp.tools.kanban.schema :as ks]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def status-enum->tag
  "MCP-facing enum -> internal tag value."
  {"inprogress" "doing"
   "inreview"   "review"
   "todo"       "todo"
   "done"       "done"})

(def valid-statuses
  "Internal canonical statuses."
  #{"todo" "doing" "review" "done"})

(def status-tag-set
  "Every token that may legitimately occupy the STATUS slot of a kanban tag
   vector: the canonical internal statuses plus the MCP enum spellings that
   older entries were tagged with. Callers swapping a status tag must remove
   this whole set, never just one spelling."
  (into valid-statuses (keys status-enum->tag)))

(def priority-order
  {"high"          0  "priority-high"   0
   "medium"        1  "priority-medium" 1
   "low"           2  "priority-low"    2})

(defn normalize-status
  "Normalize MCP enum -> internal tag. Pass-through if already canonical."
  [s]
  (get status-enum->tag s s))

(defn valid-status?
  "True iff s is a canonical internal status."
  [s]
  (boolean (valid-statuses s)))

(defn done?
  "True iff status represents a completed task."
  [s]
  (= "done" (normalize-status s)))

(defn kanban-task-type?
  "True iff content map carries the kanban task-type marker."
  [content]
  (some #(= "kanban" (get content %)) [:task-type "task-type"]))

(defn kanban-entry?
  "True iff memory entry is a kanban task (by content shape)."
  [entry]
  (boolean (kanban-task-type? (:content entry))))

(defn reserved-tag?
  "True iff `t` is a tag the kanban verbs own: `kanban`, a status tag in any
   spelling, `priority-*` or `scope:project:*`. A tag delta on `update` may
   not touch them: status moves with new_status, priority with priority, scope
   with retag new_project_id, and dropping `kanban` orphans the card."
  [t]
  (boolean (and (string? t)
                (or (= "kanban" t)
                    (contains? status-tag-set t)
                    (str/starts-with? t "priority-")
                    (str/starts-with? t "scope:project:")))))

(defn reserved-tags
  "The reserved tags (`reserved-tag?`) in `tags`, distinct, in order."
  [tags]
  (if (coll? tags)
    (vec (distinct (filter reserved-tag? tags)))
    []))

(defn coerce-tags
  "A tag parameter as a caller sends it, normalised: nil or a blank string ->
   nil, one tag string -> [tag], a collection of strings -> a vector (nil when
   empty). Anything else (a number, a map, a collection holding a non-string)
   -> :invalid, so the boundary refuses it. Without this a JSON slip like
   add_tags=\"verdict:pass\" became a vector of characters, every one dropped
   as a non-string, and the call answered ok having written no tag."
  [x]
  (cond
    (nil? x)    nil
    (string? x) (when-not (str/blank? x) [x])
    (and (or (sequential? x) (set? x)) (every? string? x)) (not-empty (vec x))
    :else       :invalid))

(m/=> reserved-tag? [:=> [:cat :any] :boolean])
(m/=> reserved-tags [:=> [:cat :any] [:vector :string]])
(m/=> coerce-tags [:=> [:cat :any] [:or [:maybe [:vector :string]] [:= :invalid]]])
(m/=> normalize-status [:=> [:cat [:maybe ks/StatusText]] [:maybe :string]])
(m/=> valid-status? [:=> [:cat [:maybe ks/StatusText]] :boolean])
(m/=> done? [:=> [:cat [:maybe ks/StatusText]] :boolean])
(m/=> kanban-task-type? [:=> [:cat [:maybe [:or ks/ContentMap [:map-of :string :any] :string]]] [:maybe [:= true]]])
(m/=> kanban-entry? [:=> [:cat ks/MaybeEntry] :boolean])
