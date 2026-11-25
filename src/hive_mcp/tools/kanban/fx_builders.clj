(ns hive-mcp.tools.kanban.fx-builders
  "Effect builders for the kanban event handlers: one pure function per
   effect value, composed by the handlers with `cond->` / threading.

   A handler (`hive-mcp.tools.kanban.events`, `hive-mcp.tools.kanban.update`)
   reads coeffects, derives values through the transitions layer
   (`hive-mcp.tools.kanban.transitions`), and assembles its effect map from
   these builders. The interpreters that perform each effect live in
   `hive-mcp.tools.kanban.effects`. Nothing here performs IO."
  (:require [hive-mcp.tools.kanban.schema :as ks]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn edit-payload
  "The facade payload of an edit: `content`, plus `tags` only when the edit
   writes tags (non-nil), so a title edit leaves the store's tags alone."
  [content tags]
  (cond-> {:content content}
    tags (assoc :tags tags)))

(defn facade-update
  "The `:kanban/facade-update` effect value: the soft commit of `payload`
   onto `task-id`."
  [task-id payload]
  {:task-id task-id :payload payload})

(defn temporal-record
  "The `:kanban/temporal-record` effect value: the audit record of `op` on
   `entry-id` with `data`, scoped to `project-id`."
  [entry-id op data project-id]
  {:entry-id entry-id :op op :data data :project-id project-id})

(defn with-facade-update
  "`fx` with its `:kanban/facade-update` set to the commit of `payload`."
  [fx task-id payload]
  (assoc fx :kanban/facade-update (facade-update task-id payload)))

(defn with-temporal-record
  "`fx` with the audit record of `op` under `fx-key` (default
   `:kanban/temporal-record`) when `data` is non-nil; `fx` unchanged when it
   is nil, so a no-op leaves no audit noise."
  ([fx entry-id op data project-id]
   (with-temporal-record fx :kanban/temporal-record entry-id op data project-id))
  ([fx fx-key entry-id op data project-id]
   (cond-> fx
     data (assoc fx-key (temporal-record entry-id op data project-id)))))

(def ^:private Fx
  "An effect map under construction."
  [:map-of :keyword :any])

(m/=> edit-payload [:=> [:cat [:map-of :any :any] [:maybe ks/DeltaTags]] ks/EditPayload])
(m/=> facade-update [:=> [:cat :any [:map-of :keyword :any]] ks/FacadeUpdate])
(m/=> temporal-record [:=> [:cat :any ks/TemporalOp :map :any] ks/TemporalRecord])
(m/=> with-facade-update [:=> [:cat Fx :any [:map-of :keyword :any]] Fx])
(m/=> with-temporal-record [:function
                            [:=> [:cat Fx :any ks/TemporalOp [:maybe :map] :any] Fx]
                            [:=> [:cat Fx :keyword :any ks/TemporalOp [:maybe :map] :any] Fx]])
