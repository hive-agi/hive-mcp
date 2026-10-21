(ns hive-mcp.extensions.record-hooks
  "Addon seam over records the host copies out of the memory stores: hooks
   that see such a record before it is written or shown, and may rewrite it
   or drop it.

   Each hook point names one record kind and fixes what a hook receives:
     :temporal/mutation  a record-mutation! opts map, before it reaches the
                         temporal trail ({:entry-id :op :data :previous-value
                         :project-id ...})
     :kanban/history     a kanban movement or completed-task row, before the
                         swarm registry stores it ({:task-id :title ...})
     :archive/record     the :da/archive! payload of a done task ({:id ...})
     :export/entry       one memory entry, before a backup or export writes it
     :project/visible    a vector of project-tree entities, before a listing,
                         a descendant set or a scan report shows them

   A hook is (fn [record] record'): it returns the record to keep, or nil to
   drop it. Hooks of a point run in id order on the caller's thread, so a hook
   can read the request context of the call that wrote; the first nil stops
   the chain. A hook that throws reaches the caller of `apply-hooks`. With no
   hook registered, a record passes through untouched.

   Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
   SPDX-License-Identifier: AGPL-3.0-or-later"
  (:require [malli.core :as m]
            [taoensso.timbre :as log]))

(def Point
  "The hook points."
  [:enum :temporal/mutation :kanban/history :archive/record :export/entry :project/visible])

(def ^:private Hook [:fn ifn?])

(defonce ^:private hooks (atom {}))

(defn register!
  "Register (or replace) hook `f` under keyword `id` at `point`. `f` may be a
   var, so a re-evaluated hook applies at once. Returns id."
  [point id f]
  {:pre [(m/validate Point point) (keyword? id) (ifn? f)]}
  (swap! hooks update point (fnil assoc (sorted-map)) id f)
  (log/info "record-hook registered:" point id)
  id)

(m/=> register! [:=> [:cat Point :keyword Hook] :keyword])

(defn unregister!
  "Remove hook `id` from `point`. Safe when absent. Returns id."
  [point id]
  (swap! hooks update point dissoc id)
  id)

(m/=> unregister! [:=> [:cat Point :keyword] :keyword])

(defn registered
  "Hook ids registered at `point`, in run order."
  [point]
  (vec (keys (get @hooks point))))

(m/=> registered [:=> [:cat Point] [:vector :keyword]])

(defn apply-hooks
  "`record` through every hook registered at `point`, in id order. nil when a
   hook dropped it."
  [point record]
  (reduce (fn [rec [_ f]]
            (let [out (f rec)]
              (if (nil? out) (reduced nil) out)))
          record
          (get @hooks point)))

(m/=> apply-hooks [:=> [:cat Point :any] :any])
