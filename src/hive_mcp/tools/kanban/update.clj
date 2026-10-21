(ns hive-mcp.tools.kanban.update
  "A `kanban update` that moves the status AND edits the content, as ONE
   commit.

   Run as a move and then an edit, the two halves were two store writes:
   the store could take one and refuse the other, so a refused description
   left a moved status behind, or the reverse, while the call answered with
   the old status (kanban 20261020212530-132e3ea7). Here the move and the
   edit are computed over one coeffect read and committed as one
   `:kanban/facade-update`, under the same per-id lock and write-outcome
   check as every other kanban verb (`events/dispatch-committed!`)."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive.events.cofx :as cofx]
            [hive.events.fx :as fx]
            [hive.events.router :as router]
            [hive-mcp.memory.temporal :as temporal]
            [hive-mcp.tools.kanban.events :as events]
            [hive-mcp.tools.kanban.predicates :as pred]
            [hive-mcp.tools.kanban.schema :as ks]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn update-fx
  "Pure handler: the effect map of a move to `new-status` plus an edit of
   title/description/priority, with ONE `:kanban/facade-update` carrying the
   moved status and tags and the edited content.

   The edit is applied to the moved entry. A move into `done` hands the
   completion hooks the entry as finally written. The edit's audit record
   rides `:kanban/edit-temporal-record`, beside the move's
   `:kanban/temporal-record`. Returns nil for a missing or non-kanban entry."
  [{:keys [:kanban/entry] :as coeffects}
   [_ {:keys [task-id new-status title description append-description priority]}]]
  (when-let [move (events/move-fx coeffects [:kanban/move {:task-id task-id :new-status new-status}])]
    (let [{moved-content :content moved-tags :tags}
          (get-in move [:kanban/facade-update :payload])
          moved   (assoc entry :content moved-content :tags moved-tags)
          edit    (events/edit-fx {:kanban/entry moved}
                                  [:kanban/edit {:task-id            task-id
                                                 :title              title
                                                 :description        description
                                                 :append-description append-description
                                                 :priority           priority}])
          content (get-in edit [:kanban/facade-update :payload :content])
          tags    (or (get-in edit [:kanban/facade-update :payload :tags]) moved-tags)
          final   (assoc moved :content content :tags tags)]
      (cond-> (assoc move :kanban/facade-update {:task-id task-id
                                                 :payload {:content content :tags tags}})
        (:kanban/notify-done move)      (assoc-in [:kanban/notify-done :entry] final)
        (:kanban/archive-external move) (assoc-in [:kanban/archive-external :entry] final)
        (:kanban/temporal-record edit)  (assoc :kanban/edit-temporal-record
                                               (:kanban/temporal-record edit))))))

(defn register!
  "Register the `:kanban/update` event and the edit-audit effect, after the
   kanban events and effects they build on. Key-addressed, so idempotent and
   reload-safe. Returns true."
  []
  (events/init!)
  (fx/reg-fx :kanban/edit-temporal-record temporal/record-mutation-silent!)
  (router/reg-event-fx
   :kanban/update
   [(cofx/inject-cofx :kanban/entry)
    (cofx/inject-cofx :kanban/project-id)]
   update-fx)
  true)

(defn- request-error
  "The Result error for an update request that cannot be dispatched, or nil."
  [{:keys [task-id new-status title description append-description priority]}]
  (let [given? (fn [v] (and (string? v) (not (str/blank? v))))]
    (cond
      (not (given? (some-> task-id str)))
      (r/err :kanban/invalid-task-id {:message "Update requires :task-id" :given task-id})

      (not (pred/valid-status? new-status))
      (r/err :kanban/invalid-status
             {:message (str "Invalid status: " new-status ". Valid: todo, doing, review, done")
              :given   new-status})

      (not (some given? [title description append-description priority]))
      (r/err :kanban/nothing-to-edit
             {:message "Update requires at least one of :title, :description, :append-description, :priority"
              :task-id task-id})

      (and (given? priority) (not (#{"high" "medium" "low"} priority)))
      (r/err :kanban/invalid-priority
             {:message "Priority must be one of high, medium, low" :given priority}))))

(defn dispatch-update!
  "Move and edit a kanban task in ONE commit. `event-payload` is the union of
   `events/dispatch-move!`'s and `events/dispatch-edit!`'s: :task-id,
   :new-status, and at least one of :title :description :append-description
   :priority (plus :directory). Both land or neither does.

   Returns (r/ok effect-map), or the Result error of an invalid request, a
   missing or non-kanban entry (:kanban/invalid-task), or a store refusal
   (:kanban/write-rejected, :kanban/write-failed)."
  [event-payload]
  (register!)
  (let [normalized (update event-payload :new-status pred/normalize-status)]
    (or (request-error normalized)
        (events/dispatch-committed! [:kanban/update normalized] (:task-id normalized)))))

(m/=> update-fx [:=> [:cat ks/Coeffects ks/Event] [:maybe ks/FxMap]])
(m/=> register! [:=> [:cat] [:= true]])
(m/=> dispatch-update! [:=> [:cat ks/DispatchPayload] ks/Result])
