(ns hive-mcp.tools.consolidated.workflow.execution-routing
  "Which forge tasks a spawn mode that cannot honor per-task execution dispatches.

   A card routed with `[:context :execution]` (provider, model, spawn mode,
   persona) needs a ling of its own. `:orchestrator` bundles every task into one
   ling's Task subagents, so it cannot honor that routing. It must not throw
   for the whole batch either: each routed task is REJECTED and reported, while
   the rest of the strike is dispatched.

   Strata (each calls only downward):

     value objects  ForgeTask, Rejection, Partition   malli, generator-capable
     sum type       ForgeTaskDisposition              one decision per task
     pure fns       disposition, reject-execution-routed

   The shapes here are the single source: the `m/=>` contracts and the
   synthesized property tests both read from them."
  (:require [malli.core :as m]
            [hive-dsl.adt :refer [defadt adt-case]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value Objects
;; =============================================================================

(def TaskId [:string {:min 1}])

(def Execution
  "Per-task execution settings as a card carries them. Open: forge reads the
   keys it knows (provider, model, spawn_mode, presets, persona)."
  [:map-of :keyword :any])

(def ForgeTask
  "A surveyed forge task, as far as execution routing needs to see it."
  [:map
   [:id TaskId]
   [:title {:optional true} :string]
   [:context {:optional true} [:map [:execution {:optional true} Execution]]]])

(def UnroutableMode
  "The spawn modes that cannot honor per-task execution."
  [:enum :orchestrator])

(def Rejection
  "One task a mode refused, as reported in a spark result's `:failed`."
  [:map {:closed true}
   [:task-id    TaskId]
   [:task-title [:string {:min 1}]]
   [:spawned    [:= false]]
   [:route      UnroutableMode]
   [:type       [:= :execution/unsupported-mode]]
   [:error      [:string {:min 1}]]])

(def Partition
  "Every input task lands in exactly one of the two, in input order."
  [:map {:closed true}
   [:accepted [:vector ForgeTask]]
   [:rejected [:vector Rejection]]])

;; =============================================================================
;; Sum Type
;; =============================================================================

(defadt ForgeTaskDisposition
  "What an unroutable mode does with one task.

   A closed sum rather than a boolean: `adt-case` over it is exhaustive, so a
   third disposition (say, queue for a ling) breaks every fold that has not
   considered it instead of defaulting to dispatch."
  [:disposition/dispatch {:task map?}]
  [:disposition/reject   {:task map?}])

;; =============================================================================
;; Pure Functions
;; =============================================================================

(def unsupported-mode-message "Per-task execution requires a ling spawn mode")

(defn execution-routed?
  "True when `task` carries per-task execution settings at [:context :execution]."
  [task]
  (boolean (seq (get-in task [:context :execution]))))

(defn disposition
  "The `ForgeTaskDisposition` of one task for an unroutable mode."
  [task]
  (forge-task-disposition (if (execution-routed? task)
                            :disposition/reject
                            :disposition/dispatch)
                          {:task task}))

(defn- ->rejection
  [route {:keys [id title]}]
  {:task-id    id
   :task-title (if (seq title) title id)
   :spawned    false
   :route      route
   :type       :execution/unsupported-mode
   :error      unsupported-mode-message})

(defn reject-execution-routed
  "Split `tasks` for `route`, a mode that cannot honor per-task execution.
   Returns {:accepted tasks-to-dispatch :rejected rejections}, both in task order."
  [route tasks]
  (reduce (fn [acc task]
            (let [d (disposition task)]
              (adt-case ForgeTaskDisposition d
                :disposition/dispatch (update acc :accepted conj (:task d))
                :disposition/reject   (update acc :rejected conj (->rejection route (:task d))))))
          {:accepted [] :rejected []}
          tasks))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> execution-routed? [:=> [:cat ForgeTask] :boolean])
(m/=> reject-execution-routed [:=> [:cat UnroutableMode [:sequential ForgeTask]] Partition])
