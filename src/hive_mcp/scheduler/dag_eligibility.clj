(ns hive-mcp.scheduler.dag-eligibility
  "Which kanban cards a DAG run may hand to a ling. Pure.

   - plan scope: a plan's cards are the targets of its `plan --depends-on-->
     task` KG edges, the ones `kanban plan-to-kanban` writes.
   - human-only cards (a `PEDRO DECISION` / `HUMAN ONLY` title, or a
     human-only tag) are never eligible.
   - within a bounded run, only the lowest open `wave:N` is eligible."
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def human-only-tags
  "Tags that reserve a card for a human."
  #{"human-only" "needs-human" "needs-pedro" "decision" "human-decision" "pedro-decision"})

(def Tags
  "A card's tags: any collection of values (rendered with `str`), or nil."
  [:maybe [:or [:sequential :any] [:set :any]]])

(def Task
  "The part of a kanban card these rules read. Open map."
  [:map
   [:id {:optional true} :any]
   [:title {:optional true} :any]
   [:tags {:optional true} Tags]])

(def Todos
  "A collection of cards."
  [:sequential Task])

(def ^:private human-only-title
  #"(?i)\bPEDRO[\s_-]+DECISION\b|\bHUMAN[\s_-]+(?:ONLY|DECISION)\b")

(defn human-only?
  "True when TASK is reserved for a human: its title names a Pedro/human
   decision or it carries one of `human-only-tags`."
  [{:keys [title tags]}]
  (boolean (or (and (string? title) (re-find human-only-title title))
               (some (comp human-only-tags str) tags))))

(m/=> human-only? [:=> [:cat [:maybe Task]] :boolean])

(defn task-wave
  "The N of TASK's `wave:N` tag, or 0 when it has none."
  [{:keys [tags]}]
  (or (some (fn [t]
              (some-> (re-matches #"wave:(\d+)" (str t)) second parse-long))
            tags)
      0))

;; parse-long of a `wave:N` digit run longer than a long answers nil, so the
;; tag falls through to the next one (or 0)
(m/=> task-wave [:=> [:cat [:maybe Task]] [:int {:min 0}]])

(defn lowest-open-wave
  "TODOS restricted to the lowest wave among those not yet CLOSED (a set of
   task ids that completed or failed)."
  [closed todos]
  (let [open (remove #(contains? closed (:id %)) todos)]
    (if (empty? open)
      []
      (let [w (apply min (map task-wave open))]
        (filterv #(= w (task-wave %)) open)))))

;; closed is called as a set (contains?); nil reads as empty
(m/=> lowest-open-wave [:=> [:cat [:maybe [:set :any]] [:maybe Todos]] [:vector Task]])

(defn eligible
  "TODOS a ling may take: human-only cards removed, and, when BOUNDED? (the run
   is limited to a plan's cards), only the lowest open wave."
  [bounded? closed todos]
  (let [machine (remove human-only? todos)]
    (if bounded?
      (lowest-open-wave closed machine)
      (vec machine))))

;; bounded? is read for truthiness
(m/=> eligible [:=> [:cat :any [:maybe [:set :any]] [:maybe Todos]] [:vector Task]])

(defn run-scope
  "The card bound for a run of PLAN-ID: explicit `:task-ids`, else the plan's
   own cards (PLAN-TASKS), else nil (board-wide) when `:board-wide?` is set or
   `:require-plan-scope?` is not. Throws ex-info when a plan scope is required
   and none resolves."
  [plan-id {:keys [task-ids board-wide? require-plan-scope?]} plan-tasks]
  (cond
    (seq task-ids)       (set task-ids)
    board-wide?          nil
    (seq plan-tasks)     (set plan-tasks)
    require-plan-scope?  (throw (ex-info (str "plan " plan-id " has no kanban cards linked to it "
                                              "(run `kanban plan-to-kanban` first), so the run "
                                              "cannot be scoped to it; pass task_ids, or "
                                              "board_wide=true to take every todo on the board.")
                                         {:plan-id plan-id :reason :no-plan-scope}))
    :else                nil))

(def RunScopeOpts
  "The start-dag! opts run-scope reads. Open map."
  [:maybe
   [:map
    [:task-ids {:optional true} [:maybe [:or [:sequential :any] [:set :any]]]]
    [:board-wide? {:optional true} :any]
    [:require-plan-scope? {:optional true} :any]]])

;; plan-tasks is the plan's linked card ids (any seqable, possibly empty)
(m/=> run-scope [:=> [:cat :any RunScopeOpts [:maybe [:or [:sequential :any] [:set :any]]]]
                 [:maybe [:set :any]]])
