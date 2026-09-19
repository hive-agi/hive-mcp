(ns hive-mcp.tools.kanban.catchup-block
  "The kanban contribution to catchup: bucket counts and the most recent
   todos for the caller's project, registered as a hive-spi catchup block
   under :kanban."
  (:require [clojure.string :as str]
            [hive-dsl.result :refer [ok ok? let-ok try-effect* ok->]]
            [hive-mcp.tools.kanban.list.plan :as list-plan]
            [hive-mcp.vectordb.kanban-facade :as kanban-facade]
            [hive-spi.catchup.registry :as blocks]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def empty-summary
  "The block's value when the board cannot be read."
  {:counts {} :recent-todos []})

(defn- entry-title [e]
  (or (get-in e [:content :title])
      (when (string? (:content e))
        (first (str/split-lines (:content e))))
      "(no title)"))

(defn gather-kanban-summary
  "Bucket counts and the ten most recently updated todos scoped to
   PROJECT-ID. Returns {:counts {:todo n :inprogress n :inreview n :done n}
   :recent-todos [{:id :title :tags} ...] :scope-tag str-or-nil}; the empty
   summary (with :scope-tag) when any board read fails."
  [project-id]
  (let [scope-tag    (when project-id (str "scope:project:" project-id))
        base-tags    (cond-> ["kanban"] scope-tag (conj scope-tag))
        empty-result (assoc empty-summary :scope-tag scope-tag)
        count-into   (fn [acc bucket tag]
                       (let-ok [n (try-effect* :kanban/count-failed
                                    (count (kanban-facade/query-entries
                                            :type "note"
                                            :tags (conj base-tags tag)
                                            :limit list-plan/whole-board
                                            :output-fields ["id"])))]
                         (ok (assoc-in acc [:counts bucket] n))))
        attach-recent (fn [acc]
                        (let-ok [rows (try-effect* :kanban/recent-failed
                                        (kanban-facade/query-entries
                                         :type "note"
                                         :tags (conj base-tags "todo")
                                         :limit 10
                                         :order-by [:updated :desc]
                                         :output-fields ["id" "content" "tags"]
                                         :include-content? true))]
                          (ok (assoc acc :recent-todos
                                     (mapv (fn [e] {:id (:id e) :title (entry-title e) :tags (:tags e)})
                                           rows)))))
        result (ok-> (ok empty-result)
                     (count-into :todo       "todo")
                     (count-into :inprogress "doing")
                     (count-into :inreview   "review")
                     (count-into :done       "done")
                     attach-recent)]
    (if (ok? result) (:ok result) empty-result)))

(def block
  "The catchup block contribution. Reads :project-id from the context."
  {:block/id    :kanban
   :block/fn    (fn [{:keys [project-id]}] (gather-kanban-summary project-id))
   :block/order 40})

(defn register!
  "Install the kanban block in the catchup block registry. Returns it."
  []
  (blocks/register-block! block))
