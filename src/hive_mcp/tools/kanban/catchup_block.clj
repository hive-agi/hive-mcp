(ns hive-mcp.tools.kanban.catchup-block
  "The kanban contribution to catchup: bucket counts and the most recent
   todos of the caller's project and its descendants, registered as a
   catchup block (hive-spi.catchup.registry) under :kanban."
  (:require [clojure.string :as str]
            [hive-dsl.result :refer [ok ok? let-ok try-effect*]]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.tools.catchup.scope-filter :as sf]
            [hive-mcp.tools.kanban.list.plan :as list-plan]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.tools.kanban.transitions :as kt]
            [hive-mcp.tools.memory-kanban.query :as mkq]
            [hive-spi.catchup.registry :as blocks]
            [hive-mcp.tools.kanban.schema :as ks]
            [malli.core :as m]))
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

(def status-buckets
  "Count bucket per status tag, in display order."
  [[:todo "todo"] [:inprogress "doing"] [:inreview "review"] [:done "done"]])

(defn board-view
  "What the block reads for PROJECT-ID: {:scope-spec :view}. :scope-spec asks
   an IBoardSource for the board; :view ({:scope-tags :visible-ids}) is what
   `gate-board` admits from it, nil for an ungated read.
     - a project: its own and its descendants' cards, never its ancestors',
       under the HCR view (`sf/hcr-view`) over those same projects;
     - \"global\": the cards stored under project_id \"global\", minus those
       tagged `scope:project:X` for a project X;
     - nil: the whole board, ungated."
  [project-id]
  (cond
    (sf/in-project? project-id)
    (let [ids (vec (distinct (cons project-id (project-scope/descendant-scopes project-id))))]
      {:scope-spec {:project-id           project-id
                    :include-descendants? true
                    :include-ancestors?   false}
       :view       (sf/hcr-view project-id ids (map project-scope/scope->tag ids))})

    (= "global" project-id)
    {:scope-spec {:project-id           "global"
                  :include-descendants? false
                  :include-ancestors?   false}
     :view       {:scope-tags  #{"scope:project:global"}
                  :visible-ids #{"global"}}}

    :else
    {:scope-spec {:project-id           nil
                  :include-descendants? true
                  :include-ancestors?   false}
     :view       nil}))

(defn gate-board
  "The ENTRIES `sf/hcr-visible?` admits under VIEW ({:scope-tags
   :visible-ids}); every entry when VIEW is nil."
  [view entries]
  (vec (if view
         (sf/scope-filter-entries entries (:scope-tags view) (:visible-ids view))
         entries)))

(defn- tagged? [entry tag] (boolean (some #{tag} (:tags entry))))

(defn summarize-board
  "Summary of the gated board ENTRIES: {:counts {bucket n} :projects n
   :recent-todos [...]}. A card counts in every `status-buckets` bucket whose
   tag it carries; :projects is the number of distinct projects (scope tag,
   else :project-id); :recent-todos are the ten most recently :updated todo
   cards as {:id :title :tags}."
  [entries]
  {:counts       (into {} (map (fn [[k tag]] [k (count (filter #(tagged? % tag) entries))]))
                       status-buckets)
   :projects     (count (distinct (keep #(or (kt/extract-project-id-from-tags %) (:project-id %))
                                        entries)))
   :recent-todos (->> entries
                      (filter #(tagged? % "todo"))
                      (sort-by :updated #(compare %2 %1))
                      (take 10)
                      (mapv (fn [e] {:id (:id e) :title (entry-title e) :tags (:tags e)})))})

(defn gather-kanban-summary
  "Bucket counts, project count and the ten most recently updated todos of
   PROJECT-ID's board (see `board-view`), read in one fetch from
   `mkq/*board-source*` and gated by `gate-board`. Cards carrying a
   `list-plan/default-hidden-tags` tag are not counted.

   Returns {:counts {:todo n :inprogress n :inreview n :done n} :projects n
   :recent-todos [{:id :title :tags} ...] :scope-tag str-or-nil}; the empty
   summary (with :scope-tag) when the board read fails."
  [project-id]
  (let [scope-tag    (when project-id (str "scope:project:" project-id))
        empty-result (assoc empty-summary :scope-tag scope-tag)
        {:keys [scope-spec view]} (board-view project-id)
        result (let-ok [board (try-effect* :kanban/board-failed
                                (src/scoped-board mkq/*board-source* scope-spec
                                                  {:required-tags ["kanban"]
                                                   :window        list-plan/whole-board}))]
                 (ok (-> (:entries board)
                         (list-plan/select-tagged ["kanban"])
                         (list-plan/drop-hidden list-plan/default-hidden-tags)
                         (->> (gate-board view))
                         summarize-board)))]
    (if (ok? result) (merge empty-result (:ok result)) empty-result)))

(def ^:private hint
  "Kanban summary for current project scope. Use mcp__hive__project kanban list status=todo|inprogress for full rows.")

(defn catchup-section
  "The catchup section for SUMMARY: nil when the board is empty (an empty
   board is not news), else the summary with its usage hint."
  [{:keys [counts recent-todos] :as summary}]
  (when (or (pos? (apply + (vals (or counts {}))))
            (seq recent-todos))
    (assoc summary :hint hint)))

(def block
  "The catchup block contribution. Reads :project-id from the context.
   Cached per project: one board scan is the same answer for every agent
   catching up on one project, and a kanban-tagged write drops the entry."
  {:block/id    :kanban
   :block/fn    (fn [{:keys [project-id]}] (catchup-section (gather-kanban-summary project-id)))
   :block/order 40
   :block/cache {:key-by [:project-id] :drop-tags #{"kanban"}}})

(defn register!
  "Install the kanban block in the catchup block registry. Returns it."
  []
  (blocks/register-block! block))

(m/=> gather-kanban-summary [:=> [:cat [:maybe :string]] ks/Summary])
(m/=> catchup-section [:=> [:cat ks/SummaryIn] ks/Section])
(m/=> register! [:=> [:cat] ks/CatchupBlock])
(m/=> board-view [:=> [:cat [:maybe ks/ProjectId]] ks/BoardView])
(m/=> gate-board [:=> [:cat ks/BoardGate [:maybe [:sequential ks/EntryIn]]] [:vector ks/EntryIn]])
(m/=> summarize-board [:=> [:cat [:maybe [:sequential ks/EntryIn]]] ks/BoardSummary])
