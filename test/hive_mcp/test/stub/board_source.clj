(ns hive-mcp.test.stub.board-source
  "In-memory IBoardSource (hive-mcp.tools.kanban.list.source) over a seq of
   kanban entries and a fixture hierarchy, recording every call.

   `scoped-board` answers what FacadeBoardSource answers over a memory store
   holding the same entries under the same hierarchy (held to that by
   hive-mcp.tools.kanban.board-source-stub-conformance-test):
     - `:scope \"all\"`, a nil :project-id, or \"global\" with
       :include-descendants? -> every row;
     - otherwise rows whose :project-id is the project, its ancestors and
       \"global\" unless :include-ancestors? is false, and its descendants
       when :include-descendants? is set;
   then unexpired rows of :type note carrying every :required-tags tag, cut
   to :window. :multi-project? is true for `:scope \"all\"`, for \"global\",
   and for more than one visible id."
  (:require [hive-mcp.test.stub.hierarchy :as hier]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.test.stub.memory-query :as mq]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- visible-ids
  "Project ids a scope spec reads under PARENTS; nil for a whole-board read."
  [parents {:keys [project-id include-descendants? include-ancestors? scope]}]
  (when-not (or (= scope "all")
                (nil? project-id)
                (and (= project-id "global") include-descendants?))
    (cond-> #{project-id}
      (and (not= project-id "global") (not (false? include-ancestors?)))
      (into (conj (hier/ancestors-of parents project-id) "global"))
      (and (not= project-id "global") include-descendants?)
      (into (hier/descendants-of parents project-id)))))

(defrecord StubBoardSource [parents entries calls]
  src/IBoardSource
  (scoped-board [_ {:keys [project-id scope] :as scope-spec} {:keys [required-tags window] :as plan}]
    (swap! calls conj [scope-spec plan])
    (let [ids  (visible-ids parents scope-spec)
          now  (System/currentTimeMillis)
          rows (->> entries
                    (filter #(= "note" (some-> (:type %) name)))
                    (filter #(or (nil? ids) (contains? ids (:project-id %))))
                    (filter #(every? (set (:tags %)) required-tags))
                    (remove #(mq/expired? % now))
                    (take (or window 100))
                    vec)]
      {:entries        rows
       :multi-project? (boolean (or (= scope "all")
                                    (= project-id "global")
                                    (and ids (next ids))))})))

(defn ->board-source
  "A StubBoardSource over ENTRIES under PARENTS ({project-id parent-or-nil})."
  [parents entries]
  (->StubBoardSource parents (vec entries) (atom [])))

(defn calls
  "Recorded [scope-spec fetch-plan] pairs, oldest first."
  [source]
  @(:calls source))
