(ns hive-mcp.tools.kanban.run-ledger-test
  "Agentic run ledger affordances (convention 20261125100357-6730324c):

   RUN-LEDGER-2  `kanban list status=all` answers every status, so one
                 tag-filtered call is a run's result table.
   RUN-LEDGER-3  `kanban update` takes add_tags / remove_tags and commits
                 them with new_status in ONE write: a verdict close is one call.
   RUN-LEDGER-8  `kanban plan-schema` names the run-ledger tag vocabulary.

   The board is injected through `*board-source*` and the memory port stub;
   no concrete namespace is redefined."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.plan.tool :as plan-tool]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.tools.consolidated.kanban :as kanban-tool]
            [hive-mcp.tools.consolidated.project :as project-tool]
            [hive-mcp.tools.kanban.list.plan :as plan]
            [hive-mcp.tools.kanban.list.schema :as ls]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.tools.kanban.update :as ku]
            [hive-mcp.tools.memory-kanban :as mk]
            [hive-mcp.tools.memory-kanban.query :as query]
            [hive-mcp.vectordb.kanban-facade :as kanban-facade]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private pid "run-ledger")

(defn- card [id status & extra-tags]
  {:id         id
   :type       "note"
   :project-id pid
   :tags       (into ["kanban" status "priority-medium" (str "scope:project:" pid)] extra-tags)
   :content    {:task-type "kanban" :title (str "card " id) :description "d"
                :status status :priority "medium"}})

(def ^:private board
  [(card "s1" "todo" "run:R1")
   (card "s2" "doing" "run:R1")
   (card "s3" "done" "run:R1" "verdict:pass")
   (card "s4" "review" "run:R1")
   (card "other" "done" "run:R2")])

(defn- ids [result]
  (set (map #(get % "id") (json/read-str (:text result)))))

(defn- seeded [f]
  (ms/with-stub-store (fn [] (ms/install! (ms/->stub board)) (f))))

(use-fixtures :each seeded)

;; =============================================================================
;; RUN-LEDGER-2: status=all
;; =============================================================================

(deftest status-all-pushes-no-status-tag
  (is (= ["kanban" "run:R1"] (plan/required-tags {:status "all" :tags ["run:R1"]})))
  (is (= ["kanban" "done" "run:R1"] (plan/required-tags {:status "done" :tags ["run:R1"]})))
  (testing "status=all is a valid list request"
    (is (m/validate ls/ListRequest {:status "all" :tags ["run:R1"]}))))

(deftest status-all-lists-every-status
  (binding [query/*board-source* (src/->seq-source board)]
    (is (= #{"s1" "s2" "s3" "s4"}
           (ids (query/list-slim* {:project_id pid :scope "all" :status "all" :tags ["run:R1"]}))))
    (testing "a concrete status still narrows"
      (is (= #{"s3"} (ids (query/list-slim* {:project_id pid :scope "all" :status "done"
                                             :tags ["run:R1"]})))))))

(deftest status-all-is-advertised
  (doseq [[tool-name schema] [["kanban" (get-in kanban-tool/tool-def [:inputSchema :properties "status"])]
                              ["project" (get-in project-tool/tool-def [:inputSchema :properties "status"])]]]
    (testing tool-name
      (is (some #{"all"} (:enum schema)))
      (is (re-find #"all" (:description schema))))))

;; =============================================================================
;; RUN-LEDGER-3: update with add_tags / remove_tags
;; =============================================================================

(defn- payload [fx] (get-in fx [:kanban/facade-update :payload]))

(deftest update-fx-applies-tag-deltas-in-the-same-commit
  (let [entry (card "s2" "doing" "run:R1" "cleanup:pending")
        fx    (ku/update-fx {:kanban/entry entry :kanban/project-id pid}
                            [:kanban/update {:task-id "s2" :new-status "done"
                                             :add-tags ["verdict:pass"]
                                             :remove-tags ["cleanup:pending"]}])
        {:keys [content tags]} (payload fx)]
    (is (= "done" (:status content)))
    (is (some #{"done"} tags))
    (is (not (some #{"doing"} tags)))
    (is (some #{"verdict:pass"} tags))
    (is (not (some #{"cleanup:pending"} tags)))
    (is (some #{"run:R1"} tags))
    (is (contains? fx :kanban/notify-done))
    (testing "the hooks see the tags as finally written"
      (is (some #{"verdict:pass"} (get-in fx [:kanban/notify-done :entry :tags]))))))

(deftest dispatch-update-accepts-tags-without-an-edit
  (let [res (ku/dispatch-update! {:task-id "s2" :new-status "done" :add-tags ["verdict:pass"]})]
    (is (contains? res :ok) (pr-str res))
    (is (some #{"verdict:pass"} (:tags (payload (:ok res)))))))

(deftest kanban-update-closes-with-a-verdict-in-one-call
  (let [res  (mk/handle-mem-kanban-move {:task_id "s2" :new_status "done"
                                         :add_tags ["verdict:fail-product" "known:F-1"]})
        slim (json/read-str (:text res) :key-fn keyword)
        stored (kanban-facade/get-entry-by-id "s2")]
    (is (not (:isError res)) (pr-str res))
    (is (= "done" (:status slim)))
    (is (= #{"verdict:fail-product" "known:F-1"}
           (set (filter #(re-find #"^(verdict|known):" %) (:tags slim)))))
    (testing "the store holds status and tags together"
      (is (= "done" (get-in stored [:content :status])))
      (is (some #{"verdict:fail-product"} (:tags stored))))))

(deftest kanban-update-with-only-tags-retags
  (let [res    (mk/handle-mem-kanban-move {:task_id "s1" :add_tags ["needs-pedro"]})
        stored (kanban-facade/get-entry-by-id "s1")]
    (is (not (:isError res)) (pr-str res))
    (is (= "todo" (get-in stored [:content :status])))
    (is (some #{"needs-pedro"} (:tags stored)))))

(deftest update-tag-params-are-advertised
  (doseq [k ["add_tags" "remove_tags"]]
    (is (re-find #"update" (get-in kanban-tool/tool-def [:inputSchema :properties k :description])))))

;; =============================================================================
;; RUN-LEDGER-8: plan-schema tag vocabulary
;; =============================================================================

(deftest plan-schema-names-the-tag-vocabulary
  (let [out   (json/read-str (:text (plan-tool/handle-plan-schema {})) :key-fn keyword)
        vocab (:tag-vocabulary out)]
    (is (string? vocab))
    (doseq [prefix ["run:" "scenario:" "persona:" "verdict:" "known:"]]
      (is (str/includes? vocab prefix) prefix))))
