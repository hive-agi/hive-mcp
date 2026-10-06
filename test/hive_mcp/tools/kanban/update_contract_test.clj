(ns hive-mcp.tools.kanban.update-contract-test
  "Boundary contract of `kanban update`, end to end through the consolidated
   handler and the multi surfaces, over the atom-backed stub memory store.

   Card 20260913181359-78ad3db5: an update naming no `new_status` never
   changes the status. A description-only or priority-only edit leaves the
   status tag and content status exactly where they were, through the direct
   `kanban update`, a multi `operations` op and a multi DSL `b>`."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.tools.consolidated.kanban :as ck]
            [hive-mcp.tools.consolidated.multi :as cm]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *store* nil)

(def ^:private task-id "20261005000000-c0ffee01")

(defn- seed-task
  "A todo kanban entry at medium priority."
  []
  {:id      task-id
   :type    "note"
   :tags    ["kanban" "todo" "priority-medium" "scope:project:hive"]
   :content {:task-type   "kanban"
             :title       "original title"
             :description "original description"
             :status      "todo"
             :priority    "medium"}})

(defn- with-seeded-store [f]
  (let [store (ms/->stub [(seed-task)])]
    (ms/with-stub-store
      (fn []
        (ms/install! store)
        (binding [*store* store] (f))))))

(use-fixtures :each with-seeded-store)

(defn- stored [] (get (ms/entries *store*) task-id))

(defn- status-of
  "[status-tag content-status] of the stored task."
  []
  (let [{:keys [tags content]} (stored)]
    [(some #{"todo" "doing" "inprogress" "review" "inreview" "done"} tags)
     (:status content)]))

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(deftest direct-update-without-status-keeps-status
  (testing "description-only"
    (let [resp (ck/handle-kanban {:command "update" :task_id task-id
                                  :description "new description"})]
      (is (not (:isError resp)))
      (is (= "todo" (:status (body resp))))
      (is (= ["todo" "todo"] (status-of)))
      (is (= "new description" (get-in (stored) [:content :description])))))
  (testing "priority-only"
    (let [resp (ck/handle-kanban {:command "update" :task_id task-id :priority "high"})]
      (is (not (:isError resp)))
      (is (= "todo" (:status (body resp))))
      (is (= "high" (:priority (body resp))))
      (is (= ["todo" "todo"] (status-of)))
      (is (some #{"priority-high"} (:tags (stored))))))
  (testing "title-only"
    (ck/handle-kanban {:command "update" :task_id task-id :title "new title"})
    (is (= ["todo" "todo"] (status-of)))))

(deftest multi-operations-update-without-status-keeps-status
  (cm/handle-multi {:operations [{"id" "a" "tool" "kanban" "command" "update"
                                  "task_id" task-id "description" "via operations"}]})
  (is (= "via operations" (get-in (stored) [:content :description])))
  (is (= ["todo" "todo"] (status-of)))
  (cm/handle-multi {:operations [{"id" "b" "tool" "kanban" "command" "update"
                                  "task_id" task-id "priority" "low"}]})
  (is (= "low" (get-in (stored) [:content :priority])))
  (is (= ["todo" "todo"] (status-of))))

(deftest multi-dsl-b>-without-status-keeps-status
  (testing "b> with the advertised id param, description only"
    (cm/handle-multi {:dsl [["b>" {"id" task-id "description" "via dsl"}]]})
    (is (= "via dsl" (get-in (stored) [:content :description])))
    (is (= ["todo" "todo"] (status-of))))
  (testing "b> with task_id, priority only"
    (cm/handle-multi {:dsl [["b>" {"task_id" task-id "priority" "high"}]]})
    (is (= "high" (get-in (stored) [:content :priority])))
    (is (= ["todo" "todo"] (status-of)))))

(deftest update-with-status-still-moves
  (ck/handle-kanban {:command "update" :task_id task-id :new_status "inreview"
                     :description "moved and edited"})
  (is (= "moved and edited" (get-in (stored) [:content :description])))
  (is (not= "todo" (second (status-of))) "naming new_status is what moves a card"))
