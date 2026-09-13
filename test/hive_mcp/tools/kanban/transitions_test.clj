(ns hive-mcp.tools.kanban.transitions-test
  "Unit + golden tests for pure kanban transition derivation."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.kanban.predicates :as kp]
            [hive-mcp.tools.kanban.transitions :as kt]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private fixture-entry
  {:id    "20260101000000-deadbeef"
   :tags  ["kanban" "review" "priority-high" "scope:project:hive-mcp"]
   :content {:task-type "kanban"
             :title    "weave-W1A: presets/core.clj"
             :status   "review"
             :priority "high"
             :created  "2026-04-04T21:34:28-0300"
             :started  "2026-04-27T00:52:15-0300"
             :context  nil}})

(deftest predicates
  (testing "valid statuses"
    (is (kp/valid-status? "todo"))
    (is (kp/valid-status? "doing"))
    (is (kp/valid-status? "review"))
    (is (kp/valid-status? "done"))
    (is (not (kp/valid-status? "wip")))
    (is (not (kp/valid-status? nil))))
  (testing "MCP enum normalization"
    (is (= "doing"  (kp/normalize-status "inprogress")))
    (is (= "review" (kp/normalize-status "inreview")))
    (is (= "todo"   (kp/normalize-status "todo")))
    (is (= "done"   (kp/normalize-status "done"))))
  (testing "kanban-entry? recognises content marker"
    (is (kp/kanban-entry? fixture-entry))
    (is (not (kp/kanban-entry? {:content {:task-type "memo"}})))))

(deftest content-val-tolerates-string-keys
  (is (= "review" (kt/content-val (:content fixture-entry) :status nil)))
  (is (= "review" (kt/content-val {"status" "review"} :status nil)))
  (is (= ::default (kt/content-val {} :status ::default))))

(deftest extract-project-id-from-tags
  (is (= "hive-mcp" (kt/extract-project-id-from-tags fixture-entry)))
  (is (nil? (kt/extract-project-id-from-tags {:tags ["kanban" "todo"]}))))

(deftest task->slim-shape
  (let [s (kt/task->slim fixture-entry)]
    (is (= #{:id :title :status :priority} (set (keys s))))
    (is (= "review" (:status s))))
  (testing "multi-project flag adds :project"
    (is (= "hive-mcp"
           (:project (kt/task->slim fixture-entry true))))))

(def ^:private routed-context
  {:plan-step-id "s1"
   :execution {:provider "ollama-compat" :model "qwen2.5:3b-instruct"
               :spawn-mode "hive-agent" :presets ["ling"]
               :persona {:caps {:decisions 2}}}})

(deftest entry-content-decodes-what-a-store-may-return
  (is (= (:content fixture-entry) (kt/entry-content fixture-entry)))
  (is (= {:task-type "kanban" :title "t"}
         (kt/entry-content {:content "{\"task-type\":\"kanban\",\"title\":\"t\"}"})))
  (is (nil? (kt/entry-content {:content "not json"})))
  (is (nil? (kt/entry-content {:content "[1,2]"})))
  (is (nil? (kt/entry-content {}))))

(deftest task->detail-keeps-description-and-context
  (let [entry (update fixture-entry :content assoc
                      :description "body" :context routed-context)
        detail (kt/task->detail entry)]
    (testing "the slim keys of the detail are the slim shape"
      (is (= (kt/task->slim entry) (select-keys detail (keys (kt/task->slim entry))))))
    (is (= "body" (:description detail)))
    (is (= routed-context (:context detail)))
    (testing "multi-project adds :project as slim does"
      (is (= "hive-mcp" (:project (kt/task->detail entry true))))))
  (testing "JSON-string content is decoded"
    (let [entry {:id "x" :content (json/write-str {:task-type "kanban" :title "t" :status "todo"
                                                   :priority "low" :description "body"
                                                   :context routed-context})}]
      (is (= {:id "x" :title "t" :status "todo" :priority "low"
              :description "body" :context routed-context}
             (kt/task->detail entry)))))
  (testing "string-keyed context comes back keyword-keyed"
    (let [entry {:id "x" :content {"title" "t" "context" (json/read-str (json/write-str routed-context))}}]
      (is (= routed-context (:context (kt/task->detail entry))))))
  (testing "absent or non-map context and absent description add nothing"
    (is (= (kt/task->slim fixture-entry) (kt/task->detail fixture-entry)))
    (is (not (contains? (kt/task->detail (assoc-in fixture-entry [:content :context] "x")) :context)))))

(deftest compute-new-content-stamps
  (testing "doing stamps :started"
    (let [c (kt/compute-new-content (:content fixture-entry) "doing")]
      (is (= "doing" (:status c)))
      (is (string? (:started c)))))
  (testing "done stamps :completed and KEEPS the entry payload"
    (let [c (kt/compute-new-content (:content fixture-entry) "done")]
      (is (= "done" (:status c)))
      (is (string? (:completed c)))
      (is (= "weave-W1A: presets/core.clj" (:title c)))))
  (testing "review/todo only update :status"
    (is (= "todo" (:status (kt/compute-new-content (:content fixture-entry) "todo"))))
    (is (nil? (:completed (kt/compute-new-content (:content fixture-entry) "todo"))))))

(deftest compute-new-tags-shape
  (let [tags (kt/compute-new-tags ["kanban" "todo" "priority-low" "scope:project:hive"
                                   "topical" "agent:x"]
                                  "done" "high" "hive-mcp")]
    (is (vector? tags))
    (is (every? string? tags))
    (is (some #{"done"} tags))
    (is (not-any? #{"todo"} tags))
    (is (some #{"priority-high"} tags))
    (is (not-any? #{"priority-low"} tags))
    (is (some #{"topical"} tags))
    (is (some #{"agent:x"} tags))
    (is (some #(.startsWith ^String % "scope:project:") tags))))

(deftest transition-golden
  (testing "review -> done preserves identity, retags, stamps"
    (let [{:keys [old-status new-status new-content new-tags title project-id]}
          (kt/transition fixture-entry "done" "hive-mcp")]
      (is (= "review" old-status))
      (is (= "done"   new-status))
      (is (= "weave-W1A: presets/core.clj" title))
      (is (= "hive-mcp" project-id))
      (is (= "done" (:status new-content)))
      (is (string? (:completed new-content)))
      (is (some #{"done"} new-tags))
      (is (not (some #{"review"} new-tags))))))

(deftest sort-by-priority-then-created
  (testing "operates on slim view (priority is a top-level key)"
    (let [a {:id "1" :priority "low"}
          b {:id "2" :priority "high"}
          c {:id "3" :priority "medium"}]
      (is (= ["2" "3" "1"]
             (mapv :id (kt/sort-by-priority-then-created [a b c]))))))
  (testing "ties on priority sort by id"
    (let [a {:id "20260105" :priority "high"}
          b {:id "20260101" :priority "high"}]
      (is (= ["20260101" "20260105"]
             (mapv :id (kt/sort-by-priority-then-created [a b])))))))
