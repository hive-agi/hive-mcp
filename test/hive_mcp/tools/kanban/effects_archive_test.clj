(ns hive-mcp.tools.kanban.effects-archive-test
  "Payload handed to :da/archive! carries the task lifecycle fields."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.tools.kanban.effects :as fx]))

(def ^:private entry
  {:id "20260919134209-7a26b0d9"
   :tags ["kanban" "done" "priority-high" "scope:project:hive" "wave:0"]
   :content {:task-type "kanban"
             :title "T"
             :description "what and why"
             :status "done"
             :priority "high"
             :created "2026-09-19T13:42:09-0300"
             :started "2026-09-19T14:00:00-0300"
             :context {:plan-step-id "step-2"}}})

(deftest payload-carries-lifecycle-fields
  (let [p (fx/archive-task-data entry "tid")]
    (is (= "tid" (:id p)))
    (is (= "T" (:title p)))
    (is (= "what and why" (:description p)))
    (is (= "high" (:priority p)))
    (is (= "2026-09-19T13:42:09-0300" (:created-at p)))
    (is (= "2026-09-19T14:00:00-0300" (:started-at p)))
    (is (= "hive" (:scope p)))
    (is (= ["kanban" "done" "priority-high" "wave:0"] (:tags p)))))

(deftest payload-tolerates-sparse-entries
  (testing "never-started task, no description, no tags"
    (let [p (fx/archive-task-data {:content {:title "only title" :started nil}} "tid")]
      (is (= "only title" (:title p)))
      (is (nil? (:started-at p)))
      (is (nil? (:description p)))
      (is (nil? (:scope p)))
      (is (= [] (:tags p)))))
  (testing "nil entry falls back to the id as title"
    (is (= "tid" (:title (fx/archive-task-data nil "tid")))))
  (testing "top-level priority/description are a fallback"
    (let [p (fx/archive-task-data {:priority "low" :description "top" :content {}} "tid")]
      (is (= "low" (:priority p)))
      (is (= "top" (:description p))))))

(defspec payload-is-total-and-never-leaks-scope-tags 100
  (prop/for-all [tags (gen/vector (gen/one-of [gen/string-alphanumeric
                                               (gen/fmap #(str "scope:project:" %) gen/string-alphanumeric)
                                               gen/keyword]))
                 content (gen/map (gen/elements [:title :description :priority :created :started])
                                  (gen/one-of [gen/string-alphanumeric (gen/return nil)]))]
    (let [p (fx/archive-task-data {:tags tags :content content} "tid")]
      (and (= "tid" (:id p))
           (some? (:title p))
           (not-any? #(and (string? %) (.startsWith ^String % "scope:")) (:tags p))))))
