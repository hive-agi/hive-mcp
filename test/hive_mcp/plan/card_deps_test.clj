(ns hive-mcp.plan.card-deps-test
  "plan-to-kanban writes each card's dependencies on the card itself, as
   `dep:<task-id>` tags, so the scheduler does not depend on the KG for them."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.plan.tool :as tool]
            [hive-mcp.scheduler.dag-waves :as dag]
            [hive-spi.kanban.registry]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- dep-tags [tags]
  (set (filter #(and (string? %) (.startsWith ^String % "dep:")) tags)))

(deftest cards-carry-their-dependencies-as-dep-tags
  (let [created (atom [])
        steps   [{:id "s1" :title "one" :depends-on []}
                 {:id "s2" :title "two" :depends-on ["s1"]}
                 {:id "s3" :title "three" :depends-on ["s1" "s2"]}]
        waves   {"s1" 0 "s2" 1 "s3" 2}]
    (with-redefs [hive-spi.kanban.registry/create-task!
                  (fn [req] (swap! created conj req)
                    {:ok {:id (str "card-" (count @created))}})]
      (let [res     ((deref #'tool/create-step-cards!) (reverse steps) waves "/tmp")
            tags-of (fn [title] (some #(when (= title (:title %)) (:tags %)) @created))]
        (is (= ["s3" "s2" "s1"] (mapv :step-id res)) "answers in step order")
        (is (= {"s1" "card-1" "s2" "card-2" "s3" "card-3"}
               (into {} (map (juxt :step-id :task-id)) res)))
        (is (= #{} (dep-tags (tags-of "one"))))
        (is (= #{"dep:card-1"} (dep-tags (tags-of "two"))))
        (is (= #{"dep:card-1" "dep:card-2"} (dep-tags (tags-of "three"))))
        (is (= #{"card-1"} (dag/card-dependencies {:tags (tags-of "two")})))))))
