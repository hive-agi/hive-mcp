(ns hive-mcp.events.handlers.task-contract-test
  "Contract facets for the task lifecycle handlers: generated events driven
   through the contracted handlers, goldens over the seeded cases, and the
   schema mutants of each handler's effects."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.handlers.task :as task]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Id [:string {:min 1 :max 10}])

(def ^:private NoCoeffects [:map {:closed true}])

(deftrifecta-from-schema task-complete-shouts-completion
  #'task/handle-task-complete
  {:in [:cat NoCoeffects
        [:tuple [:= :task/complete]
         [:map [:task-id Id] [:agent-id Id] [:result [:enum "success" "failure"]]]]]
   :out [:map
         [:log [:map [:level [:= :info]] [:message :string]]]
         [:shout [:map
                  [:agent-id Id]
                  [:event-type [:= :completed]]
                  [:data [:map [:task-id Id] [:result :string]]]]]]
   :rel (fn [[_ [_ {:keys [task-id agent-id result]}]] out]
          (and (= agent-id (get-in out [:shout :agent-id]))
               (= {:task-id task-id :result result} (get-in out [:shout :data]))
               (= (str "Task " task-id " completed by " agent-id)
                  (get-in out [:log :message]))))
   :golden-path "test/golden/hive-mcp/events/handlers/task-complete.edn"
   :num-tests 100})

(deftrifecta-from-schema task-shout-complete-titles-the-message
  #'task/handle-task-shout-complete
  {:in [:cat NoCoeffects
        [:tuple [:= :task/shout-complete]
         [:map [:task-id Id] [:title {:optional true} Id] [:agent-id Id] [:project Id]]]]
   :out [:map
         [:shout [:map
                  [:agent-id Id]
                  [:event-type [:= :completed]]
                  [:message [:string {:min 16}]]
                  [:data [:map [:task-id Id] [:title Id] [:agent-id Id] [:project Id]]]]]]
   :rel (fn [[_ [_ {:keys [task-id title agent-id project]}]] out]
          (let [t (or title task-id)]
            (and (= (str "Task completed: " t) (get-in out [:shout :message]))
                 (= {:task-id task-id :title t :agent-id agent-id :project project}
                    (get-in out [:shout :data])))))
   :golden-path "test/golden/hive-mcp/events/handlers/task-shout-complete.edn"
   :num-tests 100})

(def ^:private CommitTitle [:enum "fix login" "Fix crash" "FIX: typo" "add feature" "Refactor fixtures"])

(def ^:private CommitEffects
  [:map
   [:log [:map [:level [:= :info]] [:message :string]]]
   [:git-commit [:map [:files [:vector {:min 1} :string]] [:message :string] [:task-id Id]]]])

(defn- fix-title? [t] (str/starts-with? (str/lower-case t) "fix"))

(deftrifecta-from-schema git-commit-modified-commits-only-changed-files
  #'task/handle-git-commit-modified
  {:in [:cat NoCoeffects
        [:tuple [:= :git/commit-modified]
         [:map [:task-id Id] [:title {:optional true} CommitTitle]
          [:modified-files [:vector [:enum "src/a.clj" "src/b.clj" "test/c.clj"]]]]]]
   :out [:or CommitEffects [:map {:closed true}]]
   :rel (fn [[_ [_ {:keys [task-id title modified-files]}]] out]
          (if (empty? modified-files)
            (= {} out)
            (let [t (or title (str "Task " task-id))
                  msg (get-in out [:git-commit :message])]
              (and (= modified-files (get-in out [:git-commit :files]))
                   (= task-id (get-in out [:git-commit :task-id]))
                   (= msg (str (if (fix-title? t) "fix: " "feat: ") t))))))
   :golden-path "test/golden/hive-mcp/events/handlers/git-commit-modified.edn"
   :num-tests 100})

(deftest git-commit-modified-arms-are-disjoint
  (let [out-schema (-> (m/function-schemas) (get-in ['hive-mcp.events.handlers.task 'handle-git-commit-modified :schema]) m/children second)
        [commit-arm none-arm] (m/children out-schema)
        commit (task/handle-git-commit-modified {} [:git/commit-modified {:task-id "t" :title "fix x" :modified-files ["a"]}])
        none   (task/handle-git-commit-modified {} [:git/commit-modified {:task-id "t" :modified-files []}])]
    (is (m/validate commit-arm commit))
    (is (not (m/validate none-arm commit)))
    (is (m/validate none-arm none))
    (is (not (m/validate commit-arm none)))
    (is (= "fix: fix x" (get-in commit [:git-commit :message])))))

(deftest register-handlers-installs-the-task-events
  (ev/with-clean-registry
    (let [reg (task/register-handlers!)]
      (is (every? #(contains? reg %) [:task/complete :task/shout-complete :git/commit-modified]))
      (is (every? ev/handler-registered? [:task/complete :task/shout-complete :git/commit-modified])))))
