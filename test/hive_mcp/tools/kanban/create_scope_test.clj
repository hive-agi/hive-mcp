(ns hive-mcp.tools.kanban.create-scope-test
  "Friction 20261023210335-286ec3d5: multi DSL `b+` with project_id=\"desargues\"
   created the cards in the session cwd project and answered success."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.dsl.verbs :as verbs]
            [hive-mcp.tools.kanban.create-scope :as create-scope]
            [hive-mcp.tools.memory-kanban :as mem-kanban]
            [hive-mcp.tools.memory.crud :as mem-crud]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.vectordb.kanban-facade :as kanban-facade]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- run-create
  "The create handler with the cwd project stubbed to \"cwd-project\" and the
   write recorded. Answers [result calls]."
  [params]
  (let [calls (atom [])]
    (with-redefs [kanban-facade/query-entries (fn [& _] [])
                  scope/get-current-project-id (fn [& _] "cwd-project")
                  mem-crud/handle-add (fn [args]
                                        (swap! calls conj args)
                                        {:type "text" :text "stub-id"})]
      [(mem-kanban/handle-mem-kanban-create params) @calls])))

(defn- scope-tags [call]
  (filterv #(re-find #"^scope:project:" %) (:tags call)))

(deftest explicit-project-id-is-pure
  (is (nil? (create-scope/explicit-project-id {})))
  (is (nil? (create-scope/explicit-project-id nil)))
  (is (= "desargues" (create-scope/explicit-project-id {:project_id "desargues"})))
  (is (= "desargues" (create-scope/explicit-project-id {:project-id "desargues"})))
  (is (= "desargues" (create-scope/explicit-project-id {:tags ["x" "scope:project:desargues"]})))
  (is (= "desargues" (create-scope/explicit-project-id {:project_id "desargues"
                                                        :tags ["scope:project:desargues"]})))
  (testing "refused loudly"
    (is (thrown? clojure.lang.ExceptionInfo (create-scope/explicit-project-id {:project_id ""})))
    (is (thrown? clojure.lang.ExceptionInfo (create-scope/explicit-project-id {:project_id 7})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (create-scope/explicit-project-id {:project_id "a" :tags ["scope:project:b"]})))))

(deftest create-honours-project-id
  (let [[result [call]] (run-create {:title "card" :project_id "desargues"})]
    (is (= "stub-id" (:text result)))
    (is (= ["scope:project:desargues"] (scope-tags call)) "exactly one scope tag, the named one")))

(deftest create-honours-a-scope-tag
  (let [[_ [call]] (run-create {:title "card" :tags ["scope:project:desargues" "epic:x"]})]
    (is (= ["scope:project:desargues"] (scope-tags call)))
    (is (some #{"epic:x"} (:tags call)))))

(deftest create-without-a-project-keeps-the-cwd-project
  (let [[_ [call]] (run-create {:title "card"})]
    (is (= ["scope:project:cwd-project"] (scope-tags call)))))

(deftest conflicting-project-is-an-error-not-a-write
  (let [[result calls] (run-create {:title "card" :project_id "a" :tags ["scope:project:b"]})]
    (is (:isError result))
    (is (re-find #"disagrees" (str (:text result))))
    (is (empty? calls))))

(deftest b+-forwards-project-id
  (is (= "desargues" (:project_id (verbs/parse-sentence ["b+" {"title" "card" "project_id" "desargues"}])))))
