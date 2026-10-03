(ns hive-mcp.tools.kanban.list.session-todo-visibility-test
  "Session todos (cards tagged `session-todo`, mirrored by an agent's
   ITodoStore) are an agent's private step list, not backlog. The shared
   board hides them by default; a caller opts in by naming the tag or with
   `include_session_todos`.

   The board is injected through the `*board-source*` seam and the memory
   port stub; no concrete namespace is redefined."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.kanban.catchup-block :as catchup-block]
            [hive-mcp.tools.kanban.list.plan :as plan]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.tools.memory-kanban.query :as query]
            [hive-spi.memory.registry :as sreg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each stub/with-stub-store)

(def ^:private pid "session-todo-vis")

(defn- card [id status & extra-tags]
  {:id         id
   :type       "note"
   :project-id pid
   :tags       (into ["kanban" status "priority-medium" (str "scope:project:" pid)]
                     extra-tags)
   :content    {:task-type "kanban" :title (str "card " id)
                :status status :priority "medium"}
   :updated    (str "2026-09-07T10:00:0" (subs id (dec (count id))) "Z")})

(def ^:private board
  [(card "real-1" "todo")
   (card "real-2" "todo")
   (card "real-3" "done")
   (card "sess-4" "todo" "session-todo" "session-agent:default")
   (card "sess-5" "doing" "session-todo" "session-agent:default")])

(defn- ids [result]
  (set (map #(get % "id") (json/read-str (:text result)))))

(def ^:private base {:project_id pid :scope "all"})

;; =============================================================================
;; Pure: which tags a request hides
;; =============================================================================

(deftest hidden-tags-classification
  (testing "a plain request hides session todos"
    (is (= ["session-todo"] (plan/hidden-tags {})))
    (is (= ["session-todo"] (plan/hidden-tags {:status "todo" :tags ["epic:x"]}))))
  (testing "naming the tag opts in, AND or OR"
    (is (= [] (plan/hidden-tags {:tags ["session-todo"]})))
    (is (= [] (plan/hidden-tags {:tags ["x" "session-todo"] :tag_match "any"}))))
  (testing "the explicit flag opts in"
    (is (= [] (plan/hidden-tags {:include_session_todos true})))
    (is (= ["session-todo"] (plan/hidden-tags {:include_session_todos false})))))

(deftest drop-hidden-removes-only-tagged-entries
  (is (= ["real-1" "real-2" "real-3"]
         (mapv :id (plan/drop-hidden board ["session-todo"]))))
  (is (= 5 (count (plan/drop-hidden board [])))))

;; =============================================================================
;; list
;; =============================================================================

(deftest list-hides-session-todos-by-default
  (binding [query/*board-source* (src/->seq-source board)]
    (testing "a bare list and a status list carry no session todo"
      (is (= #{"real-1" "real-2" "real-3"} (ids (query/list-slim* base))))
      (is (= #{"real-1" "real-2"} (ids (query/list-slim* (assoc base :status "todo"))))))
    (testing "the owning agent still reads its own list by tag"
      (is (= #{"sess-4" "sess-5"}
             (ids (query/list-slim* (assoc base :tags ["session-todo" "session-agent:default"]
                                           :tag_match "all"))))))
    (testing "include_session_todos shows the whole board"
      (is (= #{"real-1" "real-2" "real-3" "sess-4" "sess-5"}
             (ids (query/list-slim* (assoc base :include_session_todos true))))))))

;; =============================================================================
;; stats + catchup counts
;; =============================================================================

(deftest stats-exclude-session-todos
  (stub/seed! (sreg/get-store) board)
  (let [stats (json/read-str (:text (query/stats* {:scope "all"})) :key-fn keyword)]
    (is (= 2 (:todo stats)))
    (is (= 0 (:doing stats)))
    (is (= 1 (:done stats))))
  (testing "opt-in counts them"
    (let [stats (json/read-str (:text (query/stats* {:scope "all" :include_session_todos true}))
                               :key-fn keyword)]
      (is (= 3 (:todo stats)))
      (is (= 1 (:doing stats))))))

(deftest catchup-summary-excludes-session-todos
  (stub/seed! (sreg/get-store) board)
  (let [{:keys [counts recent-todos]} (catchup-block/gather-kanban-summary pid)]
    (is (= {:todo 2 :inprogress 0 :inreview 0 :done 1} counts))
    (is (= #{"real-1" "real-2"} (set (map :id recent-todos))))))
