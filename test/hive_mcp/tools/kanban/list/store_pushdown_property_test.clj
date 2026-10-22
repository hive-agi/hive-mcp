(ns hive-mcp.tools.kanban.list.store-pushdown-property-test
  "The store-pushed list path answers exactly what the old path answered.

   Old path: ONE scoped-board fetch on the required tags (kanban, status,
   AND-tags), every narrowing filter (query, priority, dates, OR-tags) run
   in memory by `plan/shape`.
   New path: `list-slim-data`, which fans OR-tags out into one store query
   per tag (`plan/store-queries`) and merges the answers by id.

   Both run over generated boards, through the in-memory source and through
   the facade over the stub IMemoryStore, and must agree row for row."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.protocols.memory :as proto]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.kanban.list.plan :as plan]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.tools.memory-kanban.query :as query]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each stub/with-stub-store)

(def ^:private pid "pushdown-p")

(def ^:private extra-tags ["epic:a" "epic:b" "wave:1" "session-todo" "x"])

(def ^:private words ["auth" "store" "Kanban" "filter" "window" ""])

(def ^:private stamps
  ["2026-07-18T19:42:39-0300" "2026-08-21T16:55:25-03:00" "2026-09-07T10:00:00Z"
   "2026-10-01T00:00:00Z" nil])

(def ^:private gen-card
  (gen/let [n        (gen/choose 0 99999)
            status   (gen/elements ["todo" "doing" "review" "done"])
            prio     (gen/elements ["high" "medium" "low"])
            extras   (gen/vector-distinct (gen/elements extra-tags) {:max-elements 3})
            title    (gen/elements words)
            desc     (gen/elements words)
            created  (gen/elements stamps)
            updated  (gen/elements stamps)
            other?   (gen/frequency [[5 (gen/return false)] [1 (gen/return true)]])]
    (let [p (if other? "pushdown-q" pid)]
      (cond-> {:id         (format "20260907%06d-%08x" n n)
               :type       "note"
               :project-id p
               :tags       (into ["kanban" status (str "priority-" prio) (str "scope:project:" p)]
                                 extras)
               :content    (cond-> {:task-type "kanban" :title (str title " " n)
                                    :status status :priority prio :description desc}
                             created (assoc :created created))}
        updated (assoc :updated updated)))))

(def ^:private gen-board
  (gen/fmap #(vec (vals (into {} (map (juxt :id identity)) %)))
            (gen/vector gen-card 0 40)))

(def ^:private gen-request
  (gen/let [status    (gen/elements [nil "todo" "inprogress" "inreview" "done"])
            tags      (gen/elements [nil [] ["epic:a"] ["epic:a" "epic:b"] ["wave:1" "x" "epic:a"]
                                     ["session-todo"] ["nope"]])
            tag-match (gen/elements [nil "all" "any"])
            q         (gen/elements [nil "" "auth" "KANBAN" "zz"])
            priority  (gen/elements [nil "high" "low"])
            created   (gen/elements [nil "2026-08-01T00:00:00Z"])
            updated   (gen/elements [nil "2026-09-01T00:00:00-0300"])
            limit     (gen/elements [nil 3 50])
            offset    (gen/elements [nil 0 2])
            fields    (gen/elements [nil ["id"] ["id" "priority"]])
            scope     (gen/elements [nil "all"])]
    (cond-> {:project_id pid}
      status    (assoc :status status)
      tags      (assoc :tags tags)
      tag-match (assoc :tag_match tag-match)
      q         (assoc :query q)
      priority  (assoc :priority priority)
      created   (assoc :created_after created)
      updated   (assoc :updated_after updated)
      limit     (assoc :limit limit)
      offset    (assoc :offset offset)
      fields    (assoc :fields fields)
      scope     (assoc :scope scope))))

(defn- old-path
  "The pre-pushdown list: one fetch on the required tags, everything else
   filtered in memory."
  [source {:keys [include_ancestors scope] :as params}]
  (let [fetch-plan (plan/plan params)
        {:keys [entries multi-project?]}
        (src/scoped-board source
                          {:project-id           pid
                           :include-descendants? true
                           :include-ancestors?   (not (false? include_ancestors))
                           :scope                scope}
                          fetch-plan)]
    (plan/shape fetch-plan
                (plan/drop-hidden entries (plan/hidden-tags params))
                multi-project?)))

(defn- new-path [source params]
  (binding [query/*board-source* source]
    (query/list-slim-data params)))

(def ^:private num-tests 120)

(deftest seq-source-pushdown-equals-in-memory-filter
  (let [r (tc/quick-check
           num-tests
           (prop/for-all [board gen-board
                          req   gen-request]
             (let [source (src/->seq-source board)]
               (= (old-path source req) (new-path source req)))))]
    (is (:pass? r) (pr-str (select-keys r [:shrunk :seed])))))

(deftest facade-source-pushdown-equals-in-memory-filter
  (let [r (tc/quick-check
           num-tests
           (prop/for-all [board gen-board
                          req   gen-request]
             (proto/reset-store! (proto/get-store))
             (stub/seed! (proto/get-store) board)
             (let [source (query/->FacadeBoardSource)]
               (= (old-path source req) (new-path source req)))))]
    (is (:pass? r) (pr-str (select-keys r [:shrunk :seed])))))

(deftest or-tags-fan-out-one-query-per-tag
  (testing "OR-tags become one store query each, carrying the required tags"
    (is (= [["kanban" "todo" "a"] ["kanban" "todo" "b"]]
           (plan/store-queries (plan/plan {:status "todo" :tags ["a" "b" "a"] :tag_match "any"})))))
  (testing "AND-tags and bare requests stay one query"
    (is (= [["kanban" "a"]] (plan/store-queries (plan/plan {:tags ["a"]}))))
    (is (= [["kanban"]] (plan/store-queries (plan/plan {:query "x" :priority "high"}))))))

(deftest the-store-is-asked-only-for-or-tagged-rows
  (stub/seed! (proto/get-store)
              [{:id "1" :type "note" :project-id pid
                :tags ["kanban" "todo" "a" (str "scope:project:" pid)]
                :content {:task-type "kanban" :title "one" :status "todo" :priority "medium"}}
               {:id "2" :type "note" :project-id pid
                :tags ["kanban" "todo" (str "scope:project:" pid)]
                :content {:task-type "kanban" :title "two" :status "todo" :priority "medium"}}])
  (let [seen   (atom [])
        source (reify src/IBoardSource
                 (scoped-board [_ spec fp]
                   (let [board (src/scoped-board (query/->FacadeBoardSource) spec fp)]
                     (swap! seen conj (count (:entries board)))
                     board)))]
    (is (= ["1"] (map :id (new-path source {:project_id pid :tags ["a"] :tag_match "any"}))))
    (is (= [1] @seen) "the untagged card never left the store")))
