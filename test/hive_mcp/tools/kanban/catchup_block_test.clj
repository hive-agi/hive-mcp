(ns hive-mcp.tools.kanban.catchup-block-test
  "The kanban catchup block: bucket counts and recent todos for a project,
   contributed through hive-spi.catchup.registry so catchup composes it
   without naming the kanban domain."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-spi.catchup.registry :as blocks]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.kanban.catchup-block :as sut]
            [hive-mcp.tools.kanban.schema :as ks]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [hive-spi.memory.registry :as sreg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each mem-stub/with-stub-store)

(def ^:private pid "kanban-block-test")

(defn- entry [id title status]
  {:id         id
   :type       "note"
   :content    {:task-type "kanban" :title title :status status :priority "medium"}
   :tags       ["kanban" status "priority-medium" (str "scope:project:" pid)]
   :project-id pid
   :updated    (str "2026-09-07T10:00:0" (subs id (dec (count id))) "Z")})

(defn- seed-board! []
  (mem-stub/seed! (sreg/get-store)
                  [(entry "cb-1" "One" "todo")
                   (entry "cb-2" "Two" "todo")
                   (entry "cb-3" "Three" "doing")
                   (entry "cb-4" "Four" "done")]))

(deftest summary-counts-buckets-and-lists-recent-todos
  (seed-board!)
  (let [{:keys [counts recent-todos scope-tag]} (sut/gather-kanban-summary pid)]
    (is (= {:todo 2 :inprogress 1 :inreview 0 :done 1} counts))
    (is (= #{"cb-1" "cb-2"} (set (map :id recent-todos))))
    (is (= #{"One" "Two"} (set (map :title recent-todos))))
    (is (= (str "scope:project:" pid) scope-tag))))

(deftest summary-degrades-to-the-empty-shape-without-a-store
  (sreg/reset-registry!)
  (let [summary (sut/gather-kanban-summary pid)]
    (is (= {} (:counts summary)))
    (is (= [] (:recent-todos summary)))))

(deftest the-block-composes-under-its-id
  (seed-board!)
  (try
    (sut/register!)
    (testing "catchup reaches the summary through the registry, keyed :kanban"
      (let [{:keys [blocks failed]} (blocks/compose {:project-id pid})]
        (is (= {} failed))
        (is (= 2 (get-in blocks [:kanban :counts :todo])))))
    (finally
      (blocks/unregister-block! :kanban))))

;; -----------------------------------------------------------------------------
;; Facets
;; -----------------------------------------------------------------------------

(def ^:private Summary
  "Summaries over empty, zero-count and non-empty boards, with and without
   recent todos, so both arms of catchup-section are reached."
  [:map
   [:counts [:map-of [:enum :todo :inprogress :done] [:int {:min 0 :max 3}]]]
   [:recent-todos [:vector {:max 2} [:map [:id [:enum "t1" "t2"]] [:title [:enum "One"]] [:tags [:= ["kanban"]]]]]]
   [:scope-tag [:maybe [:enum "scope:project:p"]]]])

(deftrifecta-from-schema catchup-section-is-nil-for-an-empty-board
  hive-mcp.tools.kanban.catchup-block/catchup-section
  {:in          Summary
   :out         ks/Section
   :rel         (fn [in out]
                  (if (or (pos? (apply + (vals (:counts in)))) (seq (:recent-todos in)))
                    (= out (assoc in :hint (:hint out)))
                    (nil? out)))
   :num-tests   300
   :n-cases     10
   :golden-path "test/golden/tools/kanban/catchup-section.edn"})

(deftest-golden gather-kanban-summary-over-a-stub-board
  "test/golden/tools/kanban/catchup-gather.edn"
  (do (seed-board!)
      {:scoped   (update (sut/gather-kanban-summary pid) :recent-todos #(sort-by :id %))
       :other    (sut/gather-kanban-summary "no-such-project")
       :no-store (do (sreg/reset-registry!) (sut/gather-kanban-summary pid))}))
