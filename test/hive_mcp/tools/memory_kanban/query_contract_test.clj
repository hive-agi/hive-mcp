(ns hive-mcp.tools.memory-kanban.query-contract-test
  "Contract facets for the pure parts of the kanban query: the required-tag
   filter and the global / nil short-circuits of the visible-id resolution."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory-kanban.query :as q]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Tag [:enum "kanban" "todo" "done" "priority-high"])

(deftrifecta-from-schema filter-kanban-by-tags-keeps-entries-with-every-tag
  #'q/filter-kanban-by-tags
  {:in [:cat [:vector {:max 5} [:map [:id [:enum "a" "b" "c"]]
                                [:tags [:vector {:max 4} Tag]]
                                [:content [:map [:task-type [:enum "kanban" "note"]]]]]]
        [:vector {:min 1 :max 2} Tag]]
   :out [:sequential :map]
   :rel (fn [[entries required] out]
          (= (vec out) (filterv #(and (every? (set (:tags %)) required)
                                      (= "kanban" (get-in % [:content :task-type])))
                                entries)))
   :num-tests 200})

(deftrifecta-from-schema global-and-nil-see-no-project-filter
  #'q/resolve-visible-project-ids
  {:in [:cat [:enum nil "global"] :boolean]
   :out :nil
   :num-tests 20})

(golden/deftest-golden kanban-query-pure-golden
  "test/golden/hive-mcp/tools/memory_kanban/query-pure.edn"
  (sorted-map
   :filter       (vec (q/filter-kanban-by-tags
                       [{:id "a" :tags ["kanban" "todo"] :content {:task-type "kanban"}}
                        {:id "b" :tags ["kanban"] :content {:task-type "kanban"}}
                        {:id "c" :tags ["kanban" "todo"] :content {:task-type "note"}}]
                       ["kanban" "todo"]))
   :visible-up   (q/visible-project-ids {:project-id "a" :chain ["a" "root" "global"]})
   :visible-self (q/visible-project-ids {:project-id "a" :chain ["a" "global"]
                                         :include-ancestors? false
                                         :descendants #{"a-1"} :include-descendants? true})
   :visible-glob (q/visible-project-ids {:project-id "global"})))

(deftest descendants-of-global-are-nil
  (is (nil? (q/resolve-project-ids-with-descendants "global"))))
