(ns hive-mcp.knowledge-graph.edges.promotion-test
  "Co-access recording and co-access -> :depends-on promotion inside a fresh
   thread-local DataScript store (the KG store port injected by the harness).
   The golden walks record (create + reinforce), the co-access neighbour
   ranking and three promotion cycles (default, idempotent re-run, lowered
   threshold), ending with the :depends-on edges they wrote."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.promotion :as promotion]
            [hive-mcp.knowledge-graph.edges.queries :as queries]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-golden co-access-promotion-golden
  "test/golden/hive-mcp/knowledge-graph/edges-promotion-cycle.edn"
  (sup/in-fresh-store
   (fn []
     (let [first-batch (promotion/record-co-access! ["a" "b" "c" "a"] {:scope "s" :created-by "t"})
           too-few     (promotion/record-co-access! ["a" "a"])
           reinforced  (vec (repeatedly 4 #(promotion/record-co-access! ["a" "b"])))
           co-a        (promotion/get-co-accessed "a")
           ;; equal confidences: order among ties is index order, so pin the set
           co-c        (set (promotion/get-co-accessed "c"))
           co-none     (promotion/get-co-accessed "nobody")
           default     (promotion/promote-co-access-edges!)
           rerun       (promotion/promote-co-access-edges!)
           lowered     (promotion/promote-co-access-edges! {:threshold 0.2 :confidence 0.4 :limit 5})]
       {:first-batch first-batch :too-few too-few :reinforced reinforced
        :co-a co-a :co-c co-c :co-none co-none
        :default default :rerun rerun :lowered lowered
        :co-accessed (sup/shown-all (queries/get-edges-by-relation :co-accessed nil))
        :depends-on  (sup/shown-all (queries/get-edges-by-relation :depends-on nil))}))))

(deftest promotion-defaults-are-documented
  (is (= [0.7 0.5 20] [promotion/default-promotion-threshold
                       promotion/default-promoted-confidence
                       promotion/default-promotion-limit])))
