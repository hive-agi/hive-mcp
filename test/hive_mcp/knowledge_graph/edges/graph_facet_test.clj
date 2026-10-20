(ns hive-mcp.knowledge-graph.edges.graph-facet-test
  "Goldens over scope migration, node reads and co-access promotion against
   a fresh store."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.migration :as mig]
            [hive-mcp.knowledge-graph.edges.nodes :as nodes]
            [hive-mcp.knowledge-graph.edges.promotion :as promo]
            [hive-mcp.knowledge-graph.edges.queries :as q]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- refusal [f]
  (try (f) :no-throw (catch clojure.lang.ExceptionInfo e (ex-message e))))

(golden/deftest-golden migration-golden
  "test/golden/hive-mcp/m14-edges-migration.edn"
  (sup/in-fresh-store
   (fn []
     (sup/seed! write/add-edge!)
     {:moved (mig/migrate-edge-scopes! "s1" "s9")
      :none (mig/migrate-edge-scopes! "absent" "s9")
      :scoped-now (sup/shown-all (q/get-edges-by-scope "s9"))
      :nil-scope (refusal #(mig/migrate-edge-scopes! nil "s9"))
      :same-scope (refusal #(mig/migrate-edge-scopes! "s9" "s9"))})))

(golden/deftest-golden nodes-golden
  "test/golden/hive-mcp/m14-edges-nodes.edn"
  (sup/in-fresh-store
   (fn []
     (let [empty-ids (nodes/get-all-node-ids)]
       (sup/seed! write/add-edge!)
       {:empty empty-ids
        :all (vec (sort (nodes/get-all-node-ids)))
        :scoped (vec (sort (nodes/get-all-node-ids "s2")))
        :nil-scope (count (nodes/get-all-node-ids nil))
        :both (vec (sort (nodes/neighbors "n2")))
        :out (nodes/neighbors "n1" :out)
        :in (nodes/neighbors "n3" :in)
        :nil-dir (vec (sort (nodes/neighbors "n2" nil)))
        :self-loop (vec (sort (nodes/neighbors "n3")))
        :bad-dir (refusal #(nodes/neighbors "n1" :sideways))}))))

(golden/deftest-golden promotion-golden
  "test/golden/hive-mcp/m14-edges-promotion.edn"
  (sup/in-fresh-store
   (fn []
     (let [first-run (promo/record-co-access! ["p1" "p2" "p3" "p2"] {:scope "s" :created-by "m14"})
           again (promo/record-co-access! ["p1" "p2"])
           single (promo/record-co-access! ["p1"])
           none (promo/record-co-access! nil)]
       {:first first-run :again again :single single :none none
        :co (promo/get-co-accessed "p2")
        :co-missing (promo/get-co-accessed "zz")
        :promote-default (promo/promote-co-access-edges!)
        :promote-low (promo/promote-co-access-edges! {:threshold 0.35 :confidence 0.4 :created-by "m14"})
        :promote-again (promo/promote-co-access-edges! {:threshold 0.35})
        :depends-on (sup/shown-all (q/get-edges-by-relation :depends-on))}))))

(deftest record-co-access-caps-pairs-at-fifty
  (sup/in-fresh-store
   (fn []
     (is (= 50 (promo/record-co-access! (mapv #(str "cap-" %) (range 12))))))))
