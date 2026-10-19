(ns hive-mcp.knowledge-graph.disc.propagation-facet-test
  "Goldens over time decay and staleness propagation, against a fresh
   DataScript KG store and a fresh stub memory store, both restored after."
  (:require [hive-mcp.knowledge-graph.disc.crud :as crud]
            [hive-mcp.knowledge-graph.disc.propagation :as prop]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-test.golden :as golden]
            [hive-mcp.protocols.memory :as mem]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- decay-runs []
  (fixtures/datascript-fixture
   (fn []
     (let [empty-run (prop/apply-time-decay-to-all-discs!)]
       (crud/add-disc! {:path "/m14/prop/a.clj" :project-id "p"})
       (crud/add-disc! {:path "/m14/prop/b.log"})
       {:empty empty-run
        :all (prop/apply-time-decay-to-all-discs!)
        :scoped (prop/apply-time-decay-to-all-discs! :project-id "p")
        :scoped-map (prop/apply-time-decay-to-all-discs! {:project-id "nobody"})}))))

(golden/deftest-golden time-decay-golden
  "test/golden/hive-mcp/m14-disc-time-decay.edn"
  (decay-runs))

(defn- propagation-runs []
  (fixtures/datascript-fixture
   (fn []
     (stub/with-stub-store
      (fn []
        (let [store (mem/get-store)]
          (stub/seed! store [{:id "m14-e1" :type "note" :content "x" :grounded-from "/m14/f.clj"}
                             {:id "m14-e2" :type "note" :content "y" :grounded-from "/m14/f.clj"}])
          {:hash-mismatch (prop/propagate-staleness! "/m14/f.clj" 5.0 :hash-mismatch)
           :below-threshold (prop/propagate-staleness! "/m14/f.clj" 0.1 :time-decay)
           :unknown-reason (prop/propagate-staleness! "/m14/f.clj" nil :nope)
           :betas (into (sorted-map)
                        (map (fn [[id e]] [id (select-keys e [:staleness-beta :staleness-depth :staleness-source])]))
                        (stub/entries store))}))))))

(golden/deftest-golden propagation-golden
  "test/golden/hive-mcp/m14-disc-propagation.edn"
  (propagation-runs))
