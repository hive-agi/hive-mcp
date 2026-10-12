(ns hive-mcp.tools.catchup.caps-trifecta-test
  "Contracts for addon-provided budgets and per-bucket trimming."
  (:require [clojure.test :refer [deftest is]]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.catchup.caps :as caps]
            [hive-mcp.tools.catchup.bundle :as bundle]
            [clojure.test.check.generators :as gen]))

(defn- budget-probe [scenario]
  (let [provider (case scenario
                   :absent nil
                   :throws (fn [_] (throw (ex-info "unavailable" {})))
                   :project (fn [project-id]
                              (if (= project-id "focused")
                                {:axioms 2 :axiom-candidates 1 :decisions 0}
                                {:axioms 4}))
                   :invalid (fn [_] {:axioms -1 :axiom-candidates 1001
                                     :decisions "many"}))]
    (select-keys (caps/resolve-caps provider "focused" nil)
                 [:axioms :axiom-candidates :decisions])))

(deftrifecta addon-budget-fallback
  hive-mcp.tools.catchup.caps-trifecta-test/budget-probe
  {:golden-path "test/golden/catchup/caps-provider.edn"
   :cases {:absent :absent :throws :throws :project :project :invalid :invalid}
   :gen (gen/elements [:absent :throws :project :invalid])
   :pred (fn [result] (every? #(and (integer? %) (<= 0 % 1000)) (vals result)))
   :num-tests 30
   :mutations [["discard-provider" (fn [_] {:axioms 100 :axiom-candidates 25 :decisions 50})]]})

(defn- bucket-probe [caps-map]
  (let [entries (fn [type] (mapv (fn [i] {:id i :type type}) (range 4)))
        buckets (#'bundle/split-by-type
                 {"axiom" (entries "axiom")
                  "axiom-candidate" (entries "axiom-candidate")
                  "decision" (entries "decision")}
                 [] caps-map)]
    (select-keys (update-vals buckets count)
                 [:axioms :axiom-candidates :decisions])))

(deftrifecta all-buckets-use-caps
  hive-mcp.tools.catchup.caps-trifecta-test/bucket-probe
  {:golden-path "test/golden/catchup/caps-buckets.edn"
   :cases {:default {} :override {:axioms 2 :axiom-candidates 1 :decisions 0}
           :invalid {:axioms -1 :axiom-candidates 1001 :decisions "many"}}
   :gen (gen/elements
         [{} {:axioms 2 :axiom-candidates 1 :decisions 0}])
   :pred (fn [result] (every? #(<= 0 % 4) (vals result)))
   :num-tests 30
   :mutations [["ignore-caps" (fn [_] {:axioms 4 :axiom-candidates 4 :decisions 4})]]})

(deftest caps-resolution-is-keyed-by-project-and-profile
  (let [provider (fn [project-id] {:axioms (if (= "focused" project-id) 2 4)})]
    (is (= 2 (:axioms (caps/resolve-caps provider "focused" nil))))
    (is (= 4 (:axioms (caps/resolve-caps provider "other" nil))))
    (is (= 3 (:axioms (caps/resolve-caps provider "focused" {:caps {:axioms 3}}))))
    (is (= 2 (:axioms (caps/resolve-caps provider "focused" {:caps {:axioms -1}}))))))
