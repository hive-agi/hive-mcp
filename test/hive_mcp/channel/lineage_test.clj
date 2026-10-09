(ns hive-mcp.channel.lineage-test
  "A ling's parentless telemetry rows are routed by the registry's parent, so
   they stop reaching every coordinator that shares the project tree
   (HIVEMIND-PIGGYBACK-LEAK, kanban 20261008232949-5b0f7835)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-schemas.test :as hst]
            [hive-test.properties :refer [defprop-metamorphic]]
            [hive-mcp.channel.audience :as aud]
            [hive-mcp.channel.lineage :as lineage]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private registry
  "Literal agent -> parent table; the law reads this, not the subject."
  {"csr-scan" "coordinator:31311" "hm-leak" "coordinator:246159" "ghost" ""})

(def ^:private Row
  [:map
   [:agent-id [:maybe [:enum "csr-scan" "hm-leak" "ghost" "unknown" "coordinator:7"]]]
   [:parent-id {:optional true} [:enum "coordinator:9" "coordinator:31311" "coordinator:246159"]]
   [:project-id [:enum "hive-carto" "global"]]])

(def ^:private Input [:map [:msgs [:vector {:max 6} Row]]])

(defn attach-case
  "One-arg subject over the literal registry."
  [{:keys [msgs]}]
  (lineage/attach-parents registry msgs))

(defn- attach-law [{:keys [msgs]} out]
  (and (= (count msgs) (count out))
       (every? true?
               (map (fn [in o]
                      (= o (let [p (get registry (:agent-id in))]
                             (if (and (nil? (:parent-id in)) (:agent-id in) (seq p))
                               (assoc in :parent-id p)
                               in))))
                    msgs out))))

(hst/deftrifecta-from-schema attach-parents-contract
  hive-mcp.channel.lineage-test/attach-case
  {:in Input
   :out [:vector Row]
   :rel attach-law
   :num-tests 100
   :seed 0
   :n-cases 8})

(defprop-metamorphic attaching-twice-changes-nothing
  attach-case
  (fn [x] (update x :msgs #(lineage/attach-parents registry %)))
  =
  (gen/fmap (fn [rows] {:msgs (vec rows)})
            (gen/vector (gen/elements [{:agent-id "csr-scan" :project-id "hive-carto"}
                                       {:agent-id "hm-leak" :project-id "global"}
                                       {:agent-id "ghost" :project-id "global"}
                                       {:agent-id "csr-scan" :parent-id "coordinator:9" :project-id "hive-carto"}])
                        0 6))
  {:num-tests 60})

(deftest measured-telemetry-rows-reach-only-their-own-coordinator
  (testing "rows measured on the live coordinator 2026-10-09: a foreign ling's
            parentless project-scoped telemetry"
    (let [rows   [{:agent-id "csr-scan" :project-id "hive-carto" :event-type :progress}
                  {:agent-id "hm-leak" :project-id "hive-mcp" :event-type :progress}]
          routed (lineage/attach-parents registry rows)]
      (is (= ["hm-leak"] (map :agent-id (aud/filter-messages "coordinator:246159-hive" routed))))
      (is (= ["csr-scan"] (map :agent-id (aud/filter-messages "coordinator:31311-hive" routed))))
      (testing "without the registry parent, both reached both sessions"
        (is (= 2 (count (aud/filter-messages "coordinator:246159-hive" rows))))))))

(deftest an-absent-registry-attaches-nothing
  (is (= [{:agent-id "x"}] (lineage/attach-parents (constantly nil) [{:agent-id "x"}]))))
