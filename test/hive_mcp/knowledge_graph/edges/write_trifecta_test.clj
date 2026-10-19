(ns hive-mcp.knowledge-graph.edges.write-trifecta-test
  "Trifecta for edge-tx-data over valid specs, its ex-info refusals, and a
   golden over the write surface against a fresh store."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.edges.m14-support :as sup]
            [hive-mcp.knowledge-graph.edges.queries :as q]
            [hive-mcp.knowledge-graph.edges.write :as write]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ValidSpec
  [:map
   [:from [:enum "a" "b"]]
   [:to [:enum "c" "d"]]
   [:relation [:enum :implements :refines :depends-on :relates]]
   [:confidence {:optional true} [:double {:min 0.0 :max 1.0}]]
   [:scope {:optional true} [:enum "s1" "s2"]]
   [:source-type {:optional true} [:enum :manual :inferred]]
   [:predicate {:optional true} [:enum "Part Of" "causes"]]])

;; No :golden-path: every tx carries a fresh id and timestamp.
(hst/deftrifecta-from-schema edge-tx-data-contract
  hive-mcp.knowledge-graph.edges.write/edge-tx-data
  {:in [:cat ValidSpec]
   :out (sup/out-schema 'hive-mcp.knowledge-graph.edges.write 'edge-tx-data)
   :rel (fn [[spec] tx]
          (and (= (:from spec) (:kg-edge/from tx))
               (= (:to spec) (:kg-edge/to tx))
               (= (:relation spec) (:kg-edge/relation tx))
               (= (get spec :confidence 1.0) (:kg-edge/confidence tx))
               (= (:scope spec) (:kg-edge/scope tx))
               (= (boolean (and (= :relates (:relation spec)) (:predicate spec)))
                  (contains? tx :kg-edge/predicate))))
   :classify (fn [[spec] _] (:relation spec))
   :classify-domain #{:implements :refines :depends-on :relates}
   :num-tests 200})

(deftest edge-tx-data-refuses-malformed-specs
  (doseq [[label spec] {:nil nil
                        :blank-from {:from " " :to "b" :relation :implements}
                        :placeholder {:from "$0" :to "b" :relation :implements}
                        :relation {:from "a" :to "b" :relation :nope}
                        :confidence {:from "a" :to "b" :relation :implements :confidence 2.0}
                        :source-type {:from "a" :to "b" :relation :implements :source-type :nope}}]
    (testing (name label)
      (is (thrown? clojure.lang.ExceptionInfo (write/edge-tx-data spec))))))

(defn- writes []
  (sup/in-fresh-store
   (fn []
     (let [ids (write/add-edges! [{:from "w1" :to "w2" :relation :implements :scope "s"}
                                  {:from "w2" :to "w3" :relation :refines}])
           none (write/add-edges! [])
           sync-ids (write/add-edges-sync! [{:from "w3" :to "w1" :relation :depends-on}])
           one (write/add-edge! {:from "w1" :to "w3" :relation :relates :predicate "Part Of"}
                                conn/transact-sync!)
           [a] ids]
       {:ids (count ids) :none none :sync (count sync-ids) :one-string? (string? one)
        :update (write/update-edge-confidence! a 0.25)
        :update-missing (write/update-edge-confidence! "no-such-edge" 0.25)
        :increment (write/increment-confidence! a 0.5)
        :increment-clamped (write/increment-confidence! a 5.0)
        :increment-missing (write/increment-confidence! "no-such-edge" 0.1)
        :verify (write/verify-edge! a)
        :verify-missing (write/verify-edge! "no-such-edge")
        :event-fallback (write/emit-stats-event! :kg.m14/unregistered {} (constantly :fallback))
        :edges (sup/shown-all (q/get-all-edges))
        :removed (write/remove-edges-for-node! "w1")
        :left (sup/shown-all (q/get-all-edges))}))))

(golden/deftest-golden write-golden
  "test/golden/hive-mcp/m14-edges-write.edn"
  (writes))

(deftest a-bad-spec-in-a-batch-writes-nothing
  (sup/in-fresh-store
   (fn []
     (is (thrown? clojure.lang.ExceptionInfo
                  (write/add-edges! [{:from "x" :to "y" :relation :implements}
                                     {:from "x" :to "y" :relation :nope}])))
     (is (zero? (q/count-edges))))))
