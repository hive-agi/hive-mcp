(ns hive-mcp.memory.synthesis-protection-test
  "Regression coverage for the synthesis-afterlife provider: confidence-floor
   membership and the fail-safe SPI registry seam."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-spi.memory.ports :as ports]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.memory.synthesis-protection :as sp]
            [hive-test.golden :refer [deftest-golden]]
            [hive-mcp.knowledge-graph.store.fixtures :as kg-fixtures]))

;; Restore whatever provider the running system had, so tests don't leave the
;; live reaper mis-wired.
(defn- with-clean-provider [f]
  (let [saved (ports/protected-ids)] ;; touch to force realization; value unused
    (try (f)
         (finally (ports/register-protection-provider! nil)
                  ;; re-install the real provider for the live system
                  (sp/install!)))
    saved))

(use-fixtures :each with-clean-provider)

(defn- stub-rows [rows]
  ;; the KG read seam answers [[entity] ...]; pull-shape is the entity map
  (sp/synthesis-protected-ids (fn [_q] (mapv vector rows))))

(deftest floor-membership
  (testing "members of synthetics at/above the floor are protected; below are released"
    (let [protected (stub-rows
                     [{:kg-synthetic/members ["a1" "a2"] :kg-synthetic/confidence 0.5}
                      {:kg-synthetic/members ["b1"]      :kg-synthetic/confidence sp/protection-confidence-floor}
                      {:kg-synthetic/members ["c1"]      :kg-synthetic/confidence 0.29}
                      {:kg-synthetic/members ["d1"]      :kg-synthetic/confidence nil}])]
      (is (= #{"a1" "a2" "b1"} protected))
      (is (set? protected))))
  (testing "a member shared by a low- and high-conf synthetic stays protected"
    (is (contains? (stub-rows [{:kg-synthetic/members ["x"] :kg-synthetic/confidence 0.1}
                               {:kg-synthetic/members ["x"] :kg-synthetic/confidence 0.9}])
                   "x"))))

(deftest provider-is-fail-safe
  (testing "no provider registered -> protect nothing"
    (ports/register-protection-provider! nil)
    (is (= #{} (ports/protected-ids))))
  (testing "registered provider result is surfaced as a set"
    (ports/register-protection-provider! (fn [] ["m1" "m2" "m1"]))
    (is (= #{"m1" "m2"} (ports/protected-ids))))
  (testing "a throwing provider never breaks the reaper"
    (ports/register-protection-provider! (fn [] (throw (ex-info "boom" {}))))
    (is (= #{} (ports/protected-ids)))))

(deftest provider-swallows-kg-failure
  (testing "synthesis-protected-ids yields #{} when the KG read throws"
    (is (= #{} (sp/synthesis-protected-ids (fn [_q] (throw (ex-info "kg down" {}))))))))

(deftest real-store-protects-seeded-members
  ;; COV-FACETS-FINDINGS-W2 (f04): the default read used to pass the db as an
  ;; extra :in input, DataScript threw "Extra inputs passed", the catch answered
  ;; #{} and protection was silently off. Against a REAL store, no stubs.
  (kg-fixtures/datascript-fixture
   (fn []
     (conn/transact! [{:kg-synthetic/id "s1" :kg-synthetic/members ["a1" "a2"]
                       :kg-synthetic/confidence 0.5}
                      {:kg-synthetic/id "s2" :kg-synthetic/members ["c1"]
                       :kg-synthetic/confidence 0.29}])
     (is (= #{"a1" "a2"} (sp/synthesis-protected-ids)))
     (sp/install!)
     (is (= #{"a1" "a2"} (ports/protected-ids))
         "the installed provider reaches the reaper through the SPI"))))

;; Against a REAL (fresh, in-memory) KG store, no stubs. Pins the fixed
;; behaviour: members of the synthetic above the floor are protected.
(deftest-golden real-store-protection-golden
  "test/golden/hive-mcp/memory/synthesis-protection-real-store.edn"
  (kg-fixtures/datascript-fixture
   (fn []
     (let [empty-kg (sp/synthesis-protected-ids)
           _        (conn/transact! [{:kg-synthetic/id "s1" :kg-synthetic/members ["a1" "a2"]
                                      :kg-synthetic/confidence 0.5}
                                     {:kg-synthetic/id "s2" :kg-synthetic/members ["c1"]
                                      :kg-synthetic/confidence 0.29}])
           seeded   (sp/synthesis-protected-ids)
           install  (sp/install!)]
       {:empty-kg (vec (sort empty-kg))
        :seeded   (vec (sort seeded))
        :install  install
        :floor    sp/protection-confidence-floor}))))
