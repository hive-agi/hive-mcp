(ns hive-mcp.tools.catchup.store-port-contract-test
  "The store-facing catchup namespaces (bundle, hydration, hierarchy,
   axiom-cache, scope facade, spawn) over the IMemoryStore PORT: a seeded
   StubMemoryStore installed as the active store, no driver, no with-redefs.

   Goldens project the deterministic fields (:id :type :content) because the
   stub stamps :updated with the wall clock."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.catchup.axiom-cache :as axc]
            [hive-mcp.tools.catchup.bundle :as bundle]
            [hive-mcp.tools.catchup.contract-fixtures :as fx]
            [hive-mcp.tools.catchup.hierarchy :as hier]
            [hive-mcp.tools.catchup.hydration :as hydr]
            [hive-mcp.tools.catchup.scope :as scope]
            [hive-mcp.tools.catchup.shapes :as shapes]
            [hive-mcp.tools.catchup.spawn :as spawn]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :refer [deftest-mutations]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fx/with-seeded-store)

(defn- ids [es] (mapv :id es))
(defn- slim [es] (mapv #(select-keys % [:id :type :content]) es))

(deftest-golden catchup-bundle-over-stub
  "test/golden/catchup/store-port-bundle.edn"
  (fx/with-seeded-store
    #(into (sorted-map) (update-vals (bundle/query-catchup-bundle "p" {}) slim))))

(deftest bundle-shapes-hold
  (testing "personalized arity: every bucket, vectors, content hydrated"
    (let [b (bundle/query-catchup-bundle "p" {})]
      (is (m/validate bundle/CatchupBundle b))
      (is (= ["e1"] (ids (:axioms b))))
      (is (= ["e4"] (ids (:priority-conventions b))))
      (is (= ["e5"] (ids (:conventions b))))
      (is (= ["e3"] (ids (:decisions b))) "the sibling project's decision never surfaces")
      (is (= "decision one" (:content (first (:decisions b)))))))
  (testing "shared arity goes through the bundle cache"
    (is (identical? (bundle/query-catchup-bundle "p") (bundle/query-catchup-bundle "p")))))

(deftest query-all-scoped-groups-by-type
  (let [{:keys [by-type all]} (bundle/query-all-scoped "p")]
    (is (m/validate bundle/AllScoped {:by-type by-type :all all}))
    (is (= #{"axiom" "decision" "convention"} (set (keys by-type))))
    (is (= ["e3" "e5" "e4" "e1"] (ids all)) "merged, newest first")))

(deftest per-type-queries
  (is (= ["e3"] (ids (bundle/query-scoped-entries "decision" nil "p" 10))))
  (is (= ["e4"] (ids (bundle/query-scoped-entries "convention" ["catchup-priority"] "p" 10))))
  (is (= ["e5" "e2"] (ids (bundle/query-regular-conventions "p" #{} #{"e4"}))))
  (is (= [] (vec (bundle/query-expiring-entries "p" 5))) "nothing in the seed expires"))

(deftest-mutations query-regular-conventions-excludes
  hive-mcp.tools.catchup.bundle/query-regular-conventions
  [["no exclusion" (fn [pid _ _] (bundle/query-scoped-entries "convention" nil pid 50))]
   ["axioms only"  (fn [pid a _] (remove #(contains? a (:id %)) (bundle/query-scoped-entries "convention" nil pid 50)))]]
  (fn []
    (is (= ["e5"] (ids (bundle/query-regular-conventions "p" #{"e2"} #{"e4"}))))))

(deftest hydration-swaps-shells
  (let [out (hydr/hydrate-buckets {:axioms [{:id "e1" :distance 0.3}] :decisions [{:id "zz"} {:id "e3"}]})]
    (is (= "axiom one" (get-in out [:axioms 0 :content])))
    (is (= 0.3 (get-in out [:axioms 0 :distance])) "the vector score survives hydration")
    (is (= {:id "zz"} (get-in out [:decisions 0])) "a missing id passes through as its shell")
    (is (= {} (hydr/hydrate-buckets {}))))
  (is (true? (hydr/has-tag? {:tags ["a" "b"]} "b")))
  (is (false? (hydr/has-tag? nil "b"))))

(deftest hierarchy-fan-out
  (is (= ["e2" "e3" "e4" "e5"] (sort (ids (hier/chunked-hierarchy-fetch (mem-proto/get-store) ["p"] 10))))
      "metadata rows for exactly the requested project")
  (is (= [] (hier/chunked-hierarchy-fetch (mem-proto/get-store) [] 10)))
  (is (nil? (hier/compute-hierarchy-project-ids "global")))
  (is (nil? (hier/compute-hierarchy-project-ids nil))))

(deftest-mutations chunk-count-partition
  hive-mcp.tools.catchup.hierarchy/chunk-count
  [["off by one" (fn [ids] (inc (count (partition-all 15 ids))))]
   ["no floor"   (fn [ids] (count (partition-all 15 ids)))]
   ["constant 1" (constantly 1)]]
  (fn []
    (is (= 1 (hier/chunk-count [])))
    (is (= 1 (hier/chunk-count (range 15))))
    (is (= 3 (hier/chunk-count (range 31))))))

(deftest axiom-cache-swr
  (testing "formal and legacy rows are unioned, newest first"
    (is (= ["e2" "e1"] (ids (axc/query-axioms "p")))))
  (testing "the scope facade forwards to the same cache"
    (is (= (axc/query-axioms "p") (scope/query-axioms "p"))))
  (testing "invalidation by project and in full"
    (is (= {} (axc/invalidate-axioms-cache! "p")))
    (axc/query-axioms "p")
    (is (= 0 (axc/evict-stale!)) "fresh entries are not evicted")
    (is (= {} (axc/invalidate-axioms-cache!)))))

(deftest scope-facade-project-name
  (is (nil? (scope/get-current-project-name)))
  (is (= "b" (scope/get-current-project-name "/nonexistent-m26/a/b")))
  (is (nil? (scope/get-current-project-name "/")) "the root has no last segment"))

(deftest spawn-context-full-mode
  (let [s (spawn/spawn-context "/nonexistent-m26/p")]
    (is (string? s))
    (is (re-find #"## Project Context \(Auto-Injected\)" s))
    (is (re-find #"### Git Status" s)))
  (testing ":hints without the agent domain falls back to :full"
    (is (string? (spawn/spawn-context "/nonexistent-m26/p" {:mode :hints}))))
  (testing ":ref without cached refs falls back to :full"
    (is (string? (spawn/spawn-context "/nonexistent-m26/p" {:mode :ref})))))

(deftest entry-schema-covers-store-rows
  (is (every? #(m/validate shapes/Entry %) fx/seed)))
