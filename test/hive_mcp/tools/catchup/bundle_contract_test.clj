(ns hive-mcp.tools.catchup.bundle-contract-test
  "catchup.bundle over the IMemoryStore PORT: a seeded StubMemoryStore as the
   active store, no driver, no with-redefs. The golden projects the
   deterministic fields (:id :type :content) because the stub stamps :updated
   with the wall clock."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.catchup.bundle :as bundle]
            [hive-mcp.tools.catchup.contract-fixtures :as fx]
            [hive-mcp.tools.catchup.shapes :as shapes]
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
      (is (= ["e5" "e2"] (ids (:conventions b))) "an untagged row of \"p\" is visible by :project-id")
      (is (= ["e3"] (ids (:decisions b))) "the sibling project's decision never surfaces")
      (is (= "decision one" (:content (first (:decisions b)))))))
  (testing "shared arity goes through the bundle cache"
    (is (identical? (bundle/query-catchup-bundle "p") (bundle/query-catchup-bundle "p")))
    (is (m/validate shapes/Buckets (bundle/query-catchup-bundle "p")))))

(deftest query-all-scoped-groups-by-type
  (let [{:keys [by-type all]} (bundle/query-all-scoped "p")]
    (is (m/validate bundle/AllScoped {:by-type by-type :all all}))
    (is (= #{"axiom" "decision" "convention"} (set (keys by-type))))
    (is (= ["e3" "e5" "e2" "e4" "e1"] (ids all)) "merged, newest first")))

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
