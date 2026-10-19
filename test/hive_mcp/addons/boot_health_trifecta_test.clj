(ns hive-mcp.addons.boot-health-trifecta-test
  "Trifecta for the pure boot assessment (`hive-mcp.addons.boot-health/assess`)
   and the rendering fns. Property + mutation facets are synthesized from the
   contract (`BootFacts` -> `[:vector Issue]`) by hive-schemas.test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.addons.boot-health :as bh]
            [hive-mcp.addons.schema :as schema]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (bh/reset-state!) (try (f) (finally (bh/reset-state!)))))

(hst/deftrifecta-from-schema assess-contract
  hive-mcp.addons.boot-health/assess
  {:in        [:cat schema/BootFacts]
   :out       [:vector schema/Issue]
   :rel       (fn [[facts] issues]
                ;; failed addons always surface as a :mount-failed warning
                (= (boolean (seq (:failed facts)))
                   (boolean (some #(= :mount-failed (:issue %)) issues))))
   :num-tests 200
   :golden-path "test/golden/addons/boot-health-assess.edn"})

(deftest degraded-means-an-error-issue-test
  (is (false? (bh/degraded? [])))
  (is (false? (bh/degraded? [{:severity :warn}])))
  (is (true? (bh/degraded? [{:severity :warn} {:severity :error}]))))

(deftest record-and-render-roundtrip-test
  (testing "before any roster: no snapshot, not ready, no notice"
    (is (nil? (bh/snapshot)))
    (is (nil? (bh/notice)))
    (is (= "t" (bh/with-notice "t")))
    (is (false? (:ready? (bh/readiness-report)))))
  (testing "a degraded roster is recorded, noticed and reported"
    (let [snap (bh/record-roster! {:discovered ["a" "b"] :mounted ["a"] :failed ["b"]
                                   :expected-min 3} nil)]
      (is (m/validate schema/Snapshot snap))
      (is (true? (:degraded? snap)))
      (is (= "expected 3 addons, mounted 1 (discovered 2)" (bh/summary-line snap)))
      (is (re-find #"DEGRADED ADDON ROSTER" (bh/notice)))
      (is (re-find #"DEGRADED" (bh/with-notice "t")))
      (is (m/validate schema/ReadinessReport (bh/readiness-report)))
      (is (false? (:ready? (bh/readiness-report))))))
  (testing "memory facts re-judge the snapshot"
    (let [snap (bh/record-memory! {:backend "x" :deferred-to "mem" :store-set? false})]
      (is (some #(= :memory-store-missing (:issue %)) (:issues snap))))))

(deftest baseline-roundtrip-test
  (let [dir  (str (System/getProperty "java.io.tmpdir") "/boot-health-" (System/nanoTime))
        path (str dir "/last-healthy.edn")]
    (is (nil? (bh/read-baseline path)) "absent baseline reads nil")
    (bh/write-baseline! path {:discovered ["b" "a"] :mounted ["a"]})
    (let [b (bh/read-baseline path)]
      (is (= ["a" "b"] (:discovered b)) "written sorted")
      (is (= ["a"] (:mounted b))))
    (testing "a healthy boot persists a baseline; a degraded one does not lower it"
      (bh/record-roster! {:discovered ["a" "b" "c"] :mounted ["a" "b" "c"] :failed []} path)
      (is (= 3 (count (:discovered (bh/read-baseline path)))))
      (bh/record-roster! {:discovered ["a"] :mounted ["a"] :failed []} path)
      (is (= 3 (count (:discovered (bh/read-baseline path)))) "a drop never lowers the baseline"))))

(deftest default-baseline-path-test
  (is (string? (bh/default-baseline-path)))
  (is (re-find #"last-healthy\.edn$" (bh/default-baseline-path))))

(deftest schemas-generate-test
  (doseq [s [schema/BootFacts schema/BootIds schema/BootMemory schema/Issue
             schema/Snapshot schema/ReadinessReport]]
    (is (seq (mg/sample s {:size 5})) (str "generates: " (pr-str s)))))
