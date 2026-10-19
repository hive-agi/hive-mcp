(ns hive-mcp.nrepl.m16-facet-test
  "Trifecta for nrepl.classloader-gc/clamp... no — classloader-gc's pure fn is
   count-dynamic-classloaders; the middleware is a stateful wrapper covered by
   classloader_gc_test.clj. This namespace pins the status map and the
   rotation threshold setter as goldens against the test's own state."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.nrepl.classloader-gc :as gc]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- gc-fixture [f]
  (let [saved-threshold @#'gc/classloader-rotation-threshold
        saved-session @#'gc/shared-session-id
        saved-counter @#'gc/session-eval-counter]
    (try (f)
         (finally
           (reset! @#'gc/classloader-rotation-threshold saved-threshold)
           (reset! @#'gc/shared-session-id saved-session)
           (reset! @#'gc/session-eval-counter saved-counter)))))

(use-fixtures :each gc-fixture)

(deftest count-dynamic-classloaders-contract-test
  (let [out (gc/count-dynamic-classloaders)]
    (is (m/validate (-> (m/function-schemas)
                        (get-in ['hive-mcp.nrepl.classloader-gc 'count-dynamic-classloaders :schema])
                        m/children second)
                    out))
    (is (<= 0 (:dynamic-classloader-count out) (:chain-depth out)))))

(deftest rotation-threshold-round-trip-test
  (gc/set-rotation-threshold! 42)
  (is (= 42 (:rotation-threshold (gc/status))))
  (gc/set-rotation-threshold! 0)
  (is (= 0 (:rotation-threshold (gc/status))))
  (is (map? (gc/force-rotate!))))

;; Golden over plain, JVM-independent data only: the status map carries live
;; atom values that the golden harness's pprint cannot safely traverse.
(golden/deftest-golden classloader-status-golden
  "test/golden/hive-mcp/m16-classloader-status.edn"
  {:threshold-set (do (gc/set-rotation-threshold! 7) 7)
   :status-keys (into #{} (keys (gc/status)))
   :count-keys (into #{} (keys (gc/count-dynamic-classloaders)))})

;; The mg/import proves the out schemas generate (brief: every schema samples)
(deftest output-schemas-generate-test
  (is (every? some?
              (mg/sample (-> (m/function-schemas)
                             (get-in ['hive-mcp.nrepl.classloader-gc 'status :schema])
                             m/children second)
                         {:size 3}))))
