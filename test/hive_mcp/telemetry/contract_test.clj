(ns hive-mcp.telemetry.contract-test
  "Coverage gate for the telemetry tree. The per-namespace facets live in
   core-contract-test, health-contract-test and prometheus-contract-test;
   this namespace pins that every public defn is contracted and that every
   contract generates both its argument and its result side."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.telemetry.core]
            [hive-mcp.telemetry.health]
            [hive-mcp.telemetry.prometheus]
            [hive-schemas.coverage :as coverage]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Coverage gate
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage telemetry-contracts
  {:paths ["src/hive_mcp/telemetry"]})

(deftest telemetry-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/telemetry"]})]
    (is (= 24 (count (:universe report))))
    (is (= 24 (count (:covered report))))))

(deftest every-telemetry-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.telemetry.core hive-mcp.telemetry.health hive-mcp.telemetry.prometheus]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
