(ns hive-mcp.recall.contract-coverage-test
  "Contract coverage gates for the recall tree, one per directory: the two
   files of src/hive_mcp/recall and the src/hive_mcp/recall/canary directory.
   Every public defn carries an m/=> contract and every contract generates."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as coverage]
            [malli.core :as m]
            [malli.generator :as mg]
            [hive-mcp.recall.canary]
            [hive-mcp.recall.canary.live]
            [hive-mcp.recall.golden]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; golden.clj's recall-fault / rank-fault are `(def x #'canary/x)` var aliases:
;; calls through them reach the contracted canary fns, and the gate's :paths
;; include the file so they are in view.
(coverage/deftest-contract-coverage recall-contracts
  {:paths ["src/hive_mcp/recall/canary.clj"
           "src/hive_mcp/recall/golden.clj"]
   :exempt {}})

(coverage/deftest-contract-coverage recall-canary-contracts
  {:paths ["src/hive_mcp/recall/canary"]
   :exempt {}})

(deftest the-gates-are-not-vacuous
  (is (= 16 (count (:universe (coverage/coverage {:paths ["src/hive_mcp/recall/canary.clj"
                                                           "src/hive_mcp/recall/golden.clj"]})))))
  (is (= 7 (count (:universe (coverage/coverage {:paths ["src/hive_mcp/recall/canary"]}))))))

(deftest every-recall-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.recall.canary hive-mcp.recall.golden hive-mcp.recall.canary.live]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
