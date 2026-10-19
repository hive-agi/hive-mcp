(ns hive-mcp.analysis.resolve-trifecta-test
  "`hive-mcp.analysis.resolve` against its contract, and the coverage gate for
   src/hive_mcp/analysis."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.analysis.resolve :as resolve]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [clojure.java.io :as io]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage analysis-contracts-gate
  {:paths ["src/hive_mcp/analysis"]
   :exempt {}})

(def ^:private kondo-present?
  "Is the kondo addon on this classpath? Asked of the classpath directly."
  (some? (io/resource "clj_kondo_mcp/core.clj")))

(hst/deftrifecta-from-schema resolve-kondo-analysis-contract
  hive-mcp.analysis.resolve/resolve-kondo-analysis
  {:in        [:cat]
   :out       [:maybe ifn?]
   ;; callable exactly when the addon is on the classpath, nil otherwise
   :rel       (fn [_ out] (= kondo-present? (some? out)))})

(deftest resolves-the-same-var-every-time-test
  (is (= (resolve/resolve-kondo-analysis) (resolve/resolve-kondo-analysis))))
