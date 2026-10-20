(ns hive-mcp.crystal.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/crystal (crystal/harvest included;
   it also has its own gate) carries an `m/=>` contract or a REASONED
   exemption, and no exemption is stale. Only the gate lives here; the facets
   sit in core-contract-test, contract-facets-test and the per-ns suites."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage crystal-contracts-gate
  {:paths ["src/hive_mcp/crystal"]
   :exempt {}})
