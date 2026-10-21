(ns hive-mcp.tools.events.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/tools/events carries an `m/=>`
   contract or a REASONED exemption, and no exemption is stale (coverage
   batch m27)."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage events-contracts-gate
  {:paths  ["src/hive_mcp/tools/events"]
   :exempt {}})
