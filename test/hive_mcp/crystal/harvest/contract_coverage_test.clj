(ns hive-mcp.crystal.harvest.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/crystal/harvest carries an `m/=>`
   contract or a REASONED exemption, and no exemption is stale. Only the gate
   lives here; the facets sit in one test namespace per source namespace."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage harvest-contracts-gate
  {:paths ["src/hive_mcp/crystal/harvest"]
   :exempt {}})
