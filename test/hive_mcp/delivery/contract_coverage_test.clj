(ns hive-mcp.delivery.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/delivery carries an `m/=>` contract
   or a REASONED exemption, and no exemption is stale. Only the gate lives here;
   the facets sit in fanout-test and channels-trifecta-test."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage delivery-contracts-gate
  {:paths ["src/hive_mcp/delivery"]
   :exempt {}})
