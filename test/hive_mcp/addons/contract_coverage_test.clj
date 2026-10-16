(ns hive-mcp.addons.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/addons carries an `m/=>` contract
   or a REASONED exemption, and no exemption is stale. Only the gate lives here;
   the trifectas sit in one test namespace per source namespace."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; headless.clj defines the aliases `headless-id` ... by `(def x spi/x)`. The
;; defn-reading universe does not list a bare `def`, so those aliases carry
;; their own `m/=>` (instrumentation reaches facade callers only that way) and
;; are asserted by `hive-mcp.addons.headless-contract-test`.
(deftest-contract-coverage addons-contracts-gate
  {:paths ["src/hive_mcp/addons"]
   :exempt {}})
