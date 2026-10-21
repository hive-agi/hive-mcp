(ns hive-mcp.workflows.m31-contract-coverage-test
  "Gate: every public fn under src/hive_mcp/workflows (coverage batch m31,
   saa/ included) carries an `m/=>` contract or a REASONED exemption, and no
   exemption is stale.

   The `(def x #'other/x)` re-exports in saa-workflow, the saa.predicates and
   wrap/complete/catchup `always` aliases, and the strategy.clj facade are
   VAR aliases: invoking them goes through the var, so the contract on the
   aliased defn is reached by every caller. They are not defns and so are not
   in this gate's universe."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage workflows-m31-contracts-gate
  {:paths ["src/hive_mcp/workflows"]
   :exempt {}})
