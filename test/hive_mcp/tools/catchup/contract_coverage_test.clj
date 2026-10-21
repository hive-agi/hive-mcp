(ns hive-mcp.tools.catchup.contract-coverage-test
  "Gate (coverage batch m26): every public fn in src/hive_mcp/tools/catchup
   carries an `m/=>` contract or a REASONED exemption, and no exemption is
   stale. The scope.clj `(def x #'other/x)` re-exports are var aliases (the
   var, not its value), so instrumentation reaches callers through them and
   the gate counts the target's contract."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage catchup-contracts-gate
  {:paths  ["src/hive_mcp/tools/catchup"]
   :exempt {}})
