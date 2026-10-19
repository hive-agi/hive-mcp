(ns hive-mcp.chroma.contract-coverage-test
  "Gate: every public fn under src/hive_mcp/chroma carries an `m/=>` contract
   or a REASONED exemption, and no exemption is stale."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; chroma.core re-exports most of its surface as `(def x #'other/x)` var
;; aliases. Calls through a var alias reach the instrumented target var, so
;; those aliases are covered by the target's contract; the core `defn`s that
;; branch on the active store carry their own.
(deftest-contract-coverage chroma-contracts-gate
  {:paths ["src/hive_mcp/chroma"]
   :exempt {}})
