(ns hive-mcp.knowledge-graph.m14-contract-coverage-test
  "One contract-coverage gate per m14 directory."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage disc-contracts
  {:paths ["src/hive_mcp/knowledge_graph/disc"]
   :exempt {}})

(deftest-contract-coverage edges-contracts
  {:paths ["src/hive_mcp/knowledge_graph/edges"]
   :exempt {}})

(def ^:private breaker-facade-reason
  (str "call-through facade over the hive-mcp.resilience.breaker kernel var: its "
       "result is whatever the kernel var returns, and a redef of the kernel must "
       "reach callers of this name (hive-mcp.kernel.misfiled-facade-test), so the "
       "contract belongs on the kernel fn, outside this batch"))

(deftest-contract-coverage slots-contracts
  {:paths ["src/hive_mcp/knowledge_graph/slots"]
   :exempt (into {}
                 (map (fn [f] [(symbol "hive-mcp.knowledge-graph.slots.breaker" f)
                               breaker-facade-reason]))
                 ["fresh" "on-success" "on-failure" "maybe-recover" "decision" "attempt"
                  "record-success!" "record-failure!" "snapshot" "reset!"])})

(deftest-contract-coverage store-contracts
  {:paths ["src/hive_mcp/knowledge_graph/store"]
   :exempt {}})
