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

;; The breaker facade states the kernel's contracts over the kernel's schemas
;; (hive-mcp.resilience.breaker), so it needs no exemption.
(deftest-contract-coverage slots-contracts
  {:paths ["src/hive_mcp/knowledge_graph/slots"]
   :exempt {}})

(deftest-contract-coverage store-contracts
  {:paths ["src/hive_mcp/knowledge_graph/store"]
   :exempt {}})
