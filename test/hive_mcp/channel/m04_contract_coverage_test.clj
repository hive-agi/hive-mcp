(ns hive-mcp.channel.m04-contract-coverage-test
  "Gate: every public fn in the channel namespaces of coverage batch m04
   carries an `m/=>` contract or a REASONED exemption, and no exemption is
   stale. The rest of src/hive_mcp/channel is batch m05's and is gated in
   `hive-mcp.channel.m05-contract-coverage-test`."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage channel-m04-contracts-gate
  {:paths ["src/hive_mcp/channel/a2a.clj"
           "src/hive_mcp/channel/activation.clj"
           "src/hive_mcp/channel/async_result.clj"
           "src/hive_mcp/channel/async_result_journal.clj"
           "src/hive_mcp/channel/audience.clj"
           "src/hive_mcp/channel/axiom_projection.clj"
           "src/hive_mcp/channel/blocks.clj"
           "src/hive_mcp/channel/broadcast_ledger.clj"
           "src/hive_mcp/channel/broadcast_policy.clj"
           "src/hive_mcp/channel/commitment.clj"
           "src/hive_mcp/channel/context_store.clj"
           "src/hive_mcp/channel/conversation_inbox.clj"
           "src/hive_mcp/channel/core.clj"
           "src/hive_mcp/channel/drain_metrics.clj"
           "src/hive_mcp/channel/drain_projection.clj"]
   :exempt {}})
