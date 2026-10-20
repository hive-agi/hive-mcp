(ns hive-mcp.channel.m05-contract-coverage-test
  "Gate: every public fn in the channel namespaces of coverage batch m05 and
   under src/hive_mcp/channel/piggyback carries an `m/=>` contract or a
   REASONED exemption, and no exemption is stale. The rest of
   src/hive_mcp/channel is batch m04's and is gated there."
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage channel-m05-contracts-gate
  {:paths ["src/hive_mcp/channel/drain_rank.clj"
           "src/hive_mcp/channel/drain_telemetry.clj"
           "src/hive_mcp/channel/instruction_store.clj"
           "src/hive_mcp/channel/memory_piggyback.clj"
           "src/hive_mcp/channel/payload_ref.clj"
           "src/hive_mcp/channel/piggyback.clj"
           "src/hive_mcp/channel/piggyback_tap.clj"
           "src/hive_mcp/channel/row_transforms.clj"
           "src/hive_mcp/channel/task_signal.clj"
           "src/hive_mcp/channel/websocket.clj"
           "src/hive_mcp/channel/lineage.clj"]
   :exempt {}})

(deftest-contract-coverage channel-piggyback-contracts-gate
  {:paths ["src/hive_mcp/channel/piggyback"]
   :exempt {}})
