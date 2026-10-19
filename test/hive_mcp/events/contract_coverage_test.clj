(ns hive-mcp.events.contract-coverage-test
  (:require [hive-schemas.coverage :as coverage]))

;; The adjacent handlers.clj and handlers/ subtree belong to batch m11.
;; Enumerating the root files preserves the complete m10 universe without
;; measuring a neighboring batch whose source cannot be edited here.
(coverage/deftest-contract-coverage root-event-contracts
  {:paths ["src/hive_mcp/events/bridge.clj"
           "src/hive_mcp/events/channel_bridge.clj"
           "src/hive_mcp/events/config.clj"
           "src/hive_mcp/events/context.clj"
           "src/hive_mcp/events/contributions.clj"
           "src/hive_mcp/events/core.clj"
           "src/hive_mcp/events/dispatch.clj"
           "src/hive_mcp/events/effects.clj"
           "src/hive_mcp/events/metrics.clj"
           "src/hive_mcp/events/registry.clj"
           "src/hive_mcp/events/schemas.clj"
           "src/hive_mcp/events/write_events.clj"]})

(coverage/deftest-contract-coverage effect-submodule-contracts
  {:paths ["src/hive_mcp/events/effects"]})
