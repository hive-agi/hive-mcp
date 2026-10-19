(ns hive-mcp.knowledge-graph.m13-contract-coverage-test
  (:require [hive-schemas.coverage :refer [deftest-contract-coverage]]))

(deftest-contract-coverage knowledge-graph-and-connection-contracts
  {:paths [
           "src/hive_mcp/knowledge_graph/conn_init.clj"
           "src/hive_mcp/knowledge_graph/connection/detect.clj"
           "src/hive_mcp/knowledge_graph/connection/strategy.clj"
           "src/hive_mcp/knowledge_graph/connection/temporal.clj"
           "src/hive_mcp/knowledge_graph/connection/writer.clj"
           "src/hive_mcp/knowledge_graph/connection.clj"
           "src/hive_mcp/knowledge_graph/edge_cycle.clj"
           "src/hive_mcp/knowledge_graph/grounding.clj"
           "src/hive_mcp/knowledge_graph/migration.clj"
           "src/hive_mcp/knowledge_graph/protocol.clj"
           "src/hive_mcp/knowledge_graph/protocols.clj"
           "src/hive_mcp/knowledge_graph/queries.clj"
           "src/hive_mcp/knowledge_graph/schema.clj"
           "src/hive_mcp/knowledge_graph/scope.clj"
           "src/hive_mcp/knowledge_graph/synth.clj"
           "src/hive_mcp/knowledge_graph/versioning.clj"
          ]
   :exempt {}})
