(ns hive-mcp.knowledge-graph.connection.strategy-facet-test
  (:require [hive-mcp.knowledge-graph.connection.strategy :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden strategy-golden
  "test/golden/hive-mcp/m13-connection-strategy.edn"
  [(.getSimpleName (type (subject/select-strategy (atom []) false (constantly nil)))) (nil? (subject/assert-edge-node-ids! []))])
