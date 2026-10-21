(ns hive-mcp.knowledge-graph.protocol-facet-test
  (:require [hive-mcp.knowledge-graph.protocol :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden protocol-golden
  "test/golden/hive-mcp/m13-protocol.edn"
  (boolean (subject/store-set?)))
