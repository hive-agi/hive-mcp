(ns hive-mcp.knowledge-graph.protocols-facet-test
  (:require [hive-mcp.knowledge-graph.protocols :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden protocols-golden
  "test/golden/hive-mcp/m13-protocols.edn"
  (some? subject/IEdgeReader))
