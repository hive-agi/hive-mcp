(ns hive-mcp.knowledge-graph.connection-facet-test
  (:require [hive-mcp.knowledge-graph.connection :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden connection-golden
  "test/golden/hive-mcp/m13-connection.edn"
  (let [s (subject/gen-edge-id)] {:prefix (.startsWith s "edge-") :length (count s)}))
