(ns hive-mcp.knowledge-graph.connection.detect-facet-test
  (:require [hive-mcp.knowledge-graph.connection.detect :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden detect-golden
  "test/golden/hive-mcp/m13-connection-detect.edn"
  (subject/detect-backend))
