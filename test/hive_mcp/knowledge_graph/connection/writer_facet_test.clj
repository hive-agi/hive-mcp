(ns hive-mcp.knowledge-graph.connection.writer-facet-test
  (:require [hive-mcp.knowledge-graph.connection.writer :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden writer-golden
  "test/golden/hive-mcp/m13-connection-writer.edn"
  [(subject/flush-status 0) (subject/flush-status 2)])
