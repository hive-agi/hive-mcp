(ns hive-mcp.knowledge-graph.connection.temporal-facet-test
  (:require [hive-mcp.knowledge-graph.connection.temporal :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden temporal-golden
  "test/golden/hive-mcp/m13-connection-temporal.edn"
  [(subject/temporal-store?) (subject/history-db)])
