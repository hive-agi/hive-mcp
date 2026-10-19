(ns hive-mcp.knowledge-graph.connection.store-facet-test
  (:require [hive-mcp.knowledge-graph.connection.store :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden store-golden
  "test/golden/hive-mcp/m13-connection-store.edn"
  (boolean (subject/store-live?)))
