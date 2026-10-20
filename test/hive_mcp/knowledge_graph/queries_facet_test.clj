(ns hive-mcp.knowledge-graph.queries-facet-test
  (:require [hive-mcp.knowledge-graph.queries :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden queries-golden
  "test/golden/hive-mcp/m13-queries.edn"
  (subject/find-orphan-nodes))
