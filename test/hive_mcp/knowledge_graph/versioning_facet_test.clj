(ns hive-mcp.knowledge-graph.versioning-facet-test
  (:require [hive-mcp.knowledge-graph.versioning :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden versioning-golden
  "test/golden/hive-mcp/m13-versioning.edn"
  (subject/status))
