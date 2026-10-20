(ns hive-mcp.knowledge-graph.scope-facet-test
  (:require [hive-mcp.knowledge-graph.scope :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden scope-golden
  "test/golden/hive-mcp/m13-scope.edn"
  (subject/scope->tag "my-scope"))
