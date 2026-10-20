(ns hive-mcp.knowledge-graph.grounding-facet-test
  (:require [hive-mcp.knowledge-graph.grounding :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden grounding-golden
  "test/golden/hive-mcp/m13-grounding.edn"
  [(subject/entry-attr {:source-file "a"} :source-file) (subject/entry-attr {:metadata {:source-file "b"}} :source-file) (subject/entry-attr {:source-file ""} :source-file)])
