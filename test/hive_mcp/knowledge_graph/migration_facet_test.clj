(ns hive-mcp.knowledge-graph.migration-facet-test
  (:require [hive-mcp.knowledge-graph.migration :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden migration-golden
  "test/golden/hive-mcp/m13-migration.edn"
  (select-keys (subject/validate-migration {:edges 0 :disc 0 :synthetic 0}) [:valid? :expected :actual :missing]))
