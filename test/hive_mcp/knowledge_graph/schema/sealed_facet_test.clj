(ns hive-mcp.knowledge-graph.schema.sealed-facet-test
  (:require [hive-mcp.knowledge-graph.schema.sealed :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden sealed-golden
  "test/golden/hive-mcp/m13-schema-sealed.edn"
  (select-keys subject/sealed-edge-schema [:kg-sealed/blob :kg-sealed/tombstone]))
