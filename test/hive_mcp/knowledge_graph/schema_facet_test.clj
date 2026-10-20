(ns hive-mcp.knowledge-graph.schema-facet-test
  (:require [hive-mcp.knowledge-graph.schema :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden schema-golden
  "test/golden/hive-mcp/m13-schema.edn"
  [(subject/valid-node-id? "id") (subject/valid-node-id? " ") (subject/valid-node-id? "$ref") (subject/valid-node-id? nil)])
