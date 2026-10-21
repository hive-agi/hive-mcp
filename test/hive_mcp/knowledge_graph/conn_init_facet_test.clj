(ns hive-mcp.knowledge-graph.conn-init-facet-test
  (:require [hive-mcp.knowledge-graph.conn-init :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden conn-init-golden
  "test/golden/hive-mcp/m13-conn-init.edn"
  (let [init (subject/atom-conn-init)] [(subject/snapshot init) (subject/open-once! init (constantly :opened)) (subject/snapshot init)]))
