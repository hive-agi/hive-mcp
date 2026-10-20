(ns hive-mcp.knowledge-graph.edge-cycle-facet-test
  (:require [hive-mcp.knowledge-graph.edge-cycle :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden edge-cycle-golden
  "test/golden/hive-mcp/m13-edge-cycle.edn"
  (subject/run-cycle! {:fetch (constantly [1 2]) :step! (constantly :done) :outcome-keys [:done :skipped]}))
