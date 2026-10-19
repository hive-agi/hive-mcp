(ns hive-mcp.knowledge-graph.synth-facet-test
  (:require [hive-mcp.knowledge-graph.synth :as subject]
            [hive-test.golden :as golden]))

(golden/deftest-golden synth-golden
  "test/golden/hive-mcp/m13-synth.edn"
  (subject/batch-write! []))
