(ns hive-mcp.knowledge-graph.slots.registry-log-trifecta-test
  "Failure log severity remains distinct for optional factories and lifecycle errors."
  (:require [clojure.test.check.generators :as gen]
            [hive-mcp.knowledge-graph.slots.registry :as registry]
            [hive-test.trifecta :refer [deftrifecta]]))

(deftrifecta factory-failure-log-severity
  hive-mcp.knowledge-graph.slots.registry/factory-failure-level
  {:golden-path "test/golden/knowledge_graph/slots/factory-failure-log-severity.edn"
   :cases {:optional :factory-returned-nil
           :lifecycle :ensure-conn-failed
           :unknown nil}
   :gen (gen/elements [:factory-returned-nil :ensure-conn-failed nil])
   :pred #{:warn :error}
   :num-tests 40
   :mutations [["always-error" (fn [_] :error)]
               ["always-warn" (fn [_] :warn)]]})