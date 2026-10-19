(ns hive-mcp.knowledge-graph.schema-contract-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.schema :as schema]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-test.trifecta :refer [deftrifecta]]))

;; The subject is the contracted function, not a test-local wrapper.
(deftrifecta valid-abstraction-level-contract
  hive-mcp.knowledge-graph.schema/valid-abstraction-level?
  {:golden-path "test/golden/hive-mcp/schema-valid-abstraction-level.edn"
   :cases {:lowest 1 :highest 4 :runtime 0 :too-high 5 :nil nil}
   :gen (clojure.test.check.generators/choose -10 15)
   :pred boolean?
   :apply? false
   :num-tests 500
   :mutations [["always-valid" (constantly true)]
               ["always-invalid" (constantly false)]
               ["accept-runtime" (fn [n] (and (integer? n) (<= 0 n 4)))]]})
