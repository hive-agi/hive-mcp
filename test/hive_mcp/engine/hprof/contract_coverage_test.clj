(ns hive-mcp.engine.hprof.contract-coverage-test
  "Standing coverage gate for the hprof rotation bounded context."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]))

(deftest-contract-coverage hprof-contract-coverage
  {:paths ["src/hive_mcp/engine/hprof"]})
