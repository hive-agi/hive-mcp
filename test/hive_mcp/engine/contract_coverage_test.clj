(ns hive-mcp.engine.contract-coverage-test
  "Standing coverage gate for direct engine namespaces."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]))

(deftest-contract-coverage engine-contract-coverage
  {:paths ["src/hive_mcp/engine/health.clj"
           "src/hive_mcp/engine/manifold_pool.clj"]})
