(ns hive-mcp.embeddings.contract-coverage-test
  "Standing coverage gate for the embeddings bounded context."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]))

(deftest-contract-coverage embeddings-contract-coverage
  {:paths ["src/hive_mcp/embeddings"]})
