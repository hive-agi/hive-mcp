(ns hive-mcp.embeddings.contract-coverage-test
  "Standing coverage gate for the embeddings bounded context."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]))

(deftest-contract-coverage embeddings-contract-coverage
  {:paths ["src/hive_mcp/embeddings/boot.clj"
           "src/hive_mcp/embeddings/cache.clj"
           "src/hive_mcp/embeddings/config.clj"
           "src/hive_mcp/embeddings/deadline.clj"
           "src/hive_mcp/embeddings/http_client.clj"
           "src/hive_mcp/embeddings/model_spec.clj"
           "src/hive_mcp/embeddings/ollama.clj"
           "src/hive_mcp/embeddings/openai.clj"
           "src/hive_mcp/embeddings/openrouter.clj"
           "src/hive_mcp/embeddings/protocol.clj"
           "src/hive_mcp/embeddings/resilient.clj"
           "src/hive_mcp/embeddings/role.clj"
           "src/hive_mcp/embeddings/routing.clj"
           "src/hive_mcp/embeddings/shared_gate.clj"
           "src/hive_mcp/embeddings/space.clj"
           "src/hive_mcp/embeddings/venice.clj"]})
