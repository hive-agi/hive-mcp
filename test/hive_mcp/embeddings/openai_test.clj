(ns hive-mcp.embeddings.openai-test
  "OpenAI provider construction under an EDN-bound config (no ambient config,
   no network: the adapter touches HTTP only when a call is made). The golden
   pins the known-model dimension table and both refusals.

   Seam gap: embed-text / embed-batch reach the private `make-request`, which
   builds its own java.net.http request; there is no injectable transport, so
   the request shape is not facet-tested here (see the f04 report)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.openai :as openai]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- build [opts]
  (cfg/with-config {}
    (try (let [p (openai/->provider opts)]
           (assoc (into {} p) :dimension* (proto/embedding-dimension p)))
         (catch clojure.lang.ExceptionInfo e
           [(ex-message e) (dissoc (ex-data e) :supported :result)]))))

(deftest-golden provider-construction-golden
  "test/golden/hive-mcp/embeddings/openai-construction.edn"
  {:small    (build {:api-key "sk-t" :model "text-embedding-3-small" :api-base "http://stub/v1"})
   :large    (build {:api-key "sk-t" :model "text-embedding-3-large" :api-base "http://stub/v1"})
   :ada      (build {:api-key "sk-t" :model "text-embedding-ada-002" :api-base "http://stub/v1"})
   :unknown  (build {:api-key "sk-t" :model "not-an-openai-model" :api-base "http://stub/v1"})})

(deftest provider-satisfies-the-port
  (is (satisfies? proto/EmbeddingProvider
                  (openai/->OpenAIEmbedder "http://stub" "k" "text-embedding-3-small" 1536))))
