(ns hive-mcp.embeddings.openrouter-test
  "OpenRouter provider construction and set-as-default! under an EDN-bound
   config, with the active-provider slot owned and restored by the test.
   No network: the adapter touches HTTP only when a call is made.

   Seam gap: embed-text / embed-batch reach the private `make-request`
   (java.net.http built inline); no injectable transport (see f04 report)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.embeddings.openrouter :as openrouter]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- build [opts]
  (cfg/with-config {}
    (let [p (openrouter/->provider opts)]
      (assoc (into {} p) :dimension* (proto/embedding-dimension p)))))

(deftest-golden provider-construction-golden
  "test/golden/hive-mcp/embeddings/openrouter-construction.edn"
  {:qwen    (build {:api-key "sk-t" :model "qwen/qwen3-embedding-8b" :api-base "http://stub/v1"})
   :small   (build {:api-key "sk-t" :model "openai/text-embedding-3-small" :api-base "http://stub/v1"})
   :cohere  (build {:api-key "sk-t" :model "cohere/embed-english-v3.0" :api-base "http://stub/v1"})
   :unknown (build {:api-key "sk-t" :model "someone/new-model" :api-base "http://stub/v1"})})

(deftest-golden set-as-default-golden
  "test/golden/hive-mcp/embeddings/openrouter-set-as-default.edn"
  (let [prior (active/get-embedding-provider)]
    (try
      (cfg/with-config {}
        (let [ret (openrouter/set-as-default! {:api-key "sk-t" :model "cohere/embed-english-v3.0"
                                               :api-base "http://stub/v1"})]
          {:returns ret
           :configured? (active/embedding-configured?)
           :dimension (proto/embedding-dimension (active/get-embedding-provider))}))
      (finally
        (if prior (active/set-embedding-provider! prior) (active/reset-embedding-provider!))))))

(deftest missing-model-is-refused
  (is (thrown? clojure.lang.ExceptionInfo
               (cfg/with-no-config (openrouter/->provider {:api-key "sk-t"})))))
