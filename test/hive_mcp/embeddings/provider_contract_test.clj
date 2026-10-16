(ns hive-mcp.embeddings.provider-contract-test
  "Schema-derived facets for the contracted HTTP client predicate, and
   construction tests for every provider adapter with the transport stubbed
   (an executor-fn for Ollama; the OpenAI-spec adapters touch no network
   until a call is made)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.http-client :as http]
            [hive-mcp.embeddings.ollama :as ollama]
            [hive-mcp.embeddings.openai :as openai]
            [hive-mcp.embeddings.openrouter :as openrouter]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.resilient :as resilient]
            [hive-mcp.embeddings.venice :as venice]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m])
  (:import [java.io IOException]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; -----------------------------------------------------------------------------
;; fatal-client-error?
;; -----------------------------------------------------------------------------

(def ^:private messages
  "Fatal and benign messages, so generation reaches both branches."
  ["selector manager closed" "HttpClient has been shut down" "HttpClient closed"
   "the client is closed" "Connection refused" "Read timed out" "" nil])

(def ^:private fatal-messages
  #{"selector manager closed" "HttpClient has been shut down" "HttpClient closed"
    "the client is closed"})

;; No :golden-path: the inputs are Throwables, which an EDN snapshot cannot
;; read back. The golden facet is the literal table in
;; `fatal-client-error-table` below.
(deftrifecta-from-schema fatal-client-error-trifecta
  hive-mcp.embeddings.http-client/fatal-client-error?
  {:in [:cat [:fn {:gen/elements (mapv #(IOException. ^String %) messages)}
              #(instance? Throwable %)]]
   :out [:maybe :boolean]
   :rel (fn [[t] out]
          (let [msg (.getMessage ^Throwable t)]
            (= out (when msg (contains? fatal-messages msg)))))
   :num-tests 200
   :seed 83
   :n-cases 8})

(deftest fatal-client-error-table
  (is (= {"selector manager closed" true
          "HttpClient has been shut down" true
          "HttpClient closed" true
          "the client is closed" true
          "Connection refused" false
          "Read timed out" false
          "" false
          nil nil}
         (into {} (for [msg messages]
                    [msg (http/fatal-client-error? (IOException. ^String msg))])))))

(deftest client-cache-shape
  (let [cache (http/mk-client (constantly :c1))]
    (is (m/validate http/ClientCache cache))
    (is (= :c1 (http/get-client cache)))
    (is (= :c1 (http/rebuild! cache :stale)))))

;; -----------------------------------------------------------------------------
;; Provider construction
;; -----------------------------------------------------------------------------

(defn- ok-executor
  "Stub GPU executor answering every embed with a fixed vector."
  [_req]
  {:ok {:gpu/output {:vectors [[0.25 0.5]]}}})

(deftest ollama-provider-routes-through-the-executor
  (let [p (ollama/->provider {:model "nomic-embed-text" :host "http://stub"
                              :executor-fn ok-executor})]
    (is (m/validate proto/Provider p))
    (is (= 768 (proto/embedding-dimension p)))
    (is (= [0.25 0.5] (proto/embed-text p "x")))
    (is (= [[0.25 0.5] [0.25 0.5]] (proto/embed-batch p ["a" "b"])))
    (testing "an unknown model with no declared dimension is refused"
      (is (thrown? clojure.lang.ExceptionInfo
                   (ollama/->provider {:model "no-such-model" :executor-fn ok-executor}))))
    (testing "a declared dimension admits an unknown model"
      (is (= 12 (proto/embedding-dimension
                 (ollama/->provider {:model "custom" :declared {:dimension 12}
                                     :executor-fn ok-executor})))))))

(deftest openai-spec-providers-construct-without-network
  (doseq [[ctor model dim] [[openai/->provider "text-embedding-3-large" 3072]
                            [openrouter/->provider "cohere/embed-english-v3.0" 1024]
                            [openrouter/->provider "unknown/model" 4096]
                            [venice/->provider "text-embedding-qwen3-8b" 4096]]]
    (let [p (ctor {:api-key "sk-test" :model model})]
      (is (m/validate proto/Provider p))
      (is (= dim (proto/embedding-dimension p)) model)))
  (is (thrown? clojure.lang.ExceptionInfo
               (openai/->provider {:api-key "sk-test" :model "not-an-openai-model"}))))

(deftest resilient-embedder-is-a-provider
  (let [p (ollama/->provider {:model "nomic-embed-text" :executor-fn ok-executor})
        r (resilient/resilient-embedder [{:provider p :provider-key :ollama}]
                                        {:warmth (atom #{}) :health (atom {})})]
    (is (m/validate proto/Provider r))
    (is (= 768 (proto/embedding-dimension r)))
    (is (= [0.25 0.5] (proto/embed-text r "x")))
    (is (m/validate proto/Provider
                    (resilient/resilient-embedder [{:provider p}] 1000 2000)))
    (is (thrown? clojure.lang.ExceptionInfo (resilient/resilient-embedder [])))))
