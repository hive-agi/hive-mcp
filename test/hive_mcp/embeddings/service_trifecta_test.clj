(ns hive-mcp.embeddings.service-trifecta-test
  "Schema-derived property, golden and mutation facets for the contracted pure
   parts of the embedding service, plus stubbed-port tests for the active
   provider and per-collection routing."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.embeddings.config :as config]
            [hive-mcp.embeddings.registry :as registry]
            [hive-mcp.embeddings.service :as service]
            [hive-mcp.embeddings.warmup :as warmup]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; -----------------------------------------------------------------------------
;; estimate-tokens
;; -----------------------------------------------------------------------------

(defn- expected-tokens
  "Oracle read off the docstring: ceil(chars / ratio), ratio defaulting to
   `default-chars-per-token` when absent or non-positive."
  [content ratio]
  (let [r (if (and (number? ratio) (pos? ratio)) (double ratio) 3.0)
        n (count (or content ""))]
    (long (Math/ceil (/ n r)))))

(deftrifecta-from-schema estimate-tokens-trifecta
  hive-mcp.embeddings.service/estimate-tokens
  {:in [:cat [:maybe :string] [:maybe [:enum -1 0 1 2.5 3.0 4]]]
   :out [:int {:min 0}]
   :rel (fn [[content ratio] out] (= out (expected-tokens content ratio)))
   :num-tests 300
   :seed 89
   :n-cases 10
   :golden-path "test/golden/embeddings/service-estimate-tokens.edn"})

(deftest validate-content-size-arms
  (testing "content that fits answers nil"
    (is (nil? (service/validate-content-size! "abc" {:max-tokens 1 :provider-key :p}))))
  (testing "oversized content throws :embedder/input-too-large"
    (is (= :embedder/input-too-large
           (try (service/validate-content-size! "abcdefg" {:max-tokens 1 :provider-key :p})
                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))

(deftest no-embed-types-register-by-name
  (is (= "c4-snapshot-test" (service/register-no-embed-type! :c4-snapshot-test)))
  (is (true? (service/no-embed-type? "c4-snapshot-test")))
  (is (false? (service/no-embed-type? nil)))
  (is (nil? (service/register-no-embed-type! nil))))

;; -----------------------------------------------------------------------------
;; Per-collection routing with a stubbed factory
;; -----------------------------------------------------------------------------

(defrecord FixedProvider [dim]
  hive-mcp.embeddings.protocol/EmbeddingProvider
  (embed-text [_ _] (vec (repeat dim 0.5)))
  (embed-batch [_ texts] (mapv (fn [_] (vec (repeat dim 0.5))) texts))
  (embedding-dimension [_] dim))

(use-fixtures :each
  (fn [t]
    (service/reset-service!)
    (active/reset-embedding-provider!)
    (registry/register-factory! :ollama (fn [cfg] (->FixedProvider (:dimension cfg))))
    (try (t)
         (finally
           (service/reset-service!)
           (active/reset-embedding-provider!)
           (registry/init!)))))

(deftest configured-collection-routes-through-the-registry
  (let [cfg (config/->EmbeddingConfig :ollama "nomic-embed-text" 3 {})]
    (is (nil? (service/configure-collection! "test-coll" cfg)))
    (is (= cfg (service/get-collection-config "test-coll")))
    (is (= {"test-coll" "ollama/nomic-embed-text (3 dims)"}
           (service/list-configured-collections)))
    (is (= 3 (service/get-dimension-for "test-coll")))
    (is (= [0.5 0.5 0.5] (service/embed-for-collection "test-coll" "hello")))
    (is (= [[0.5 0.5 0.5]] (service/embed-batch-for-collection "test-coll" ["x"])))
    (is (true? (service/provider-available-for? "test-coll")))
    (is (m/validate [:map [:has-config? [:= true]] [:dimension [:= 3]]]
                    (service/collection-embedding-status "test-coll")))
    (is (= 1 (:collection-count (service/status))))
    (is (nil? (service/unconfigure-collection! "test-coll")))
    (is (nil? (service/get-collection-config "test-coll")))))

(deftest unconfigured-collection-without-fallback-throws
  ;; Availability must agree with resolution: nothing resolves "nowhere"
  ;; (no route, no global provider), so it is NOT available, and
  ;; get-provider-for throws.
  (is (false? (service/provider-available-for? "nowhere")))
  (is (thrown? clojure.lang.ExceptionInfo (service/get-provider-for "nowhere"))))

(deftest active-provider-lifecycle
  (is (false? (active/embedding-configured?)))
  (is (thrown? clojure.lang.ExceptionInfo (active/require-embedding!)))
  (is (nil? (active/set-embedding-provider! (->FixedProvider 2))))
  (is (true? (active/embedding-configured?)))
  (is (nil? (active/require-embedding!)))
  (is (= [0.5 0.5] (active/embed-text (active/get-embedding-provider) "x")))
  (is (= [[0.5 0.5]] (active/embed-batch (->FixedProvider 2) ["x"])))
  (testing "a collection with no route falls back to the global provider"
    (is (= 2 (active/get-dimension-for "not-a-memory-collection")))
    (is (= [0.5 0.5] (active/embed-text-for "not-a-memory-collection" "x")))
    (is (= [[0.5 0.5]] (active/embed-batch-for "not-a-memory-collection" ["x"]))))
  (is (nil? (active/reset-embedding-provider!))))

(deftest registry-surface
  (is (true? (registry/register-venice!)))
  (is (m/validate registry/CacheStats (registry/cache-stats)))
  (is (some #{:ollama} (registry/list-factories)))
  (testing "an INVALID config is unavailable, never a throw: the log path is total"
    (is (false? (registry/provider-available? {:not "a config"})))
    (is (false? (registry/provider-available? nil)))
    (is (string? (config/describe {:not "a config"})))
    (is (string? (config/describe nil))))
  (registry/register-factory! :openai (fn [_] (throw (ex-info "down" {}))))
  (is (false? (registry/provider-available?
               (config/->EmbeddingConfig :openai "m" 4 {}))))
  (is (true? (registry/provider-available?
              (config/->EmbeddingConfig :ollama "m" 4 {}))))
  (is (map? (registry/unregister-factory! :no-such-type)))
  (is (nil? (registry/clear-cache!))))

;; -----------------------------------------------------------------------------
;; warmup probe through an injected port
;; -----------------------------------------------------------------------------

(deftest warmup-probe-arms
  (let [ident {:host "h" :model "m" :keys #{:k}}
        before @hive-mcp.embeddings.resilient/warm-providers]
    (try
      (is (true? (warmup/probe! (constantly [1.0]) ident)))
      (is (false? (warmup/probe! (constantly []) ident)) "an empty vector is not warm")
      (is (false? (warmup/probe! (fn [_] (throw (ex-info "down" {}))) ident)))
      (finally (reset! hive-mcp.embeddings.resilient/warm-providers before)))))
