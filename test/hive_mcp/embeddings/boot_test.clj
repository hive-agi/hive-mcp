(ns hive-mcp.embeddings.boot-test
  "The :embeddings boot step over an INJECTED, EDN-bound config source. With
   no embedding model configured the step wires the EmbeddingService and
   configures no provider; the golden pins that across two configs.

   It configures no vector store: Chroma left for the hive-chroma addon, which
   owns its own endpoint (every backend addon does).

   With a model configured the global fallback provider comes from the
   :ollama-provider-fn factory seam, so that branch runs with no HTTP probe."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.embeddings.boot :as boot]
            [hive-mcp.embeddings.service :as svc]
            [hive-test.golden :refer [deftest-golden]]
            [hive-mcp.embeddings.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-ports [f]
  (let [prior-p (active/get-embedding-provider)]
    (try (f)
         (finally
           (if prior-p
             (active/set-embedding-provider! prior-p)
             (active/reset-embedding-provider!))))))

(use-fixtures :each own-ports)

(defn- boot-under [config]
  (active/reset-embedding-provider!)
  (cfg/with-config config
    {:returned    (boot/init-embedding-provider!)
     :service     (select-keys (svc/status) [:initialized? :global-fallback?])
     :provider?   (active/embedding-configured?)}))

(deftest-golden boot-without-model-golden
  "test/golden/hive-mcp/embeddings/boot-without-model.edn"
  ;; A :services :chroma section is ignored now: no vector store is configured
  ;; by this step, so both configs boot the same way.
  {:endpoint-a (boot-under {:services {:chroma {:host "chroma.boot" :port 8123}}})
   :endpoint-b (boot-under {})})

(deftest boot-answers-a-boolean
  (is (boolean? (cfg/with-config {} (boot/init-embedding-provider!)))))

(deftest boot-with-model-uses-the-provider-factory-seam
  ;; COV-FACETS-FINDINGS-W2 (f04 seam gap): with a model configured the step
  ;; built a real Ollama provider (HTTP probe of <host>/api/tags) and took no
  ;; factory. :ollama-provider-fn now injects it.
  (active/reset-embedding-provider!)
  (let [calls (atom [])
        fake  (reify proto/EmbeddingProvider
                (embed-text [_ _] [0.0])
                (embed-batch [_ ts] (mapv (constantly [0.0]) ts))
                (embedding-dimension [_] 1))]
    (cfg/with-config {:embeddings {:ollama {:host "http://ollama.stub:1" :model "nomic-embed-text"}}}
      (is (true? (boot/init-embedding-provider!
                  {:ollama-provider-fn (fn [opts] (swap! calls conj opts) fake)}))))
    (is (= [{:host "http://ollama.stub:1" :model "nomic-embed-text"}] @calls))
    ;; the active provider is the injected one, behind the shared embed gate
    (is (identical? fake (:delegate (active/get-embedding-provider) (active/get-embedding-provider))))))
