(ns hive-mcp.embeddings.boot-test
  "The :embeddings boot step over INJECTED ports: an EDN-bound config source
   and the in-memory Chroma transport. With no embedding model configured the
   step wires Chroma and the EmbeddingService and configures no provider; the
   golden pins that across two Chroma endpoints (configured and default).

   With a model configured the global fallback provider comes from the
   :ollama-provider-fn factory seam, so that branch runs with no HTTP probe."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.embeddings.boot :as boot]
            [hive-mcp.embeddings.service :as svc]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-test.golden :refer [deftest-golden]]
            [hive-mcp.embeddings.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-ports [f]
  (let [prior-t   (client/transport)
        prior-p   (active/get-embedding-provider)
        prior-cfg (conn/get-config)]
    (client/set-transport! (transport/->transport))
    (try (f)
         (finally
           (conn/configure! (select-keys prior-cfg [:host :port :collection-name]))
           (client/set-transport! prior-t)
           (if prior-p
             (active/set-embedding-provider! prior-p)
             (active/reset-embedding-provider!))))))

(use-fixtures :each own-ports)

(defn- boot-under [config]
  (active/reset-embedding-provider!)
  (cfg/with-config config
    {:returned    (boot/init-embedding-provider!)
     :chroma      (select-keys (conn/get-config) [:host :port])
     :service     (select-keys (svc/status) [:initialized? :global-fallback?])
     :provider?   (active/embedding-configured?)}))

(deftest-golden boot-without-model-golden
  "test/golden/hive-mcp/embeddings/boot-without-model.edn"
  ;; Both endpoints come from the bound config, which outranks CHROMA_HOST /
  ;; CHROMA_PORT, so the snapshot does not depend on the machine's env.
  {:endpoint-a (boot-under {:services {:chroma {:host "chroma.boot" :port 8123}}})
   :endpoint-b (boot-under {:services {:chroma {:host "chroma.other" :port 9001}}})})

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
    (cfg/with-config {:services {:chroma {:host "chroma.boot" :port 8123}}
                      :embeddings {:ollama {:host "http://ollama.stub:1" :model "nomic-embed-text"}}}
      (is (true? (boot/init-embedding-provider!
                  {:ollama-provider-fn (fn [opts] (swap! calls conj opts) fake)}))))
    (is (= [{:host "http://ollama.stub:1" :model "nomic-embed-text"}] @calls))
    ;; the active provider is the injected one, behind the shared embed gate
    (is (identical? fake (:delegate (active/get-embedding-provider) (active/get-embedding-provider))))))
