(ns hive-mcp.embeddings.boot
  "Boot step that wires embeddings: the Chroma connection, the EmbeddingService
   and the per-collection provider routing.

   This is MEMORY DOMAIN work, not kernel work: every name it touches is a
   vector store or an embedding provider, and a build with no memory domain
   has nothing to configure. It used to sit in `hive-mcp.server.init`, which
   is why the kernel required chroma.core and three embeddings namespaces.

   The kernel now keeps only the entry point (`server.init/init-embedding-provider!`),
   which runs whatever contributed the `:embeddings` boot step: this namespace
   while it ships inside core (declared in
   resources/hive-mcp/boot-contributions.edn), the hive-memory addon at
   `initialize!` afterwards. With nobody contributing, boot skips the step and
   says so, which is what a kernel-only build should do."
  (:require [hive-mcp.chroma.core :as chroma]
            [hive-mcp.config.core :as global-config]
            [hive-mcp.dns.result :as result]
            [hive-mcp.embeddings.config :as embedding-config]
            [hive-mcp.embeddings.ollama :as ollama]
            [hive-mcp.embeddings.service :as embedding-service]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn init-embedding-provider!
  "Initialize embedding providers for semantic memory search.

  Sets up:
  1. Chroma connection (vector database)
  2. EmbeddingService (per-collection routing)
  3. Per-collection embedding configuration:
     - hive-mcp-memory  : Ollama
     - hive-mcp-presets : OpenRouter when configured, else Ollama
     - hive-mcp-plans   : OpenRouter when configured, else Ollama
     - hive-ingest      : OpenRouter when configured
  4. Global fallback provider (Ollama)

  Configuration precedence:
  1. ~/.config/hive-mcp/config.edn :embeddings section
  2. ~/.config/hive-mcp/config.edn :services / :secrets sections
  3. Environment variables (OLLAMA_HOST, OPENROUTER_API_KEY, etc.) as fallback
  4. Built-in endpoint defaults (hosts only). Embedding models have no default:
     embeddings.ollama.model / embeddings.openrouter.model must be set, and a
     missing one is logged as an error naming the key."
  []
  (result/rescue-log "init-embedding-provider!" false
    ;; Load global config to get :embeddings section
                 (let [cfg (global-config/get-global-config)
                       embed-cfg (get cfg :embeddings {})
                       ollama-cfg (get embed-cfg :ollama {})
                       openrouter-cfg (get embed-cfg :openrouter {})]

      ;; Configure Chroma connection - config.edn :services > env vars > defaults
                   (let [chroma-host (global-config/get-service-value :chroma :host :env "CHROMA_HOST" :default "localhost")
                         chroma-port (global-config/get-service-value :chroma :port :env "CHROMA_PORT" :parse parse-long :default 8000)]
                     (chroma/configure! {:host chroma-host :port chroma-port})
                     (log/info "Chroma configured:" chroma-host ":" chroma-port))

      ;; Initialize EmbeddingService for per-collection routing
                   (embedding-service/init!)

      ;; Ollama host from :embeddings > :services > env vars > default endpoint.
      ;; Embedding models come from :embeddings only; there is no default model.
                   (let [ollama-host (or (:host ollama-cfg)
                                         (global-config/get-service-value :ollama :host
                                                                          :env "OLLAMA_HOST"
                                                                          :default "http://localhost:11434"))
                         ollama-model (:model ollama-cfg)
                         openrouter-key? (boolean (global-config/get-secret :openrouter-api-key))
                         openrouter-model (when openrouter-key? (:model openrouter-cfg))
                         ollama-emb-cfg (when ollama-model
                                          (result/rescue nil
                                                         (embedding-config/ollama-config {:host ollama-host :model ollama-model})))
                         configure-ollama! (fn [collection]
                                             (when ollama-emb-cfg
                                               (result/rescue nil
                                                              (embedding-service/configure-collection! collection ollama-emb-cfg))))
                         configure-openrouter! (fn [collection]
                                                 (boolean
                                                  (and openrouter-model
                                                       (result/rescue false
                                                                      (embedding-service/configure-collection!
                                                                       collection
                                                                       (embedding-config/openrouter-config {:model openrouter-model}))
                                                                      true))))]

                     (when-not ollama-model
                       (log/error "No Ollama embedding model configured: set embeddings.ollama.model"
                                  "(hive config set embeddings.ollama.model <model-id>)."
                                  "Ollama-backed collections and the global fallback provider are left unconfigured."))
                     (when (and openrouter-key? (not openrouter-model))
                       (log/error "OPENROUTER_API_KEY is set but no OpenRouter embedding model is configured:"
                                  "set embeddings.openrouter.model (hive config set embeddings.openrouter.model <model-id>)."
                                  "OpenRouter-backed collections fall back to Ollama."))

        ;; Memory collection: Ollama
                     (configure-ollama! "hive-mcp-memory")

        ;; Presets collection: OpenRouter when configured, else Ollama
                     (if (configure-openrouter! "hive-mcp-presets")
                       (log/info "Presets collection configured with OpenRouter")
                       (configure-ollama! "hive-mcp-presets"))

        ;; Plans collection: OpenRouter when configured, else Ollama with a truncation warning
                     (if (configure-openrouter! "hive-mcp-plans")
                       (log/info "Plans collection configured with OpenRouter")
                       (do
                         (configure-ollama! "hive-mcp-plans")
                         (log/warn "Plans collection using Ollama - entries >1500 chars may be truncated")))

        ;; Ingest collection: OpenRouter when configured
                     (when (configure-openrouter! "hive-ingest")
                       (log/info "Ingest collection configured with OpenRouter"))

        ;; Global fallback provider (Ollama), only with a configured model
                     (when ollama-model
                       (chroma/set-embedding-provider! (ollama/->provider {:host ollama-host :model ollama-model}))
                       (log/info "Global fallback embedding provider: Ollama at" ollama-host))

                     (log/info "Embedding config from config.edn:" {:ollama-host ollama-host
                                                                    :ollama-model ollama-model
                                                                    :openrouter-model openrouter-model})
                     (log/info "EmbeddingService status:" (embedding-service/status))
                     true))))
