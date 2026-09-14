(ns hive-mcp.embeddings.env-config
  "Typed env-var / override resolution for embedding providers, via hive-di.

   Separate from hive-mcp.embeddings.config (which is the per-collection
   EmbeddingConfig value-object ns). This ns carries only the provider
   defaults sourced from env + overrides:

     OllamaConfig         / resolve-OllamaConfig
     OpenAIConfig         / resolve-OpenAIConfig
     OpenRouterConfig     / resolve-OpenRouterConfig
     VeniceConfig         / resolve-VeniceConfig

   Resolution order (per hive-di):
     1. Explicit overrides map passed to (resolve-*Config overrides)
     2. Environment variable lookup
     3. blank->nil normalization (\"\" → trigger default)
     4. Pre-typed default (skips coercion)
     5. hive-dsl.coerce on string env values

   Endpoint defaults (host, api-base) are descriptors and carry a default.
   :model carries none: hive-mcp chooses no embedding model. OpenAI,
   OpenRouter and Venice require it (a missing value resolves to {:error ...}
   naming the env var); Ollama leaves it optional because host-only callers
   (list-models, pull-model) resolve the same config."
  (:require [hive-di.core :refer [defconfig env]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defconfig OllamaConfig
  :host  (env "OLLAMA_HOST"
              :default "http://localhost:11434"
              :type    :string
              :doc     "Ollama server base URL (no trailing slash)")
  :model (env "OLLAMA_MODEL"
              :required false
              :type     :string
              :doc      "Embedding model name. No default: pass :model or set OLLAMA_MODEL."))

(defconfig OpenAIConfig
  :api-base (env "OPENAI_API_BASE"
                 :default "https://api.openai.com/v1"
                 :type    :string
                 :doc     "OpenAI API base URL — /embeddings is appended.")
  :model    (env "OPENAI_EMBEDDING_MODEL"
                 :type    :string
                 :doc     "Embedding model name. Required: pass :model or set OPENAI_EMBEDDING_MODEL."))

(defconfig OpenRouterConfig
  :api-base (env "OPENROUTER_API_BASE"
                 :default "https://openrouter.ai/api/v1"
                 :type    :string
                 :doc     "OpenRouter API base URL — /embeddings is appended.")
  :model    (env "OPENROUTER_EMBEDDING_MODEL"
                 :type    :string
                 :doc     "Embedding model name. Required: pass :model or set OPENROUTER_EMBEDDING_MODEL."))

(defconfig VeniceConfig
  :api-base (env "VENICE_API_BASE"
                 :default "https://api.venice.ai/api/v1"
                 :type    :string
                 :doc     "Venice API base URL — /embeddings is appended.")
  :model    (env "VENICE_EMBEDDING_MODEL"
                 :type    :string
                 :doc     "Embedding model name. Required: pass :model or set VENICE_EMBEDDING_MODEL."))
