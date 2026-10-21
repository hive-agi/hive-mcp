(ns hive-mcp.embeddings.protocol
  "EmbeddingProvider — `def` aliases of hive-spi.embeddings.ports.

   The protocol itself lives in hive-spi; every historical
   hive-mcp.embeddings.protocol/* name still resolves here. `satisfies?`
   must be called on the ports vars, never on these aliases."
  (:require [hive-spi.embeddings.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(do
  (def EmbeddingProvider ports/EmbeddingProvider)
  (def embed-text ports/embed-text)
  (def embed-batch ports/embed-batch)
  (def embedding-dimension ports/embedding-dimension))

;; =============================================================================
;; Value-object schemas for the port (shared by every embeddings namespace)
;; =============================================================================

(defn- fixed-provider
  "A deterministic EmbeddingProvider of dimension `dim`: the generator behind
   `Provider`, so every contract mentioning a provider can generate one."
  [dim]
  (reify ports/EmbeddingProvider
    (embed-text [_ _] (vec (repeat dim 0.5)))
    (embed-batch [_ texts] (mapv (fn [_] (vec (repeat dim 0.5))) texts))
    (embedding-dimension [_] dim)))

(def Provider
  "Anything satisfying hive-spi's EmbeddingProvider port. Generates small
   fixed providers."
  [:fn {:error/message "should satisfy EmbeddingProvider"
        :gen/schema [:int {:min 1 :max 8}]
        :gen/fmap fixed-provider}
   #(satisfies? ports/EmbeddingProvider %)])

(def Vector
  "One embedding: a sequence of numbers (providers return vectors of doubles)."
  [:sequential number?])
