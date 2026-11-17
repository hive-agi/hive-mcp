(ns hive-mcp.embeddings.protocol
  "EmbeddingProvider — `def` aliases of hive-spi.embeddings.ports.

   The protocol itself lives in hive-spi; every historical
   hive-mcp.embeddings.protocol/* name still resolves here. `satisfies?`
   must be called on the ports vars, never on these aliases."
  (:require [hive-spi.embeddings.ports :as ports]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def EmbeddingProvider ports/EmbeddingProvider)

;; Methods are defn delegates, not `def` aliases: an alias captures the method
;; fn at load time and misses any impl registered later by map-based `extend`.
(defn embed-text "Delegates to `ports/embed-text`." [provider text]
  (ports/embed-text provider text))
(defn embed-batch "Delegates to `ports/embed-batch`." [provider texts]
  (ports/embed-batch provider texts))
(defn embedding-dimension "Delegates to `ports/embedding-dimension`." [provider]
  (ports/embedding-dimension provider))

;; Port-method delegates. The provider input is loose here (protocol dispatch
;; rejects a non-provider) and malli arrives late, so these stay :any on both
;; sides; the typed Provider\/Vector pair lives with the value-object schemas
;; below, which this ns loads after them.
(m/=> embed-text [:=> [:cat :any :string] [:sequential number?]])
(m/=> embed-batch [:=> [:cat :any [:sequential :string]] [:sequential [:sequential number?]]])
(m/=> embedding-dimension [:=> [:cat :any] :int])

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
