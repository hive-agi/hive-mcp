(ns hive-mcp.embeddings.active
  "Embedding provider management (atom-backed registry, routing helpers).

   The EmbeddingProvider protocol itself has moved to
   hive-mcp.embeddings.protocol so that embedder implementations do not
   need to depend on this (chroma-layer) namespace. Aliases below keep
   backward compatibility for any existing callers that reach for
   `chroma.embeddings/EmbeddingProvider` or its method fns."
  (:require [hive-mcp.embeddings.protocol :as proto]
            [taoensso.timbre :as log]
            [hive-mcp.embeddings.shared-gate :as shared]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; --- Backward-compat re-exports for the relocated protocol ---
(def EmbeddingProvider proto/EmbeddingProvider)
(defn embed-text
  "Invoke the embedding port; ensure even a supplied one-off provider crosses
   the process gate. Registry/global providers are already decorated."
  [provider text]
  (proto/embed-text (shared/gated-provider provider) text))

(m/=> embed-text [:=> [:cat proto/Provider :string] proto/Vector])
(defn embed-batch
  "Invoke the same gated embedding port for batch and single calls."
  [provider texts]
  (proto/embed-batch (shared/gated-provider provider) texts))

(m/=> embed-batch [:=> [:cat proto/Provider [:sequential :string]] [:sequential proto/Vector]])
(def embedding-dimension proto/embedding-dimension)

(defonce ^:private embedding-provider (atom nil))

(defn set-embedding-provider!
  "Set the global embedding provider through the same gated port as the
   registry. Wrapping is idempotent for registry-sourced providers."
  [provider]
  (reset! embedding-provider (some-> provider shared/gated-provider))
  (log/info "Embedding provider set:" (type provider)))

;; nil clears the provider; the body ends in log/info, which returns nil.
(m/=> set-embedding-provider! [:=> [:cat [:maybe proto/Provider]] :nil])

(defn embedding-configured?
  "Check if an embedding provider is configured."
  []
  (some? @embedding-provider))

(m/=> embedding-configured? [:=> [:cat] :boolean])

(defn get-embedding-provider
  "Get the current embedding provider."
  []
  @embedding-provider)

(m/=> get-embedding-provider [:=> [:cat] [:maybe proto/Provider]])

(defn reset-embedding-provider!
  "Reset the embedding provider atom."
  []
  (reset! embedding-provider nil)
  (log/info "Embedding provider reset (was:" (type @embedding-provider) ")"))

(m/=> reset-embedding-provider! [:=> [:cat] :nil])

(defn require-embedding!
  "Guard: throws if embedding provider not configured."
  []
  (when-not @embedding-provider
    (throw (ex-info "Embedding provider not configured. Call set-embedding-provider! first."
                    {:type :no-embedding-provider}))))

(m/=> require-embedding! [:=> [:cat] :nil])

(defn get-provider-for
  "Get embedding provider for a specific collection."
  [collection-name]
  (try
    (require 'hive-mcp.embeddings.service)
    (let [get-provider (resolve 'hive-mcp.embeddings.service/get-provider-for)]
      (get-provider collection-name))
    (catch Exception _
      (or @embedding-provider
          (throw (ex-info "No embedding provider available" {:collection collection-name}))))))

;; Collection names are strings at every call site; the service answers nil
;; for a collection it has no route for, so the result may be nil.
(m/=> get-provider-for [:=> [:cat [:maybe :string]] [:maybe proto/Provider]])

(defn embed-text-for
  "Embed text using the provider configured for the collection."
  [collection-name text]
  (let [provider (get-provider-for collection-name)]
    (embed-text provider text)))

(m/=> embed-text-for [:=> [:cat [:maybe :string] :string] proto/Vector])

(defn embed-batch-for
  "Embed multiple texts using the provider configured for the collection."
  [collection-name texts]
  (let [provider (get-provider-for collection-name)]
    (embed-batch provider texts)))

(m/=> embed-batch-for [:=> [:cat [:maybe :string] [:sequential :string]] [:sequential proto/Vector]])

(defn get-dimension-for
  "Get embedding dimension for a collection's provider."
  [collection-name]
  (let [provider (get-provider-for collection-name)]
    (embedding-dimension provider)))

(m/=> get-dimension-for [:=> [:cat [:maybe :string]] pos-int?])
