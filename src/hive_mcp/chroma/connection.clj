(ns hive-mcp.chroma.connection
  "Chroma connection configuration, collection management, and health status."
  (:require [hive-mcp.chroma.client :as chroma]
            [hive-mcp.embeddings.active :as emb]
            [hive-mcp.embeddings.service :as embedding-service]
            [hive-mcp.embeddings.space :as space]
            [taoensso.timbre :as log] [hive-dsl.result :refer [rescue]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private default-config
  {:host "localhost"
   :port 8000
   :collection-name "hive-mcp-memory"})

(defonce ^:private config (atom default-config))

(def Config
  "Chroma connection settings; callers may merge in vendor options."
  [:map
   [:host :string]
   [:port :int]
   [:collection-name :string]])

(def Status
  "Chroma integration status."
  [:map
   [:configured? :boolean]
   [:provider [:maybe :string]]
   [:collection [:maybe :string]]
   [:host [:maybe :string]]
   [:port [:maybe :int]]])

(defn get-config
  "Get current Chroma configuration map."
  []
  @config)

(m/=> get-config [:=> [:cat] Config])

(defn configure!
  "Configure Chroma connection settings."
  [opts]
  (swap! config merge opts)
  (chroma/configure (select-keys opts [:host :port :api-version :tenant :database]))
  (log/info "Chroma configured:" (select-keys @config [:host :port :collection-name])))

(m/=> configure! [:=> [:cat [:map
                             [:host {:optional true} :string]
                             [:port {:optional true} :int]
                             [:collection-name {:optional true} :string]]]
                  :nil])

(defonce ^:private collection-cache (atom nil))

(defn- try-get-collection
  "Try to get existing collection, returns nil on failure."
  [coll-name]
  (rescue nil (deref (chroma/get-collection coll-name) 10000 nil)))

(defn- create-new-collection
  "Create a new Chroma collection with dimension metadata, plus the embedding
   space `stamp` (space/space->metadata) when the caller can name one."
  ([coll-name dim] (create-new-collection coll-name dim nil))
  ([coll-name dim stamp]
   (let [result (deref (chroma/create-collection
                        coll-name {:metadata (merge {:dimension dim :created-by "hive-mcp"} stamp)})
                       15000 ::timeout)]
     (when (= result ::timeout)
       (throw (ex-info "Chroma create-collection timed out" {:collection coll-name})))
     result)))

(defn- cache-collection!
  "Cache and return collection, logging action."
  [coll log-msg]
  (reset! collection-cache coll)
  (log/info log-msg)
  coll)

(defonce ^:private named-collection-cache (atom {}))

(def metric
  "Distance Chroma scores a collection with when no hnsw:space is given."
  :l2)

(defonce ^:private legacy-warned (atom #{}))

(declare admit-space!)

(defn get-or-create-collection
  "Get the default (unnamed) collection or create it. A new one is stamped
   with the global provider's space when that space can be named.

   With `space` (an EmbeddingSpace) the collection must hold it, as in
   `get-or-create-named-collection`: a stamped collection of another space
   throws ex-info :embedding-space-mismatch, an unstamped (legacy) one is
   admitted with a warning. No argument, or a nil `space`, skips the guard."
  ([] (get-or-create-collection nil))
  ([space]
   (let [coll-name (:collection-name @config)
         coll (or @collection-cache
                  (let [_ (emb/require-embedding!)
                        dim (emb/embedding-dimension (emb/get-embedding-provider))]
                    (if-let [existing (try-get-collection coll-name)]
                      (cache-collection! existing (str "Using existing Chroma collection: " coll-name))
                      (cache-collection!
                       (create-new-collection
                        coll-name dim
                        (some-> (or space (embedding-service/default-space {:metric metric}))
                                space/space->metadata))
                       (str "Created Chroma collection: " coll-name " dimension: " dim)))))]
     (if space (admit-space! coll-name coll space) coll))))

(m/=> get-or-create-collection
      [:function
       [:=> [:cat] chroma/Coll]
       [:=> [:cat [:maybe space/EmbeddingSpace]] chroma/Coll]])

(defn- admit-space!
  "Guard `coll` (named `coll-name`) for vectors of `space`. A stamped
   collection of another space throws :embedding-space-mismatch; an unstamped
   one is legacy: admitted, with one warning per collection per process."
  [coll-name coll space]
  (let [{:keys [verdict space-id]} (space/admit coll-name space (when (map? coll) (:metadata coll)))]
    (when (and (= :legacy verdict) (not (contains? @legacy-warned coll-name)))
      (swap! legacy-warned conj coll-name)
      (log/warn "Embedding space unrecorded for legacy collection" coll-name
                "- admitting vectors of space" space-id
                "unverified; isolate or re-embed to stamp it"))
    coll))

(defn get-or-create-named-collection
  "Get or create a named collection with explicit dimension.
   Caches per collection-name. Used by type-based embedder routing.

   With `space` (an EmbeddingSpace) a new collection is stamped with it, and
   an existing one must hold it: a stamped collection of another space throws
   ex-info :embedding-space-mismatch, so vectors of two spaces never share a
   collection; an unstamped (legacy) collection is admitted with a warning.
   A nil `space` (provider without a nameable space) skips the guard."
  ([coll-name dimension] (get-or-create-named-collection coll-name dimension nil))
  ([coll-name dimension space]
   (let [coll (or (get @named-collection-cache coll-name)
                  (let [coll (or (try-get-collection coll-name)
                                 (create-new-collection coll-name dimension
                                                        (some-> space space/space->metadata)))]
                    (swap! named-collection-cache assoc coll-name coll)
                    (log/info "Named collection ready:" coll-name "dimension:" dimension)
                    coll))]
     (if space (admit-space! coll-name coll space) coll))))

(m/=> get-or-create-named-collection
      [:function
       [:=> [:cat :string pos-int?] chroma/Coll]
       [:=> [:cat :string pos-int? [:maybe space/EmbeddingSpace]] chroma/Coll]])

(defn reset-collection-cache!
  "Reset the collection cache (for testing/reconnection)."
  []
  (reset! collection-cache nil)
  (reset! legacy-warned #{})
  (reset! named-collection-cache {}))

(m/=> reset-collection-cache! [:=> [:cat] [:map-of {:max 0} :any :any]])

(defn status
  "Get Chroma integration status."
  []
  {:configured? (emb/embedding-configured?)
   :provider (when-let [p (emb/get-embedding-provider)] (str (type p)))
   :collection (:collection-name @config)
   :host (:host @config)
   :port (:port @config)})

(m/=> status [:=> [:cat] Status])

(defn chroma-available?
  "Check if Chroma is configured and reachable."
  []
  (when (emb/embedding-configured?)
    (try
      (get-or-create-collection)
      true
      (catch Exception e
        (log/debug "Chroma availability check failed:" (.getMessage e))
        false))))

(m/=> chroma-available? [:=> [:cat] [:maybe :boolean]])

(defn reinitialize-embeddings!
  "Fix hot-reload protocol mismatch by reloading namespaces and reinitializing.
   Ollama callers may inject :host and :executor-fn (the provider's transport
   port); production defaults and other providers are unchanged."
  [& {:keys [provider-type host executor-fn]
      :or {provider-type :ollama}}]
  (log/info "Reinitializing embeddings due to protocol mismatch...")

  (remove-ns 'hive-mcp.embeddings.ollama)
  (remove-ns 'hive-mcp.embeddings.openai)
  (remove-ns 'hive-mcp.embeddings.openrouter)
  (remove-ns 'hive-mcp.embeddings.registry)

  (require 'hive-mcp.embeddings.ollama :reload)
  (require 'hive-mcp.embeddings.openai :reload)
  (require 'hive-mcp.embeddings.openrouter :reload)
  (require 'hive-mcp.embeddings.registry :reload)

  (reset-collection-cache!)
  (emb/reset-embedding-provider!)

  (let [registry-init (resolve 'hive-mcp.embeddings.registry/init!)
        registry-clear (resolve 'hive-mcp.embeddings.registry/clear-cache!)]
    (registry-clear)
    (registry-init))

  (let [config-value (requiring-resolve 'hive-mcp.config.core/get-config-value)
        opts         (cond-> {}
                       (and (= :ollama provider-type) host)
                       (assoc :host host)

                       (and (= :ollama provider-type) executor-fn)
                       (assoc :executor-fn executor-fn)

                       :always
                       (merge (when-let [model (config-value (str "embeddings." (name provider-type) ".model"))]
                                {:model model})))
        provider (case provider-type
                   :ollama ((resolve 'hive-mcp.embeddings.ollama/->provider) opts)
                   :openai ((resolve 'hive-mcp.embeddings.openai/->provider) opts)
                   :openrouter ((resolve 'hive-mcp.embeddings.openrouter/->provider) opts))]
    (emb/set-embedding-provider! provider (when-let [model (:model opts)]
                                            (let [spec {:impl provider-type :model (str model)}]
                                              ;; Carry the provider's resolved :host (OllamaEmbedder
                                              ;; records carry it; other providers lack it) so the
                                              ;; digest resolver asks the right server, not
                                              ;; http://localhost:11434 by default.
                                              (if-let [host (:host provider)]
                                                (assoc spec :host (str host))
                                                spec))))

    (let [fixed? (satisfies? emb/EmbeddingProvider provider)]
      (if fixed?
        (log/info "Embeddings reinitialized successfully")
        (log/error "Reinitialization failed - protocol still mismatched"))
      {:fixed? fixed?
       :provider-type provider-type
       :dimension (when fixed? (emb/embedding-dimension provider))})))

(m/=> reinitialize-embeddings! [:=> [:cat [:* :any]]
                                [:map
                                 [:fixed? :boolean]
                                 [:provider-type [:enum :ollama :openai :openrouter]]
                                 [:dimension [:maybe pos-int?]]]])
