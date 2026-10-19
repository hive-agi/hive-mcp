(ns hive-mcp.chroma.connection
  "Chroma connection configuration, collection management, and health status."
  (:require [hive-mcp.chroma.client :as chroma]
            [hive-mcp.embeddings.active :as emb]
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
  "Create a new Chroma collection with dimension metadata."
  [coll-name dim]
  (let [result (deref (chroma/create-collection coll-name {:metadata {:dimension dim :created-by "hive-mcp"}})
                      15000 ::timeout)]
    (when (= result ::timeout)
      (throw (ex-info "Chroma create-collection timed out" {:collection coll-name})))
    result))

(defn- cache-collection!
  "Cache and return collection, logging action."
  [coll log-msg]
  (reset! collection-cache coll)
  (log/info log-msg)
  coll)

(defn get-or-create-collection
  "Get existing collection or create new one."
  []
  (or @collection-cache
      (let [coll-name (:collection-name @config)
            _ (emb/require-embedding!)
            dim (emb/embedding-dimension (emb/get-embedding-provider))]
        (if-let [existing (try-get-collection coll-name)]
          (cache-collection! existing (str "Using existing Chroma collection: " coll-name))
          (cache-collection! (create-new-collection coll-name dim)
                             (str "Created Chroma collection: " coll-name " dimension: " dim))))))

(m/=> get-or-create-collection [:=> [:cat] chroma/Coll])

(defonce ^:private named-collection-cache (atom {}))

(defn get-or-create-named-collection
  "Get or create a named collection with explicit dimension.
   Caches per collection-name. Used by type-based embedder routing."
  [coll-name dimension]
  (or (get @named-collection-cache coll-name)
      (let [coll (or (try-get-collection coll-name)
                     (create-new-collection coll-name dimension))]
        (swap! named-collection-cache assoc coll-name coll)
        (log/info "Named collection ready:" coll-name "dimension:" dimension)
        coll)))

(m/=> get-or-create-named-collection [:=> [:cat :string pos-int?] chroma/Coll])

(defn reset-collection-cache!
  "Reset the collection cache (for testing/reconnection)."
  []
  (reset! collection-cache nil)
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
  "Fix hot-reload protocol mismatch by reloading namespaces and reinitializing."
  [& {:keys [provider-type] :or {provider-type :ollama}}]
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
        opts         (if-let [model (config-value (str "embeddings." (name provider-type) ".model"))]
                       {:model model}
                       {})
        provider (case provider-type
                   :ollama ((resolve 'hive-mcp.embeddings.ollama/->provider) opts)
                   :openai ((resolve 'hive-mcp.embeddings.openai/->provider) opts)
                   :openrouter ((resolve 'hive-mcp.embeddings.openrouter/->provider) opts))]
    (emb/set-embedding-provider! provider)

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
