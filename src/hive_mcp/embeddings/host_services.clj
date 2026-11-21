(ns hive-mcp.embeddings.host-services
  "This host's side of `hive-spi.vector.host-services`: publish the embedding,
   routing, gate, config, ingest-search and named-collection services a
   vector-store backend addon (hive-chroma, ...) consumes, so the addon calls
   through the published SPI instead of resolving hive-mcp symbols by name.

   Registered as a boot contribution (resources/hive-mcp/boot-contributions.edn)
   ahead of the :embeddings step, so every backend's initialize! finds them.
   Each capability is the host VAR, so a reloaded namespace is seen by the
   next call."
  (:require [hive-mcp.config.core :as config]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.embeddings.role :as role]
            [hive-mcp.embeddings.service :as service]
            [hive-mcp.embeddings.shared-gate :as shared]
            [hive-mcp.memory.ingest-search :as ingest-search]
            [hive-mcp.protocols.vector :as vector]
            [hive-spi.vector.host-services :as hs]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn capabilities
  "The capability map this host publishes, keyed as in
   `hive-spi.vector.host-services/catalog`."
  []
  {:embedding/require!             #'active/require-embedding!
   :embedding/configured?          #'active/embedding-configured?
   :embedding/provider             #'active/get-embedding-provider
   :embedding/set-provider!        #'active/set-embedding-provider!
   :embedding/reset-provider!      #'active/reset-embedding-provider!
   :embedding/dimension            #'active/embedding-dimension
   :embedding/embed-text           #'active/embed-text
   :embedding/embed-batch          #'active/embed-batch
   :embedding/provider-for         #'active/get-provider-for
   :embedding/embed-text-for       #'active/embed-text-for
   :embedding/embed-batch-for      #'active/embed-batch-for
   :embedding/dimension-for        #'active/get-dimension-for
   :routing/no-embed-type?         #'service/no-embed-type?
   :routing/resolve-for-type       #'service/resolve-provider-for-type
   :routing/resolve-for-type+size  #'service/resolve-provider-for-type+size
   :routing/validate-content-size! #'service/validate-content-size!
   :routing/space-for-resolved     #'service/space-for-resolved
   :routing/default-space          #'service/default-space
   :routing/sibling-failover       #'service/sibling-failover
   :routing/type->collection-names #'service/type->collection-names
   :routing/all-collection-names   #'service/all-collection-names
   :query/shape                    #'role/shape-query
   :config/get-in                  #'config/get-in-config
   :ingest/resolve-search          #'ingest-search/resolve-ingest-search
   :ingest/normalize-results       #'ingest-search/normalize-ingest-results
   :ingest/merge-and-rerank        #'ingest-search/merge-and-rerank
   :gate/with-permit               #'shared/with-permit
   :gate/process-gate              (fn [] shared/process-gate)
   :gate/lane                      (fn [] shared/*lane*)
   :vector/port                    (fn [] @#'vector/IVectorCollectionStore)
   :vector/store-set?              #'vector/store-set?
   :vector/set-store!              #'vector/set-store!})

(defn register!
  "Publish `capabilities` through hive-spi. Idempotent. Returns the set of
   capability keys now registered."
  []
  (set (keys (hs/register-services! (capabilities)))))

(m/=> capabilities [:=> [:cat] [:map-of :keyword ifn?]])
(m/=> register! [:=> [:cat] [:set :keyword]])
