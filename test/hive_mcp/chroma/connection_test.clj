(ns hive-mcp.chroma.connection-test
  "hive-mcp.chroma.connection over INJECTED ports: the in-memory Chroma
   transport and a mock embedding provider. Goldens pin config merging,
   status with and without a provider, and the collection caches."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.embeddings.registry :as registry]
            [hive-mcp.embeddings.service :as service]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- connection-caches []
  {:collection @@#'conn/collection-cache
   :named @@#'conn/named-collection-cache
   :legacy-warned @@#'conn/legacy-warned})

(defn- restore-connection-caches! [caches]
  (reset! @#'conn/collection-cache (:collection caches))
  (reset! @#'conn/named-collection-cache (:named caches))
  (reset! @#'conn/legacy-warned (:legacy-warned caches)))

(defn- own-ports [f]
  (let [prior-t      (client/transport)
        prior-p      (active/get-embedding-provider)
        prior-spec   (active/get-embedding-provider-spec)
        prior-cfg    (conn/get-config)
        prior-caches (connection-caches)
        resolver-fn  @(deref #'service/digest-resolver)]
    (client/set-transport! (transport/->transport))
    (service/set-digest-resolver! (fn [_] nil))
    (conn/reset-collection-cache!)
    (try (f)
         (finally
           (conn/configure! (select-keys prior-cfg [:host :port :collection-name]))
           (restore-connection-caches! prior-caches)
           (registry/clear-cache!)
           (client/set-transport! prior-t)
           (service/set-digest-resolver! resolver-fn)
           (if prior-p
             (active/set-embedding-provider! prior-p prior-spec)
             (active/reset-embedding-provider!))))))

(use-fixtures :each own-ports)

(deftest-golden status-and-caches-golden
  "test/golden/hive-mcp/chroma/connection-status-caches.edn"
  (let [_          (active/reset-embedding-provider!)
        unprovided {:status (conn/status) :available? (conn/chroma-available?)}
        _          (active/set-embedding-provider! (fixtures/->MockEmbedder 8))
        _          (conn/configure! {:host "chroma.test" :port 9000 :collection-name "f04-coll"})
        cfg        (conn/get-config)
        provided   (update (conn/status) :provider some?)
        c1         (conn/get-or-create-collection)
        c2         (conn/get-or-create-collection)
        n1         (conn/get-or-create-named-collection "f04-named" 1024)
        n2         (conn/get-or-create-named-collection "f04-named" 1024)
        avail      (conn/chroma-available?)
        reset      (conn/reset-collection-cache!)]
    {:unprovided unprovided
     :config     (select-keys cfg [:host :port :collection-name])
     :provided   provided
     :coll       (:name c1)
     :cached?    (identical? c1 c2)
     :named      (:name n1)
     :named-cached? (identical? n1 n2)
     :available? avail
     :reset      reset}))

(deftest reset-drops-the-cache
  (active/set-embedding-provider! (fixtures/->MockEmbedder 8))
  (let [c1 (conn/get-or-create-collection)]
    (conn/reset-collection-cache!)
    (is (not (identical? c1 (conn/get-or-create-collection))))))

(deftest provider-spec-describes-the-provider-without-global-state
  (let [host "http://ollama.custom:11434"]
    (testing "a provider with a resolved host carries it into the spec"
      (is (= {:impl :ollama :model "nomic-embed-text" :host host}
             (conn/provider-spec :ollama "nomic-embed-text" {:host host}))))
    (testing "a provider without a host has no :host key"
      (let [spec (conn/provider-spec :openai "text-embedding-3-small" {})]
        (is (= {:impl :openai :model "text-embedding-3-small"} spec))
        (is (not (contains? spec :host)))))
    (testing "a nil model produces no spec"
      (is (nil? (conn/provider-spec :ollama nil {:host host}))))))
