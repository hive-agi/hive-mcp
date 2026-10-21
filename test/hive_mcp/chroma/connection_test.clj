(ns hive-mcp.chroma.connection-test
  "hive-mcp.chroma.connection over INJECTED ports: the in-memory Chroma
   transport and a mock embedding provider. Goldens pin config merging,
   status with and without a provider, and the collection caches."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-ports [f]
  (let [prior-t   (client/transport)
        prior-p   (active/get-embedding-provider)
        prior-cfg (conn/get-config)]
    (client/set-transport! (transport/->transport))
    (conn/reset-collection-cache!)
    (try (f)
         (finally
           (conn/configure! (select-keys prior-cfg [:host :port :collection-name]))
           (conn/reset-collection-cache!)
           (client/set-transport! prior-t)
           (if prior-p
             (active/set-embedding-provider! prior-p)
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
