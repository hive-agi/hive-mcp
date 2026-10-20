(ns hive-mcp.chroma.seam-contract-test
  "The Chroma seam namespaces under instrumentation: the transport setter,
   the connection config/status/cache, the read/write gates and the
   IVectorCollectionStore adapter, all over the in-memory transport."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.chroma.gate :as gate]
            [hive-mcp.chroma.vector-store :as vs]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.protocols.vector :as vp]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'client/set-transport! #'client/transport #'client/get-collection
    #'client/create-collection #'client/add #'client/get #'client/query
    #'client/delete #'client/update #'client/delete-collection
    #'conn/get-config #'conn/get-or-create-collection
    #'conn/get-or-create-named-collection #'conn/reset-collection-cache!
    #'conn/status #'conn/chroma-available?
    #'gate/deref-read #'gate/deref-write #'gate/gate-stats
    #'vs/chroma-vector-store})

(defn- chroma-without-server [f]
  (let [prior-transport (client/transport)
        prior-provider  (active/get-embedding-provider)]
    (client/set-transport! (transport/->transport))
    (active/set-embedding-provider! (fixtures/->MockEmbedder 16))
    (conn/reset-collection-cache!)
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try
      (cfg/with-config {:embedder {:no-embed-types #{} :routes {}}} (f))
      (finally
        (mi/unstrument! {:filters [(mi/-filter-var contracted)]})
        (conn/reset-collection-cache!)
        (client/set-transport! prior-transport)
        (active/set-embedding-provider! prior-provider)))))

(use-fixtures :each chroma-without-server)

(deftest seam-schemas-generate
  (doseq [s [client/Transport client/Coll client/Records conn/Config conn/Status
             gate/Pending gate/GateStats vs/VectorStore]]
    (is (seq (mg/sample s {:size 3})))))

(deftest-golden connection-status
  "test/golden/chroma/connection-status.edn"
  (let [cfg0 (conn/get-config)]
    {:config-keys (sort (keys cfg0))
     :status      (conn/status)
     :available?  (conn/chroma-available?)
     :coll        (:name (conn/get-or-create-collection))
     :named       (:name (conn/get-or-create-named-collection "hive-mcp-memory-1024d" 1024))
     :reset       (conn/reset-collection-cache!)}))

(deftest gates-pass-values-through
  (is (= 7 (gate/deref-read (doto (promise) (deliver 7)))))
  (is (= 7 (gate/deref-read (doto (promise) (deliver 7)) 1000)))
  (is (= :w (gate/deref-write (doto (promise) (deliver :w)))))
  (is (m/validate gate/GateStats (gate/gate-stats))))

(deftest-golden transport-round-trip
  "test/golden/chroma/transport-round-trip.edn"
  (let [coll (gate/deref-write (client/create-collection "c1" :metadata {:d 2}))]
    (gate/deref-write (client/add coll [{:id "a" :embedding [0.0 1.0] :document "A" :metadata {:k :v}}
                                        {:id "b" :embedding [1.0 0.0] :document "B" :metadata {:k "w"}}]))
    (gate/deref-write (client/update coll [{:id "a" :document "A2"}]))
    (let [got    (gate/deref-read (client/get coll :ids ["a"]))
          hits   (gate/deref-read (client/query coll [0.0 1.0] :num-results 2))
          _      (gate/deref-write (client/delete coll :ids ["b"]))
          left   (gate/deref-read (client/get coll))
          known  (gate/deref-read (client/get-collection "c1"))
          _      (gate/deref-write (client/delete-collection coll))
          gone   (gate/deref-read (client/get-collection "c1"))]
      {:got (mapv (juxt :id :document :metadata) got)
       :hits (mapv :id hits)
       :left (mapv :id left)
       :known (:name known)
       :gone gone})))

(deftest vector-store-adapter
  (let [store (vs/chroma-vector-store)]
    (is (satisfies? vp/IVectorCollectionStore store))
    (is (identical? (client/transport) (:transport store)))
    (let [t (transport/->transport)]
      (is (identical? t (:transport (vs/chroma-vector-store t)))))
    (testing "the adapter answers nearest first through the port"
      (let [c (vp/-create-collection store "vs" {:get-or-create? true})]
        (vp/-add store c [{:id "x" :embedding [0.0 0.0]} {:id "y" :embedding [3.0 4.0]}] {})
        (is (= ["x" "y"] (mapv :id (vp/-query store c [0.0 0.0] {:n-results 2}))))))))

(deftest set-transport-answers-what-it-installed
  (let [t (transport/->transport)]
    (is (identical? t (client/set-transport! t)))
    (is (identical? t (client/transport)))))
