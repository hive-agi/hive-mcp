(ns hive-mcp.memory.store.spi-conformance-test
  "hive-spi.memory.conformance against the IMemoryStore implementations
   hive-mcp core still owns.

   - ChromaMemoryStore, driven through the in-memory IChromaTransport stub, a
     deterministic mock embedder and a static config, so the whole adapter
     (chroma.crud, chroma.search, chroma.maintenance) runs with no server.
   - The in-repo StubMemoryStore, which the rest of the suite injects as a
     stand-in for a real backend and so must honour the same contract.

   The vectordb facade owns no store: it delegates to the registered one, so
   the stores above are what it serves.

   The trifectas at the bottom pin the two pure contracts the adapter fixes
   rest on: :output-fields projection and the staleness field names."
  (:require [clojure.test :refer [use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-spi.memory.conformance :refer [defconformance]]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.chroma.crud :as crud]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.memory.store.chroma :as chroma-store]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-mcp.test.stub.memory-store :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private test-config
  "No embedder providers configured: every type falls back to the one global
   (mock) provider and its default collection."
  {:embedder {:no-embed-types #{} :routes {}}})

(defn chroma-without-server
  "Fixture: a fresh in-memory transport, a mock embedder and a static config
   for each test, restoring the prior transport and provider afterwards."
  [f]
  (let [prior-transport (client/transport)
        prior-provider  (active/get-embedding-provider)]
    (client/set-transport! (transport/->transport))
    (active/set-embedding-provider! (fixtures/->MockEmbedder 16))
    (conn/reset-collection-cache!)
    (try
      (cfg/with-config test-config (f))
      (finally
        (conn/reset-collection-cache!)
        (client/set-transport! prior-transport)
        (active/set-embedding-provider! prior-provider)))))

(use-fixtures :each chroma-without-server)

(defconformance chroma-spi
  #(chroma-store/create-store)
  {:connect-config {:host "localhost" :port 8000}})

(defconformance stub-spi
  #(stub/->stub)
  {:connect-config {}})

;; =============================================================================
;; The adapter fixes, as pure contracts
;; =============================================================================

(defn project-row
  "Unary port for trifecta: [rows output-fields] -> projected rows."
  [[rows fields]]
  (chroma-store/project-fields rows fields))

(def ^:private gen-row
  (gen/hash-map :id (gen/fmap str gen/uuid)
                :content gen/string-alphanumeric
                :type (gen/elements ["note" "decision"])))

(deftrifecta output-fields-projection
  hive-mcp.memory.store.spi-conformance-test/project-row
  {:golden-path "test/golden/hive-mcp/memory/chroma-project-fields.edn"
   :cases {:no-fields   [[{:id "a" :content "c" :type "note"}] nil]
           :type-only   [[{:id "a" :content "c" :type "note"}] ["type"]]
           :id-survives [[{:id "a" :content "c"}] ["content"]]}
   :gen (gen/tuple (gen/vector gen-row 0 4)
                   (gen/elements [nil [] ["type"] ["id" "type"] [:content]]))
   :pred (fn [[rows fields :as input]]
           (let [out (project-row input)]
             (and (= (map :id rows) (map :id out))
                  (if (seq fields)
                    (every? #(every? (conj (set (map keyword fields)) :id) (keys %)) out)
                    (= rows out)))))
   :num-tests 80
   :mutations [["ignores-fields" (fn [[rows _]] rows)]
               ["drops-id" (fn [[rows fields]]
                             (if (seq fields)
                               (mapv #(select-keys % (map keyword fields)) rows)
                               rows))]]})

(defn- port-or-short [opts port short]
  (if (some? (get opts port)) (get opts port) (get opts short)))

(deftrifecta staleness-field-names
  hive-mcp.chroma.crud/staleness-updates
  {:golden-path "test/golden/hive-mcp/memory/chroma-staleness-updates.edn"
   :cases {:port-names  {:staleness-alpha 1 :staleness-beta 9}
           :short-names {:alpha 2 :beta 3 :source :transitive :depth 1}
           :port-wins   {:staleness-beta 9 :beta 1}
           :empty       {}}
   :gen (gen/map (gen/elements [:staleness-alpha :staleness-beta :staleness-depth
                                :alpha :beta :depth])
                 gen/nat)
   :pred (fn [opts]
           (let [out (crud/staleness-updates opts)]
             (and (every? #{:staleness-alpha :staleness-beta :staleness-source :staleness-depth}
                          (keys out))
                  (every? (fn [[port short]] (= (get out port) (port-or-short opts port short)))
                          [[:staleness-alpha :alpha]
                           [:staleness-beta :beta]
                           [:staleness-depth :depth]]))))
   :num-tests 80
   :mutations [["ignores-port-names" (fn [opts]
                                       (cond-> {}
                                         (:alpha opts) (assoc :staleness-alpha (:alpha opts))
                                         (:beta opts)  (assoc :staleness-beta (:beta opts))
                                         (:depth opts) (assoc :staleness-depth (:depth opts))))]
               ["passes-through" (fn [opts] opts)]]})
