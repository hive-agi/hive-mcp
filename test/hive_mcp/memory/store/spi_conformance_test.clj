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
  (:require [clojure.test :refer [use-fixtures deftest testing is]]
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
            [hive-mcp.test.stub.memory-store :as stub]
            [clojure.string]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.catchup.hierarchy :as hier]
            [hive-spi.memory.ports :as ports]
            [hive-spi.memory.decorate :as decorate]
            [hive-spi.embeddings.ports :as emb-ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private test-config
  "No embedder providers configured: every type falls back to the one global
   (mock) provider and its default collection."
  {:embedder {:no-embed-types #{} :routes {}}})

(defrecord LetterEmbedder []
  ;; A similarity-preserving stub: the unit vector of a text's letter counts
  ;; (a-z, plus one slot for every other character). Texts sharing letters
  ;; land near each other, so a query finds the entry whose embedded text it
  ;; resembles, which a hash embedder cannot show: under one, the Chroma
  ;; document (Type/Tags/Content) is no nearer its own content than any text.
  emb-ports/EmbeddingProvider
  (embed-text [_ text]
    (let [counts (reduce (fn [v ch]
                           (let [i (- (int (Character/toLowerCase (char ch))) (int \a))]
                             (update v (if (<= 0 i 25) i 26) inc)))
                         (vec (repeat 27 0.0))
                         (str text))
          norm   (Math/sqrt (reduce + 1e-9 (map #(* % %) counts)))]
      (mapv #(/ % norm) counts)))
  (embed-batch [this texts]
    (mapv #(emb-ports/embed-text this %) texts))
  (embedding-dimension [_] 27))

(defn chroma-without-server
  "Fixture: a fresh in-memory transport, a similarity-preserving stub
   embedder (LetterEmbedder) and a static config for each test, restoring the
   prior transport and provider afterwards."
  [f]
  (let [prior-transport (client/transport)
        prior-provider  (active/get-embedding-provider)]
    (client/set-transport! (transport/->transport))
    (active/set-embedding-provider! (->LetterEmbedder))
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
  {:connect-config {:host "localhost" :port 8000}
   ;; chroma.crud/query-entries evicts expired entries asynchronously on every
   ;; default read (evict-expired-async!), so a later :include-expired? read
   ;; races the eviction. That is a real divergence from the port, recorded in
   ;; review-spi-mcp-1.md MEDIUM-1; it is skipped by name, not hidden.
   :skip {:query-excludes-expired-by-default
          "chroma evicts expired entries on default reads (evict-expired-async!)"}})

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
           :id-survives [[{:id "a" :content "c"}] ["content"]]
           ;; The catchup metadata projection names Milvus columns; the golden
           ;; pins them onto kebab entry keys.
           :milvus-cols [[{:id "a" :content "c" :type "note" :project-id "p1"
                           :content-hash "h"}]
                         ["id" "type" "project_id" "content_hash"]]}
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

(deftest chroma-catchup-metadata-scan-then-hydrate
  (testing "the catchup metadata projection keeps :project-id and get-entries restores :content"
    (let [s   (chroma-store/create-store)
          _   (mem-proto/reset-store! s)
          id  (mem-proto/add-entry! s {:type :decision :content "the body"
                                       :tags ["t"] :project-id "p1"
                                       :duration :medium})
          row (first (filter #(= id (:id %))
                             (mem-proto/query-entries
                              s {:type "decision" :limit 10
                                 :output-fields hier/metadata-projection})))]
      (is (= "p1" (:project-id row)) "project_id must map to :project-id")
      (is (nil? (:content row)) "the metadata scan carries no content")
      (is (mem-proto/batch-store? s) "hydration needs IMemoryStoreBatch")
      (is (= ["the body"] (mapv :content (mem-proto/get-entries s [id "missing-id"])))))))

(deftrifecta output-field-key
  hive-mcp.memory.store.chroma/field-key
  {:golden-path "test/golden/hive-mcp/memory/chroma-field-key.edn"
   :cases {:kebab        "type"
           :milvus-col   "project_id"
           :two-unders   "unhelpful_count"
           :keyword-in   :content-hash}
   :gen (gen/fmap (fn [parts] (clojure.string/join "_" parts))
                  (gen/not-empty (gen/vector (gen/not-empty gen/string-alphanumeric) 1 3)))
   :pred (fn [f]
           (let [k (chroma-store/field-key f)]
             (and (keyword? k)
                  (not (clojure.string/includes? (name k) "_"))
                  (= (count (name f)) (count (name k))))))
   :num-tests 80
   :mutations [["plain-keyword" (fn [f] (keyword f))]
               ["drops-underscores" (fn [f] (keyword (clojure.string/replace (name f) "_" "")))]]})

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

;; =============================================================================
;; Seal readiness: :embed-text and IMemoryStoreScan beyond the conformance cases
;; =============================================================================

(defn- stored-text
  "Every document and metadata value the in-memory transport holds, as one
   string: what is at rest, whatever spelling put it there."
  []
  (pr-str (for [[_ c] @(:state (client/transport))
                r (vals (:records c))]
            [(:document r) (:metadata r)])))

(deftest chroma-declares-seal-capabilities
  (let [s (chroma-store/create-store)]
    (is (decorate/embed-text-capable? (mem-proto/store-status s)))
    (is (satisfies? ports/IMemoryStoreScan s))))

(deftest chroma-never-persists-embed-text-in-any-spelling
  (let [s  (chroma-store/create-store)
        _  (mem-proto/reset-store! s)
        id (mem-proto/add-entry! s {:type "note" :content {:body "visible" :embed-text "nested-secret"}
                                    :tags ["t"] :duration :medium
                                    :embed-text "top-secret"
                                    "embed-text" "string-secret"
                                    :x/embed-text "ns-secret"})]
    (is (string? id))
    (doseq [secret ["top-secret" "string-secret" "ns-secret" "nested-secret" "embed-text"]]
      (is (not (clojure.string/includes? (stored-text) secret)) secret))
    (is (= "visible" (get-in (mem-proto/get-entry s id) [:content :body])))))

(deftest chroma-update-without-content-reembeds-from-embed-text
  (let [s     (chroma-store/create-store)
        _     (mem-proto/reset-store! s)
        id    (mem-proto/add-entry! s {:type "note" :content "cipher-0" :tags ["t"] :duration :medium
                                       :embed-text "before"})
        other (mem-proto/add-entry! s {:type "note" :content "cipher-1" :tags ["t"] :duration :medium})]
    (is (= id (:id (first (mem-proto/search-similar s "before" {:limit 2})))))
    (let [r (mem-proto/update-entry! s id {:embed-text "after"})]
      (is (= "cipher-0" (:content r)) "content untouched")
      (is (not (contains? r :embed-text))))
    (is (= id (:id (first (mem-proto/search-similar s "after" {:limit 2}))))
        "the vector now comes from the new :embed-text")
    (is (not= other id))
    (is (not (clojure.string/includes? (stored-text) "after")))))

(deftest chroma-scan-ids-pages-past-one-page
  (let [s   (chroma-store/create-store)
        _   (mem-proto/reset-store! s)
        es  (mapv (fn [i] {:id (format "scan-%05d" i) :type "note" :content (str "row " i)
                           :tags ["t"] :duration "medium"})
                  (range 1203))
        _   (crud/index-memory-entries! es)
        ids (ports/scan-ids s {})]
    (is (= 1203 (count ids)))
    (is (= (set (map :id es)) (set ids)))
    (is (= (count ids) (count (distinct ids))))))
