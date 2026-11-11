(ns hive-mcp.embeddings.space-stamp-test
  "Collection provenance: a vector collection is stamped with the embedding
   space it was created in, and every write and search through it is refused
   when the provider now emits another space. Unstamped (legacy) collections
   are admitted, never refused.

   Everything runs over stub ports: the in-memory Chroma transport, a mock
   embedder handed out by the registry, an EDN config and a digest resolver
   that answers from an atom. No live store, no Ollama."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.chroma.crud :as crud]
            [hive-mcp.chroma.gate :as gate]
            [hive-mcp.chroma.search :as search]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.embeddings.ollama :as ollama]
            [hive-mcp.embeddings.registry :as registry]
            [hive-mcp.embeddings.service :as service]
            [hive-mcp.embeddings.space :as space]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private dim 8)
(def ^:private coll-name "hive-mcp-memory-8d")

(def ^:private embedder-config
  {:embedder {:default :stub-8 :routes {} :no-embed-types #{}
              :providers {:stub-8 {:impl :ollama :model "stub-embed" :dimension dim
                                   :max-tokens 8192 :host "http://stub.invalid"}}}})

(def ^:private digest (atom "sha256:aaaa"))

(def ^:private contracted
  #{#'space/space->metadata #'space/metadata->space #'space/admit
    #'conn/get-or-create-named-collection #'service/space-for-resolved
    #'ollama/digest-in-tags})

(def ^:private ^:dynamic *transport* nil)

(defn- stub-ports [f]
  (let [prior-t (client/transport)
        prior-p (active/get-embedding-provider)
        t       (transport/->transport)]
    (client/set-transport! t)
    (active/set-embedding-provider! (fixtures/->MockEmbedder dim))
    (conn/reset-collection-cache!)
    (registry/clear-cache!)
    (reset! digest "sha256:aaaa")
    (service/set-digest-resolver! (fn [_spec] @digest))
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try
      (with-redefs [registry/get-provider (fn [c] (fixtures/->MockEmbedder (:dimension c)))]
        (binding [*transport* t]
          (cfg/with-config embedder-config (f))))
      (finally
        (mi/unstrument! {:filters [(mi/-filter-var contracted)]})
        (service/set-digest-resolver! (fn [_] nil))
        (conn/reset-collection-cache!)
        (registry/clear-cache!)
        (client/set-transport! prior-t)
        (if prior-p
          (active/set-embedding-provider! prior-p)
          (active/reset-embedding-provider!))))))

(use-fixtures :each stub-ports)

(defn- space-of [overrides]
  (space/->space (merge {:provider :ollama :model "stub-embed" :digest "sha256:aaaa"
                         :dimension dim :metric :l2 :query-template-rev "raw"
                         :doc-serializer-rev service/doc-serializer-rev}
                        overrides)))

(defn- stored-metadata [name]
  (get-in @(:state *transport*) [name :metadata]))

(defn- thrown-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;; =============================================================================
;; Pure stamp
;; =============================================================================

(deftest stamp-round-trips
  (let [s (space-of {})]
    (testing "the stamp is flat scalar metadata carrying the space id"
      (let [md (space/space->metadata s)]
        (is (= (space/space-id s) (:embed-space-id md)))
        (is (= "ollama" (:embed-provider md)))
        (is (every? (some-fn string? int? boolean?) (vals md)))))
    (testing "read back with keyword or string keys it is the same space"
      (is (= s (space/metadata->space (space/space->metadata s))))
      (is (= s (space/metadata->space (update-keys (space/space->metadata s) name)))))
    (testing "unrecorded fields stay unrecorded"
      (let [bare (space-of {:digest nil :query-template-rev nil})]
        (is (not (contains? (space/space->metadata bare) :embed-digest)))
        (is (= bare (space/metadata->space (space/space->metadata bare))))))
    (testing "metadata without a stamp is legacy: no space"
      (is (nil? (space/metadata->space nil)))
      (is (nil? (space/metadata->space {:dimension 8 :created-by "hive-mcp"}))))))

(deftest admit-verdicts
  (let [s (space-of {})]
    (is (= :stamped (:verdict (space/admit coll-name s (space/space->metadata s)))))
    (is (= :legacy (:verdict (space/admit coll-name s {:dimension 8}))))
    (is (= :legacy (:verdict (space/admit coll-name s nil))))
    (let [data (thrown-data #(space/admit coll-name s
                                          (space/space->metadata (space-of {:digest "sha256:bbbb"}))))]
      (is (= :embedding-space-mismatch (:error data)))
      (is (= coll-name (:collection data)))
      (is (= [:digest] (:differs-on data)))
      (is (= #{:isolate :re-embed} (:remedy data))))
    (testing "a serializer revision change is a different space too"
      (is (= [:doc-serializer-rev]
             (:differs-on (thrown-data #(space/admit coll-name s
                                                     (space/space->metadata
                                                      (space-of {:doc-serializer-rev "old/0"}))))))))))

;; =============================================================================
;; Connection: stamp on create, check on open
;; =============================================================================

(deftest collection-is-stamped-on-create-and-checked-on-open
  (let [s (space-of {})]
    (conn/get-or-create-named-collection coll-name dim s)
    (is (= s (space/metadata->space (stored-metadata coll-name)))
        "the new collection carries its space")
    (is (some? (conn/get-or-create-named-collection coll-name dim s))
        "the same space reopens it")
    (conn/reset-collection-cache!)
    (is (= :embedding-space-mismatch
           (:error (thrown-data #(conn/get-or-create-named-collection
                                  coll-name dim (space-of {:model "other-embed"})))))
        "another space is refused, also after the cache is dropped")
    (is (some? (conn/get-or-create-named-collection coll-name dim))
        "the unguarded arity is unchanged")))

(deftest legacy-collection-is-admitted-not-refused
  (gate/deref-write (client/create-collection coll-name :metadata {:dimension dim}))
  (let [coll (conn/get-or-create-named-collection coll-name dim (space-of {}))]
    (is (= coll-name (:name coll)))
    (is (nil? (space/metadata->space (stored-metadata coll-name)))
        "admitting a legacy collection never rewrites it")))

;; =============================================================================
;; Write and search paths
;; =============================================================================

(deftest write-path-refuses-a-drifted-model
  (let [id (crud/index-memory-entry! {:id "e1" :type "note" :content "alpha"})]
    (is (= "e1" id))
    (is (= "sha256:aaaa" (:digest (space/metadata->space (stored-metadata coll-name))))
        "the first write stamps the collection with the served digest")
    (is (= 1 (transport/record-count *transport*)))
    (testing "the same tag re-pulled with new weights refuses to write"
      (reset! digest "sha256:bbbb")
      (service/set-digest-resolver! (fn [_spec] @digest))
      (conn/reset-collection-cache!)
      (let [data (thrown-data #(crud/index-memory-entry! {:id "e2" :type "note" :content "beta"}))]
        (is (= :embedding-space-mismatch (:error data)))
        (is (= [:digest] (:differs-on data)))
        (is (= coll-name (:collection data))))
      (is (= 1 (transport/record-count *transport*)) "nothing was written"))))

(deftest search-path-refuses-a-drifted-model
  (crud/index-memory-entry! {:id "e1" :type "note" :content "alpha"})
  (is (= ["e1"] (mapv :id (search/search-similar "alpha" :type "note" :limit 3))))
  (reset! digest "sha256:bbbb")
  (service/set-digest-resolver! (fn [_spec] @digest))
  (conn/reset-collection-cache!)
  (is (= :embedding-space-mismatch
         (:error (thrown-data #(search/search-similar "alpha" :type "note" :limit 3))))
      "a query embedded in another space is refused, never answered"))

(deftest legacy-collection-keeps-serving-writes-and-searches
  (gate/deref-write (client/create-collection coll-name :metadata {:dimension dim}))
  (is (= "e1" (crud/index-memory-entry! {:id "e1" :type "note" :content "alpha"})))
  (is (= ["e1"] (mapv :id (search/search-similar "alpha" :type "note" :limit 3)))))

;; =============================================================================
;; Space of a resolved provider; Ollama digest
;; =============================================================================

(deftest space-for-resolved-names-the-provider-space
  (let [resolved (service/resolve-provider-for-type "note")]
    (is (= (space-of {}) (service/space-for-resolved resolved {:metric :l2})))
    (testing "a declared digest wins over the port"
      (cfg/with-config (assoc-in embedder-config [:embedder :providers :stub-8 :digest] "sha256:decl")
        (is (= "sha256:decl" (:digest (service/space-for-resolved resolved))))))
    (testing "the global fallback has no nameable space"
      (is (nil? (service/space-for-resolved {:provider-key :fallback :dimension dim}))))))

(deftest ollama-digest-from-tags
  (let [tags {:models [{:name "qwen3-embedding:4b" :model "qwen3-embedding:4b" :digest "d4b"}
                       {:name "nomic-embed-text:latest" :model "nomic-embed-text:latest" :digest "dn"}]}]
    (is (= "d4b" (ollama/digest-in-tags tags "qwen3-embedding:4b")))
    (is (= "dn" (ollama/digest-in-tags tags "nomic-embed-text")) "bare name = :latest")
    (is (nil? (ollama/digest-in-tags tags "absent")))
    (is (nil? (ollama/digest-in-tags nil "absent")))))
