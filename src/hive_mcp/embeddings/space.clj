(ns hive-mcp.embeddings.space
  "EmbeddingSpace: the identity of a vector space.

   Two vectors are comparable only when the same model, at the same exact
   digest, was run through the same query template and document serializer and
   then measured the same way. A dimension match is necessary and nowhere near
   sufficient: two models of 768 dimensions share a width and nothing else.

   This namespace is a pure value layer. It performs no I/O: the Ollama digest
   (/api/show), the collection metadata read and the cache wiring all live in
   adapters that hand a plain map in.

   Strata (each calls only downward):

     guard    check-compatible!   refuse to mix two spaces
     identity space-id            stable sha256 of the canonical space
     value    ->space             a normalized, validated EmbeddingSpace
     floor    unknown-space-id    what an unattributed vector set answers to"
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value object
;; =============================================================================

(def Metric
  "Distance metric the index scores with."
  [:enum :cosine :l2 :ip])

(def EmbeddingSpace
  "Everything that makes one vector incomparable with another. A nil
   :digest or revision means 'not recorded', which is a different space from
   any recorded one: absence is never a wildcard."
  [:map
   [:provider           :keyword]
   [:model              :string]
   [:digest             [:maybe :string]]
   [:dimension          [:int {:min 1 :max 16384}]]
   [:metric             Metric]
   [:normalized?        :boolean]
   [:query-template-rev [:maybe :string]]
   [:doc-serializer-rev [:maybe :string]]])

(def space-keys
  "The keys that define a space, in canonical order."
  [:provider :model :digest :dimension :metric :normalized?
   :query-template-rev :doc-serializer-rev])

(def defaults
  "What an unstated field means. Cosine over unit vectors is how every
   provider we ship is queried; the revisions and digest default to nil
   (unrecorded)."
  {:digest             nil
   :metric             :cosine
   :normalized?        false
   :query-template-rev nil
   :doc-serializer-rev nil})

(defn ->space
  "Build an EmbeddingSpace from a partial map, filling `defaults`. Returns the
   normalized map, or throws ex-info (:error :invalid-embedding-space) with the
   malli explanation when a required field is missing or malformed."
  [partial-space]
  (let [space (-> (merge defaults partial-space)
                  (select-keys space-keys))]
    (when-not (m/validate EmbeddingSpace space)
      (throw (ex-info "Invalid EmbeddingSpace"
                      {:error   :invalid-embedding-space
                       :space   space
                       :explain (m/explain EmbeddingSpace space)})))
    space))

;; Input is a partial map: the body fills defaults and throws on what is
;; still missing, so any map is admitted.
(m/=> ->space [:=> [:cat [:maybe [:map-of :keyword :any]]] EmbeddingSpace])

(def SpaceId
  "A space id: 64 lowercase hex characters. Generates from 32 random bytes."
  [:re {:gen/schema [:vector {:min 32 :max 32} [:int {:min 0 :max 255}]]
        :gen/fmap (fn [bs] (apply str (map #(format "%02x" %) bs)))}
   #"^[0-9a-f]{64}$"])

(def SpaceKey
  "One of `space-keys`."
  (into [:enum] space-keys))

(defn config->space
  "The EmbeddingSpace implied by an EmbeddingConfig-shaped map
   ({:provider-type :model :dimension}) plus any `extra` fields the caller
   knows (digest, metric, revisions). Throws like `->space` when the config
   does not carry enough to name a space."
  ([config] (config->space config nil))
  ([config extra]
   (->space (merge {:provider  (:provider-type config)
                    :model     (:model config)
                    :dimension (:dimension config)}
                   extra))))

(m/=> config->space [:function
                     [:=> [:cat [:maybe [:map-of :keyword :any]]] EmbeddingSpace]
                     [:=> [:cat [:maybe [:map-of :keyword :any]] [:maybe [:map-of :keyword :any]]] EmbeddingSpace]])

;; =============================================================================
;; Identity
;; =============================================================================

(defn canonical
  "The canonical string of `space`: keys in fixed order, values via pr-str, so
   the same space prints identically in every JVM and every run."
  [space]
  (let [s (->space space)]
    (pr-str (mapv (fn [k] [k (get s k)]) space-keys))))

(m/=> canonical [:=> [:cat [:map-of :keyword :any]] [:string {:min 1}]])

(defn- sha256-hex
  [^String text]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        bs (.digest md (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" %) bs))))

(defn space-id
  "Stable identifier of a space: the sha256 hex of its canonical form. Equal
   spaces share an id, any differing field changes it."
  [space]
  (sha256-hex (canonical space)))

(m/=> space-id [:=> [:cat [:map-of :keyword :any]] SpaceId])

(def unknown-space-id
  "What a vector set with no recorded provenance answers to. It equals no
   real `space-id`, so legacy data is detected rather than silently trusted."
  "unknown")

;; =============================================================================
;; Guard
;; =============================================================================

(defn differing-keys
  "Keys on which spaces `a` and `b` differ, in canonical order."
  [a b]
  (let [a (->space a) b (->space b)]
    (filterv #(not= (get a %) (get b %)) space-keys)))

(m/=> differing-keys [:=> [:cat [:map-of :keyword :any] [:map-of :keyword :any]]
                      [:vector SpaceKey]])

(defn compatible?
  "True when `a` and `b` are the same space."
  [a b]
  (empty? (differing-keys a b)))

(m/=> compatible? [:=> [:cat [:map-of :keyword :any] [:map-of :keyword :any]] :boolean])

(defn check-compatible!
  "Return `expected` when `actual` is the same space. `actual` nil means the
   existing vectors carry no provenance; that is refused too, because a
   collection nobody can vouch for is exactly the case this guards. Throws
   ex-info (:error :embedding-space-mismatch) naming both spaces, both ids and
   the differing keys, with the remedy: isolate in a new collection or
   re-embed."
  [expected actual]
  (let [expected (->space expected)
        actual   (some-> actual ->space)
        diff     (if actual (differing-keys expected actual) space-keys)]
    (if (and actual (empty? diff))
      expected
      (throw (ex-info (str "Embedding space mismatch: collection holds "
                           (if actual (space-id actual) unknown-space-id)
                           " but the provider emits " (space-id expected)
                           (when actual (str " (differs on " (pr-str diff) ")"))
                           ". Isolate into a new collection or re-embed.")
                      {:error        :embedding-space-mismatch
                       :expected     expected
                       :expected-id  (space-id expected)
                       :actual       actual
                       :actual-id    (if actual (space-id actual) unknown-space-id)
                       :differs-on   diff
                       :remedy       #{:isolate :re-embed}})))))

(m/=> check-compatible! [:=> [:cat [:map-of :keyword :any] [:maybe [:map-of :keyword :any]]]
                         EmbeddingSpace])

;; =============================================================================
;; Collection stamp
;; =============================================================================
;;
;; A vector store keeps the space a collection was created in beside it, as
;; flat scalar metadata (Chroma and Milvus both refuse nested values and nil).
;; Keywords are written by name and read back as keywords, so the stamp
;; survives a JSON round trip unchanged.

(def stamp-keys
  "Collection-metadata key of each space field. :embed-space-id marks a
   collection as stamped; its absence is what makes a collection legacy."
  {:provider           :embed-provider
   :model              :embed-model
   :digest             :embed-digest
   :dimension          :embed-dimension
   :metric             :embed-metric
   :normalized?        :embed-normalized
   :query-template-rev :embed-query-template-rev
   :doc-serializer-rev :embed-doc-serializer-rev})

(def StampMetadata
  "The stamp as collection metadata: scalar values only, nil fields dropped."
  [:map-of :keyword [:or :string :int :boolean]])

(defn space->metadata
  "The metadata that stamps a collection with `space`: every recorded field
   under its `stamp-keys` name plus :embed-space-id. Unrecorded (nil) fields
   are left out; reading the stamp back yields nil for them again."
  [space]
  (let [s (->space space)]
    (into {:embed-space-id (space-id s)}
          (keep (fn [k]
                  (let [v (get s k)]
                    (when (some? v)
                      [(stamp-keys k) (if (keyword? v) (name v) v)]))))
          space-keys)))

(m/=> space->metadata [:=> [:cat [:map-of :keyword :any]] StampMetadata])

(defn- meta-get
  "Value of keyword K in collection metadata MD, whose keys may come back as
   keywords or strings depending on the transport."
  [md k]
  (let [v (get md k ::absent)]
    (if (= ::absent v) (get md (name k)) v)))

(defn metadata->space
  "The space a collection was stamped with, read from its metadata, or nil
   when the metadata carries no stamp (a legacy collection). Throws like
   `->space` when a stamp is present but malformed."
  [md]
  (when (and (map? md) (some? (meta-get md :embed-space-id)))
    (let [raw (into {} (map (fn [[k mk]] [k (meta-get md mk)])) stamp-keys)]
      (->space (cond-> raw
                 (some? (:provider raw))      (update :provider keyword)
                 (some? (:metric raw))        (update :metric keyword)
                 (some? (:dimension raw))     (update :dimension long)
                 (string? (:normalized? raw)) (update :normalized? #(= "true" %))
                 (nil? (:metric raw))         (dissoc :metric)
                 (nil? (:normalized? raw))    (dissoc :normalized?))))))

(m/=> metadata->space [:=> [:cat [:maybe [:map-of :any :any]]] [:maybe EmbeddingSpace]])

(def Admission
  "Verdict of `admit`: the collection holds `expected`'s space (:stamped), or
   carries no stamp at all (:legacy, read with a warning, never refused)."
  [:map
   [:verdict [:enum :stamped :legacy]]
   [:collection :string]
   [:space EmbeddingSpace]
   [:space-id SpaceId]])

(defn admit
  "Pure guard for one collection. `expected` is the space the caller is about
   to write or query with; `md` is the collection's metadata.

     stamped, same space  => {:verdict :stamped ...}
     no stamp             => {:verdict :legacy ...}   caller warns, proceeds
     stamped, other space => throws ex-info :embedding-space-mismatch
                             (see `check-compatible!`) carrying :collection

   A legacy collection is admitted because refusing it would take every
   pre-stamp collection offline; it is reported, not trusted."
  [collection expected md]
  (let [expected (->space expected)
        stamped  (metadata->space md)
        base     {:collection collection
                  :space      expected
                  :space-id   (space-id expected)}]
    (if (nil? stamped)
      (assoc base :verdict :legacy)
      (try
        (check-compatible! expected stamped)
        (assoc base :verdict :stamped)
        (catch clojure.lang.ExceptionInfo e
          (throw (ex-info (str "Collection " collection ": " (ex-message e))
                          (assoc (ex-data e) :collection collection)
                          e)))))))

(m/=> admit [:=> [:cat :string [:map-of :keyword :any] [:maybe [:map-of :any :any]]]
             Admission])
