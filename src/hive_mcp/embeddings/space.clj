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

;; =============================================================================
;; Identity
;; =============================================================================

(defn canonical
  "The canonical string of `space`: keys in fixed order, values via pr-str, so
   the same space prints identically in every JVM and every run."
  [space]
  (let [s (->space space)]
    (pr-str (mapv (fn [k] [k (get s k)]) space-keys))))

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

(defn compatible?
  "True when `a` and `b` are the same space."
  [a b]
  (empty? (differing-keys a b)))

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
