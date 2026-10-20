(ns hive-mcp.embeddings.value-trifecta-test
  "Schema-derived property, golden and mutation facets for the pure value
   layer of embeddings: cache keys, space identity and role shaping."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.cache :as cache]
            [hive-mcp.embeddings.role :as role]
            [hive-mcp.embeddings.space :as space]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; -----------------------------------------------------------------------------
;; cache-key
;; -----------------------------------------------------------------------------

(defn- sha256
  "Independent hex digest (HexFormat, not the subject's format loop)."
  [^String s]
  (.formatHex (java.util.HexFormat/of)
              (.digest (java.security.MessageDigest/getInstance "SHA-256")
                       (.getBytes s "UTF-8"))))

(defn- expected-key
  "Oracle read off the docstring: collection + optional space + text hash."
  [coll space-id text]
  (str/join ":" (cond-> [(str coll)] space-id (conj space-id) true (conj (sha256 text)))))

(deftrifecta-from-schema cache-key-trifecta
  hive-mcp.embeddings.cache/cache-key
  {:in [:cat [:maybe [:enum "hive-mcp-memory" "hive-ingest"]]
        [:maybe [:enum "s1" "abc"]] :string]
   :out :string
   :rel (fn [[c s t] out] (= out (expected-key c s t)))
   :num-tests 200
   :seed 61
   :n-cases 10
   :golden-path "test/golden/embeddings/cache-key.edn"})

(deftest cache-round-trip-is-space-scoped
  (cache/configure! {:cap 4 :ttl-ms 60000})
  (try
    (is (= [1.0 2.0] (cache/store! "c" "s1" "hello" [1.0 2.0])))
    (is (= [1.0 2.0] (cache/lookup "c" "s1" "hello")))
    (is (nil? (cache/lookup "c" "s2" "hello")) "another space never answers")
    (is (nil? (cache/lookup "c" "hello")) "legacy key is distinct")
    (let [st (cache/stats)]
      (is (m/validate cache/CacheStats st))
      (is (= {:size 1 :hits 1 :misses 2} (select-keys st [:size :hits :misses]))))
    (testing "the LRU cap evicts the eldest"
      (doseq [i (range 6)] (cache/store! "c" (str "t" i) [(double i)]))
      (is (= 4 (:size (cache/stats)))))
    (finally (cache/clear!) (cache/configure! {}))))

(deftest cache-expired-entries-are-evicted-lazily
  (cache/configure! {:cap 4 :ttl-ms -1})
  (try
    (cache/store! "c" "x" [1.0])
    (is (nil? (cache/lookup "c" "x")))
    (is (= 1 (:evicts (cache/stats))))
    (finally (cache/clear!) (cache/configure! {}))))

;; -----------------------------------------------------------------------------
;; space
;; -----------------------------------------------------------------------------

(def ^:private SmallSpace
  "EmbeddingSpace with a narrow vocabulary so two draws collide often."
  [:map
   [:provider [:enum :ollama :openrouter]]
   [:model [:enum "a" "b"]]
   [:digest [:maybe [:enum "d1"]]]
   [:dimension [:enum 8 768]]
   [:metric space/Metric]
   [:normalized? :boolean]
   [:query-template-rev [:maybe [:enum "q1"]]]
   [:doc-serializer-rev [:maybe [:enum "s1"]]]])

(deftrifecta-from-schema differing-keys-trifecta
  hive-mcp.embeddings.space/differing-keys
  {:in [:cat SmallSpace SmallSpace]
   :out [:vector space/SpaceKey]
   :rel (fn [[a b] out]
          (= out (vec (for [k space/space-keys :when (not= (k a) (k b))] k))))
   :num-tests 300
   :seed 67
   :n-cases 10
   :golden-path "test/golden/embeddings/space-differing-keys.edn"})

(deftrifecta-from-schema space-id-trifecta
  hive-mcp.embeddings.space/space-id
  {:in [:cat SmallSpace]
   :out space/SpaceId
   :rel (fn [[s] out] (= out (sha256 (space/canonical s))))
   :num-tests 200
   :seed 71
   :n-cases 8
   :golden-path "test/golden/embeddings/space-id.edn"})

(deftest check-compatible-arms
  (let [s {:provider :ollama :model "a" :dimension 8}]
    (is (= (space/->space s) (space/check-compatible! s s)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"mismatch"
                          (space/check-compatible! s nil)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (space/check-compatible! s (assoc s :model "b"))))
    (is (thrown? clojure.lang.ExceptionInfo (space/->space {:provider :ollama})))
    (is (= (space/->space s)
           (space/config->space {:provider-type :ollama :model "a" :dimension 8})))))

;; -----------------------------------------------------------------------------
;; role
;; -----------------------------------------------------------------------------

(deftrifecta-from-schema shape-query-trifecta
  hive-mcp.embeddings.role/shape-query
  {:in [:cat [:enum nil :raw :qwen3-retrieval-v1] :string]
   :out :string
   :rel (fn [[spec text] out]
          (if (= spec :qwen3-retrieval-v1)
            (= out (str "Instruct: " role/qwen-retrieval-instruction "\nQuery: " text))
            (= out text)))
   :num-tests 200
   :seed 73
   :n-cases 8
   :golden-path "test/golden/embeddings/role-shape-query.edn"})

(deftrifecta-from-schema shape-document-trifecta
  hive-mcp.embeddings.role/shape-document
  {:in [:cat [:enum :raw :content-only :metadata-envelope]
        [:map [:content :string] [:type [:enum "note" "decision"]]
         [:tags [:vector {:max 3} [:enum "a" "b"]]]]]
   :out :string
   :rel (fn [[spec {:keys [content type tags]}] out]
          (if (= spec :metadata-envelope)
            (= out (str "Type: " type "\n"
                        (when (seq tags) (str "Tags: " (str/join ", " tags) "\n"))
                        "Content: " content))
            (= out content)))
   :num-tests 200
   :seed 79
   :n-cases 8
   :golden-path "test/golden/embeddings/role-shape-document.edn"})

(deftest unknown-template-revisions-fail-closed
  (is (thrown? clojure.lang.ExceptionInfo (role/shape-query :nope "x")))
  (is (thrown? clojure.lang.ExceptionInfo (role/shape-document :nope {:content "x"})))
  (is (= "{\"a\":1}" (role/shape-document {:template :raw} {:content {:a 1}}))))
