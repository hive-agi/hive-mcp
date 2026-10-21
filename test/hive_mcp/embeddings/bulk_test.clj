(ns hive-mcp.embeddings.bulk-test
  "Facets for hive-mcp.embeddings.bulk: the chunk size read from config, and
   bulk embedding through an injected EmbeddingProvider (no provider IO, no
   with-redefs)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.embeddings.bulk :as bulk]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.embeddings.ports :as ports]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; max-texts-from-config (pure)
;; =============================================================================

(def ^:private MaxTexts
  "What a config may hold at [:embeddings :batch :max-texts]: a usable
   positive int, or something the reader must fall back from."
  [:or pos-int? [:int {:max 0}] :string :nil])

(defn- cfg-of
  "A config for one generated [shape n] pair: the configured branch holds a
   positive int; every other shape is a value or a gap the reader must fall
   back from."
  [[shape n]]
  (case shape
    :configured {:embeddings {:batch {:max-texts n}}}
    :non-positive {:embeddings {:batch {:max-texts (- 1 n)}}}
    :string {:embeddings {:batch {:max-texts (str n)}}}
    :nil {:embeddings {:batch {:max-texts nil}}}
    :no-batch {:embeddings {}}
    :empty {}))

(def ^:private Cfg
  "A config map whose [:embeddings :batch :max-texts] holds a positive int
   about half the time, so both the configured and the default branch are
   reached. The generator picks the branch first (correlated generation); the
   contract's input is :any, so this schema only sizes the generator."
  [:and {:gen/schema [:tuple [:enum :configured :configured :configured :configured :configured
                              :non-positive :string :nil :no-batch :empty]
                      [:int {:min 1 :max 64}]]
         :gen/fmap cfg-of}
   [:map
    [:embeddings {:optional true}
     [:map [:batch {:optional true} [:map [:max-texts {:optional true} MaxTexts]]]]]]])

(defn- configured [cfg] (get-in cfg [:embeddings :batch :max-texts]))

(deftrifecta-from-schema max-texts hive-mcp.embeddings.bulk/max-texts-from-config
  {:in Cfg
   :out pos-int?
   :rel (fn [cfg out]
          (let [n (configured cfg)]
            (= out (if (pos-int? n) n bulk/default-max-texts))))
   :classify (fn [cfg _] (if (pos-int? (configured cfg)) :configured :default))
   :classify-domain #{:configured :default}
   :golden-path "test/golden/hive-mcp/embeddings/bulk-max-texts.edn"
   :num-tests 200})

;; =============================================================================
;; embed-batch! (provider port injected)
;; =============================================================================

(def ^:private EmbedArgs
  "Arguments with an explicit chunk size, so the global config is never read."
  [:cat proto/Provider [:vector {:gen/max 30} :string]
   [:map [:max-texts [:int {:min 1 :max 10}]]]])

(deftrifecta-from-schema embed-batch hive-mcp.embeddings.bulk/embed-batch!
  {:in EmbedArgs
   :out [:vector proto/Vector]
   :rel (fn [[provider texts _] out]
          (and (= (count texts) (count out))
               (every? #(= (ports/embedding-dimension provider) (count %)) out)))
   :num-tests 100})

(defrecord RecordingProvider [calls]
  ports/EmbeddingProvider
  (embed-text [_ text] [(double (count text))])
  (embed-batch [_ texts]
    (swap! calls conj (count texts))
    (mapv (fn [t] [(double (count t))]) texts))
  (embedding-dimension [_] 1))

(defn- chunking
  "Provider calls (texts per call) and the answer for `n` texts at `max-texts`."
  [n max-texts]
  (let [calls (atom [])
        texts (mapv #(apply str (repeat % "x")) (range n))
        out (bulk/embed-batch! (->RecordingProvider calls) texts {:max-texts max-texts})]
    {:calls @calls :out out}))

(golden/deftest-golden embed-batch-chunks-golden
  "test/golden/hive-mcp/embeddings/bulk-chunking.edn"
  (into (sorted-map)
        (for [[n k] [[0 8] [5 8] [20 8] [7 3] [9 3]]]
          [(str n "@" k) (chunking n k)])))

(deftest one-vector-per-text-in-order
  (let [{:keys [calls out]} (chunking 20 8)]
    (is (= [8 8 4] calls) "no provider call carries more than max-texts texts")
    (is (= (mapv #(vector (double %)) (range 20)) out))))
