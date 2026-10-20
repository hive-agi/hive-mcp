(ns hive-mcp.recall.golden-contract-test
  "Contract facets for the golden recall corpus helpers: the tokenizer, the
   binary-bag cosine, the store constructors and the scoped registration."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.protocols.memory :as mem]
            [hive-mcp.recall.golden :as g]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Text
  [:or :nil :int
   [:string {:gen/fmap #(str/join " " %) :gen/schema [:vector {:max 6} [:enum "Quokka" "vestibule" "7731" "--" "JDK-25" "a.b"]]}]])

(deftrifecta-from-schema tokens-are-lowercased-alphanumeric-words
  #'g/tokens
  {:in [:cat Text]
   :out [:set :string]
   :rel (fn [[s] out]
          (and (every? (fn [t] (and (seq t) (every? #(or (Character/isDigit (char %)) (<= (int \a) (int %) (int \z))) t))) out)
               (every? #(str/includes? (str/lower-case (str s)) %) out)))
   :golden-path "test/golden/hive-mcp/recall/golden-tokens.edn"
   :num-tests 100})

(def ^:private Bag [:set {:max 4} [:enum "quokka" "pelican" "7731" "canary" "index"]])

(deftrifecta-from-schema cosine-is-symmetric-and-bounded
  #'g/cosine
  {:in [:cat Bag Bag]
   :out [:double {:min 0.0 :max 1.0}]
   :rel (fn [[a b] out]
          (and (== out (g/cosine b a))
               (if (and (seq a) (= a b)) (== 1.0 out) true)
               (if (or (empty? a) (empty? b) (empty? (set/intersection a b))) (zero? out) (pos? out))))
   :golden-path "test/golden/hive-mcp/recall/golden-cosine.edn"
   :num-tests 200})

(deftrifecta-from-schema golden-store-holds-the-corpus
  #'g/->golden-store
  {:in [:cat [:enum :distance :similarity]]
   :out g/GoldenStore
   :rel (fn [[emit] out] (and (= emit (:emit out)) (= g/populated-count (count (:corpus out)))))
   :num-tests 20})

(golden/deftest-golden store-behaviour-golden
  "test/golden/hive-mcp/recall/golden-store-behaviour.edn"
  (let [project (fn [rows] (mapv #(select-keys % [:id :distance]) rows))]
    {:contract (project (mem/search-similar (g/->golden-store) g/anchor-query {:limit 3}))
     :bugged (project (mem/search-similar (g/->golden-store :similarity) g/anchor-query {:limit 3}))
     :default-emit (= (:emit (g/->golden-store)) :distance)
     :empty (mem/search-similar (g/->empty-store) g/anchor-query {:limit 3})
     :empty-count (:count (mem/store-status (g/->empty-store)))}))

(deftest with-store-restores-what-was-registered
  (let [prior (:default (mem/registered-stores))
        store (g/->empty-store)
        seen (g/with-store* store (fn [] (identical? store (:default (mem/registered-stores)))))]
    (is (true? seen))
    (is (= (nil? prior) (nil? (:default (mem/registered-stores)))))
    (is (= :result (g/with-store* store (constantly :result))))
    (is (thrown? Exception (g/with-store* store #(throw (ex-info "boom" {})))))
    (is (= (nil? prior) (nil? (:default (mem/registered-stores)))) "restored after a throw too")))
