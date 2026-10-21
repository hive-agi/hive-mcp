(ns hive-mcp.protocols.store-metrics-test
  "Storage metering: memory/KG/vector wrappers and embedding timing record
   bounded op keys through a recording stub recorder."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.embeddings.protocol :as ep]
            [hive-mcp.embeddings.shared-gate :as sg]
            [hive-mcp.protocols.kg :as pkg]
            [hive-mcp.protocols.store-metrics :as sm]
            [hive-mcp.protocols.vector :as vp]
            [hive-spi.kg.protocol :as kgp]
            [hive-spi.memory.decorate :as decorate]
            [hive-spi.memory.ports :as ports]
            [hive-spi.memory.stub :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private observed (atom []))

(defn- recording-recorder [k _elapsed-ns outcome]
  (swap! observed conj [k outcome]))

(use-fixtures :each
  (fn [t]
    (reset! observed [])
    (sm/set-recorder! recording-recorder)
    (try (t)
         (finally (sm/rebind!)))))

(deftest label-of-name-strips-port-suffixes
  (is (= :datahike (sm/label-of-name "DatahikeKGStore")))
  (is (= :chroma (sm/label-of-name "ChromaMemoryStore")))
  (is (= :ollama (sm/label-of-name "OllamaEmbedder")))
  (is (= :open-router (sm/label-of-name "OpenRouterProvider")))
  (is (= :store (sm/label-of-name "Store"))))

(deftest memory-store-wrapper-records-and-forwards
  (let [base (stub/create-store)
        w    (sm/wrap-store :memory base)]
    (testing "every satisfied port protocol is still satisfied"
      (doseq [p [ports/IMemoryStore ports/IMemoryStoreBatch]
              :when (satisfies? p base)]
        (is (satisfies? p w))))
    (testing "the decorator chain peels the wrapper to the backend"
      (is (identical? base (decorate/base-store w)))
      (is (identical? base (sm/unwrap w))))
    (ports/add-entry! w {:id "e1" :type "note" :content "c" :tags []})
    (is (= "e1" (:id (ports/get-entry w "e1"))))
    (is (= [[[:store/memory :stub :add-entry!] :ok]
            [[:store/memory :stub :get-entry] :ok]]
           @observed))
    (testing "wrapping twice is identity"
      (is (identical? w (sm/wrap-store :memory w))))))

(deftest kg-wrapper-records-errors-and-rethrows
  (let [boom (reify kgp/IKGStore
               (query [_ _q] (throw (ex-info "boom" {})))
               (query [_ _q _in] :ok))
        w    (sm/wrap-store :kg boom)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom" (kgp/query w 1)))
    (is (= :ok (kgp/query w 1 [])))
    (is (= [[[:store/kg :reify :query] :error]
            [[:store/kg :reify :query] :ok]]
           @observed))))

(deftest kg-facade-is-metered
  (pkg/query (pkg/noop-store) '[:find ?e :where [?e :a]])
  (is (= [[[:store/kg :noop :query] :ok]] @observed)))

(deftest vector-store-installed-metered
  (let [prev (vp/get-store)
        s    (reify vp/IVectorCollectionStore
               (-get-collection [_ n] {:name n}))]
    (try
      (is (identical? s (vp/set-store! s)))
      (is (sm/metered? (vp/get-store)))
      (is (= {:name "c"} (vp/-get-collection (vp/get-store) "c")))
      (is (= [[[:store/vector :reify :-get-collection] :ok]] @observed))
      (finally (if prev (vp/set-store! prev) (vp/clear-store!))))))

(defrecord FakeEmbedder []
  ep/EmbeddingProvider
  (embed-text [_ _t] [0.5])
  (embed-batch [_ ts] (mapv (constantly [0.5]) ts))
  (embedding-dimension [_] 1))

(deftest embedding-calls-are-metered-inside-the-gate
  (let [p (sg/gated-provider (->FakeEmbedder) (sg/new-gate 1 1000))]
    (is (= [0.5] (ep/embed-text p "x")))
    (is (= [[0.5] [0.5]] (ep/embed-batch p ["a" "b"])))
    (is (= [[[:embed/fake :embed-text] :ok]
            [[:embed/fake :embed-batch] :ok]]
           @observed))))

(deftest disabled-metering-is-transparent
  (sm/set-recorder! nil)
  (let [base (stub/create-store)]
    (is (identical? base (sm/wrap-store :memory base)))
    (is (= [0.5] (ep/embed-text (sg/gated-provider (->FakeEmbedder) (sg/new-gate 1 1000)) "x")))
    (is (empty? @observed))))

(deftest a-throwing-recorder-never-fails-the-call
  (sm/set-recorder! (fn [_ _ _] (throw (RuntimeException. "sink down"))))
  (let [w (sm/wrap-store :memory (stub/create-store))]
    (ports/add-entry! w {:id "e2" :type "note" :content "c" :tags []})
    (is (= "e2" (:id (ports/get-entry w "e2"))))))
