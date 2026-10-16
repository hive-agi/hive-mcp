(ns hive-mcp.embeddings.space-test
  "EmbeddingSpace identity, the compatibility guard, and space-scoped cache
   keys. Pure: no provider, no network, no collection."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.embeddings.cache :as cache]
            [hive-mcp.embeddings.space :as space]
            [hive-test.trifecta :refer [deftrifecta]]))

(def ^:private nomic
  {:provider :ollama :model "nomic-embed-text" :dimension 768
   :digest "sha256:0a109f422b47" :metric :cosine :normalized? true
   :query-template-rev "q1" :doc-serializer-rev "d1"})

(defn characterize-space
  "Identity facts about a space: the id is a 64-char hex string, stable across
   calls, and moves with every defining field."
  [scenario]
  (let [id (space/space-id nomic)]
    (case scenario
      :stable      {:hex-64 (and (= 64 (count id)) (every? (set "0123456789abcdef") id))
                    :repeatable (= id (space/space-id nomic))
                    :key-order-free (= id (space/space-id (into (sorted-map) nomic)))}
      :digest      {:changes (not= id (space/space-id (assoc nomic :digest "sha256:other")))}
      :serializer  {:changes (not= id (space/space-id (assoc nomic :doc-serializer-rev "d2")))}
      :query       {:changes (not= id (space/space-id (assoc nomic :query-template-rev "q2")))}
      :dimension   {:changes (not= id (space/space-id (assoc nomic :dimension 1024)))}
      :metric      {:changes (not= id (space/space-id (assoc nomic :metric :l2)))}
      :normalized  {:changes (not= id (space/space-id (assoc nomic :normalized? false)))}
      :model       {:changes (not= id (space/space-id (assoc nomic :model "mxbai-embed-large")))}
      :provider    {:changes (not= id (space/space-id (assoc nomic :provider :openai)))}
      :unrecorded  {:changes (not= id (space/space-id (assoc nomic :digest nil)))
                    :not-unknown (not= id space/unknown-space-id)})))

(deftrifecta embedding-space-identity
  hive-mcp.embeddings.space-test/characterize-space
  {:golden-path "test/golden/embeddings/embedding-space-identity.edn"
   :cases {:stable :stable :digest :digest :serializer :serializer
           :query :query :dimension :dimension :metric :metric
           :normalized :normalized :model :model :provider :provider
           :unrecorded :unrecorded}
   :gen (gen/elements [:stable :digest :serializer :query :dimension :metric
                       :normalized :model :provider :unrecorded])
   :pred map?
   :num-tests 40
   :mutations [["ignores-digest" (fn [_] {:changes false})]
               ["empty" (fn [_] {})]]})

(defn characterize-guard
  "The compatibility guard's verdict for a pair of spaces, reduced to data."
  [scenario]
  (let [verdict (fn [expected actual]
                  (try {:ok (space/check-compatible! expected actual)}
                       (catch clojure.lang.ExceptionInfo e
                         (let [d (ex-data e)]
                           {:error (:error d)
                            :differs-on (:differs-on d)
                            :remedy (:remedy d)
                            :names-both (boolean (and (:expected-id d) (:actual-id d)))}))))]
    (case scenario
      :same        {:same (= (space/->space nomic) (:ok (verdict nomic nomic)))}
      :new-digest  (verdict nomic (assoc nomic :digest "sha256:new"))
      :new-serial  (verdict nomic (assoc nomic :doc-serializer-rev "d2"))
      :legacy      (verdict nomic nil)
      :wide        (verdict nomic (assoc nomic :dimension 1024 :model "mxbai-embed-large")))))

(deftrifecta embedding-space-guard
  hive-mcp.embeddings.space-test/characterize-guard
  {:golden-path "test/golden/embeddings/embedding-space-guard.edn"
   :cases {:same :same :new-digest :new-digest :new-serial :new-serial
           :legacy :legacy :wide :wide}
   :gen (gen/elements [:same :new-digest :new-serial :legacy :wide])
   :pred map?
   :num-tests 30
   :mutations [["always-accepts" (fn [_] {:same true})]
               ["empty" (fn [_] {})]]})

(deftest cache-key-scopes-by-space
  (testing "no space id keeps the legacy key"
    (is (= (cache/cache-key "c" "t") (cache/cache-key "c" nil "t"))))
  (testing "different spaces never share a key for the same text"
    (let [a (space/space-id nomic)
          b (space/space-id (assoc nomic :digest "sha256:new"))]
      (is (not= (cache/cache-key "c" a "t") (cache/cache-key "c" b "t")))
      (is (not= (cache/cache-key "c" a "t") (cache/cache-key "c" "t")))))
  (testing "a vector stored under one space is invisible to another"
    (cache/clear!)
    (let [a (space/space-id nomic)
          b (space/space-id (assoc nomic :doc-serializer-rev "d2"))]
      (cache/store! "c" a "text" [1.0 2.0])
      (is (= [1.0 2.0] (cache/lookup "c" a "text")))
      (is (nil? (cache/lookup "c" b "text")))
      (is (nil? (cache/lookup "c" "text"))))
    (cache/clear!)))

(deftest invalid-space-is-refused
  (is (= :invalid-embedding-space
         (:error (ex-data (try (space/->space {:provider :ollama})
                               (catch clojure.lang.ExceptionInfo e e))))))
  (testing "config->space maps an EmbeddingConfig-shaped map"
    (is (= "nomic-embed-text"
           (:model (space/config->space {:provider-type :ollama
                                         :model "nomic-embed-text"
                                         :dimension 768}))))))
