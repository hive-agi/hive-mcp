(ns hive-mcp.memory.ingest-search-test
  "After the Chroma retirement there is no ingest cross-collection search to
   resolve, so a federated merge is exactly the default results, re-ranked."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.memory.ingest-search :as ingest-search]))

(deftest resolver-answers-nil-without-chroma
  (is (nil? (ingest-search/resolve-ingest-search))))

(defn federated-merge
  "Unary adapter: merge `default-results` with whatever the resolved ingest
   search contributes (nothing, now), the same way chroma.search/search-federated
   composes them."
  [[default-results limit]]
  (let [ingest-results (when-let [search-fn (ingest-search/resolve-ingest-search)]
                         (ingest-search/normalize-ingest-results
                          (:ok (search-fn "q" {:limit limit}))))]
    (ingest-search/merge-and-rerank default-results ingest-results limit)))

(def ^:private gen-entry
  (gen/hash-map :id (gen/elements ["a" "b" "c" "d"])
                :distance (gen/double* {:min 0.0 :max 2.0 :NaN? false :infinite? false})))

(deftrifecta federated-merge-without-ingest
  hive-mcp.memory.ingest-search-test/federated-merge
  {:golden-path "test/golden/hive-mcp/memory/federated-merge-without-ingest.edn"
   :cases {:empty     [[] 10]
           :one       [[{:id "a" :distance 0.4}] 10]
           :dedup     [[{:id "a" :distance 0.9} {:id "a" :distance 0.2} {:id "b" :distance 0.5}] 10]
           :truncated [[{:id "a" :distance 0.3} {:id "b" :distance 0.1} {:id "c" :distance 0.2}] 2]}
   :gen (gen/tuple (gen/vector gen-entry 0 8) (gen/choose 1 5))
   :pred (fn [out] (and (vector? out)
                        (= (count out) (count (distinct (map :id out))))
                        (apply <= (map :distance out))))
   :num-tests 60
   :mutations [["keeps-duplicates" (fn [[rs limit]] (vec (take limit (sort-by :distance rs))))]
               ["unsorted" (fn [[rs limit]] (vec (take limit (vals (reduce #(assoc %1 (:id %2) %2) {} (reverse rs))))))]]})
