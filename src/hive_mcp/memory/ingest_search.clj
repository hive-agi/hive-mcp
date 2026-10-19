(ns hive-mcp.memory.ingest-search
  (:require [malli.core :as m]))

(declare resolve-ingest-search normalize-ingest-results merge-and-rerank)

(defn resolve-ingest-search
  "Runtime-resolve ingest cross-collection search. Zero compile-time coupling.

   Neutralized after the Chroma retirement: the only implementation this ever
   resolved, `hive-ingestor.storage.chroma/search-across-collections!`, was
   deleted from hive-ingestor (its storage/chroma_retired_test pins the
   absence). Resolving it on every federated search only paid a classpath
   lookup to throw and land in nil. It now answers nil directly, so
   `search-federated` returns the default-collection results alone. Kept as
   the seam a future ingest search port plugs into; callers already treat nil
   as \"no ingest search\"."
  []
  nil)

(m/=> resolve-ingest-search [:=> [:cat] :nil])

(defn normalize-ingest-results
  "Normalize ingest search results to match search-similar output shape."
  [raw-results]
  (when (sequential? raw-results)
    (->> raw-results
         (mapcat (fn [coll-result]
                   (let [coll-name (:collection coll-result)
                         ids       (:ids coll-result)
                         docs      (:documents coll-result)
                         metas     (:metadatas coll-result)
                         dists     (:distances coll-result)]
                     (when (and ids docs)
                       (map (fn [id doc meta dist]
                              {:id id
                               :document doc
                               :metadata (or meta {})
                               :distance (or dist 999.0)
                               :collection coll-name})
                            (first ids) (first docs) (first metas) (first dists))))))
         (remove nil?)
         vec)))

(m/=> normalize-ingest-results
      [:=> [:cat [:maybe [:sequential :map]]]
       [:maybe [:vector [:map [:id :any] [:document :any]
                              [:metadata :map] [:distance :double]
                              [:collection [:maybe :string]]]]]])

(defn merge-and-rerank
  "Merge two result sequences, deduplicate by :id keeping closest distance, sort ascending.
   Pure function — no IO."
  [results-a results-b limit]
  (->> (concat results-a results-b)
       (reduce (fn [acc entry]
                 (let [id (:id entry)]
                   (if (contains? acc id)
                     (let [existing-dist (or (:distance (get acc id)) 999.0)
                           new-dist      (or (:distance entry) 999.0)]
                       (if (< new-dist existing-dist)
                         (assoc acc id entry)
                         acc))
                     (assoc acc id entry))))
               {})
       vals
       (sort-by #(or (:distance %) 999.0))
       (take limit)
       vec))

(m/=> merge-and-rerank [:=> [:cat [:sequential [:map [:id :string] [:distance {:optional true} :double]]] [:sequential [:map [:id :string] [:distance {:optional true} :double]]] [:int {:min 0}]] [:vector :map]])