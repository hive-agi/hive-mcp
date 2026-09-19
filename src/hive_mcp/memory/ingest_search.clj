(ns hive-mcp.memory.ingest-search
  (:require [hive-dsl.result :as r]))

(declare resolve-ingest-search normalize-ingest-results merge-and-rerank)

(defn resolve-ingest-search
  "Runtime-resolve ingest cross-collection search. Zero compile-time coupling."
  []
  (r/guard Exception nil
    (requiring-resolve 'hive-ingestor.storage.chroma/search-across-collections!)))

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