(ns hive-mcp.vectordb.memory-store
  "An in-memory `IVectorCollectionStore`, for tests.

   This is what the port buys: plan.plans and presets.core used to need Chroma
   listening on localhost:8000 before a single one of their tests could run.
   They now depend on a seam, and this fills it.

   It is deliberately strict where a forgiving fake would hide a regression:

   - `-query` returns records NEAREST FIRST carrying an ASCENDING `:distance`.
     A backend that passed a cosine SIMILARITY through the same key would order
     every result backwards, and a fake returning insertion order would agree
     with both spellings, so the bug would live in production and the suite
     would stay green.
   - a dimension mismatch THROWS rather than padding, because that is how a
     wrong embedding provider announces itself.

   Temporary in this repo: hive-spi 1.2.0 ships `hive-spi.vector.memory`, the
   same implementation as the port's reference. When the pin moves, delete this
   and require that."
  (:require [hive-mcp.protocols.vector :as vp]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- l2-distance [a b]
  (when (not= (count a) (count b))
    (throw (ex-info "Embedding dimension mismatch"
                    {:reason ::dimension-mismatch
                     :expected (count a) :actual (count b)})))
  (Math/sqrt (reduce + 0.0 (map (fn [x y] (let [d (- x y)] (* d d))) a b))))

(defn- matches-where? [where record]
  (or (nil? where)
      (every? (fn [[k v]] (= v (get (:metadata record) k))) where)))

(defn- select-records [records {:keys [ids where limit]}]
  (cond->> (vals records)
    (seq ids) (filter #(contains? (set ids) (:id %)))
    where     (filter #(matches-where? where %))
    limit     (take limit)))

(defn- coll-key
  "Callers pass back whatever handle they were given. Accept both the handle
   and a bare name, so a test that hardcodes a name still works."
  [coll]
  (if (map? coll) (:name coll) coll))

(defn- handle
  "The handle for COLL-NAME. It carries :metadata, which is the part callers
   depend on: `get-or-create-collection` reads the embedding dimension from it,
   and a handle without it would recreate the collection on every read."
  [state coll-name]
  (when-let [entry (get @state coll-name)]
    {:name coll-name :metadata (:metadata entry)}))

(defrecord InMemoryVectorStore [state]
  vp/IVectorCollectionStore
  (-configure [this _opts] this)

  (-get-collection [_ coll-name]
    (handle state coll-name))

  (-create-collection [this coll-name opts]
    (let [exists? (contains? @state coll-name)]
      (when (and exists? (not (:get-or-create? opts)))
        (throw (ex-info "Collection already exists"
                        {:reason ::collection-exists :collection coll-name})))
      (when-not exists?
        (swap! state assoc coll-name {:metadata (:metadata opts) :records {}}))
      (vp/-get-collection this coll-name)))

  (-delete-collection [_ coll]
    (swap! state dissoc (coll-key coll))
    nil)

  (-add [_ coll records _opts]
    (swap! state update-in [(coll-key coll) :records]
           (fn [existing]
             (reduce (fn [acc r] (assoc acc (:id r) r)) (or existing {}) records)))
    nil)

  (-get [_ coll opts]
    (vec (select-records (get-in @state [(coll-key coll) :records]) opts)))

  (-query [_ coll embedding {:keys [n-results where]}]
    (cond->> (select-records (get-in @state [(coll-key coll) :records]) {:where where})
      true      (keep (fn [r]
                        (when-let [e (:embedding r)]
                          (assoc r :distance (l2-distance embedding e)))))
      true      (sort-by :distance)
      n-results (take n-results)
      true      vec))

  (-delete [_ coll opts]
    (let [doomed (set (map :id (select-records (get-in @state [(coll-key coll) :records]) opts)))]
      (swap! state update-in [(coll-key coll) :records] #(apply dissoc % doomed)))
    nil)

  (-update [_ coll records]
    (swap! state update-in [(coll-key coll) :records]
           (fn [existing]
             (reduce (fn [acc r]
                       (if (contains? acc (:id r)) (update acc (:id r) merge r) acc))
                     (or existing {}) records)))
    nil))

(defn in-memory-store
  "A fresh in-memory store. Each call is independent, so one test cannot poison
   another through it."
  []
  (->InMemoryVectorStore (atom {})))

(defn with-in-memory-store
  "A `use-fixtures` fn installing a fresh in-memory store for each test, and
   RESTORING whatever was installed before.

   Restoring the captured prior value rather than clearing is the point: a
   fixture that ends by installing a constant is a write dressed as a cleanup,
   and it leaves every later namespace in this JVM reading the wrong store."
  [f]
  (let [prior (vp/get-store)]
    (vp/set-store! (in-memory-store))
    (try (f)
         (finally
           (if prior (vp/set-store! prior) (vp/clear-store!))))))
