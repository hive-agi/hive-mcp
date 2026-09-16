(ns hive-mcp.chroma.vector-store
  "Chroma as an `IVectorCollectionStore`.

   This is the ADAPTER, and it is the one place allowed to know both sides:
   the port in `hive-mcp.protocols.vector` and the vendor transport in
   `hive-mcp.chroma.client`. It lives under `chroma.*` on purpose, because a
   provider is where naming a vendor is correct. Callers depend on the port.

   The two protocols already have the same nine methods and the same arities,
   so two things are translated here, and they are the two the callers were
   each repeating by hand:

   - the vendor returns DEREF-ABLE values, so every call site wrapped itself in
     `ws/deref-safe!` with its own timeout. The adapter owns that now; the port
     returns values.
   - the vendor returns a COLUMN-oriented result (parallel :ids / :documents /
     :metadatas / :embeddings / :distances vectors) and the port promises a seq
     of RECORD MAPS. `rows` is that transposition."
  (:require [hive-mcp.chroma.client :as client]
            [hive-mcp.protocols.vector :as vp]
            [hive-weave.safe :as ws]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private read-timeout-ms
  "Reads were 15s at the call sites; writes and deletes were 30s. Kept as they
   were rather than unified, so this refactor changes no timeout."
  15000)

(def ^:private write-timeout-ms 30000)

(defn- await!
  "Resolve a vendor result to a value.

   Three shapes, because three things produce them: the real client returns a
   `Future`, which must be bounded (`deref-safe!` casts to Future and is the
   only form that can time out); a test transport may return any other IDeref,
   which is already realised or cheap; and a stub may return the value itself."
  [v timeout-ms]
  (cond
    (instance? java.util.concurrent.Future v) (ws/deref-safe! v timeout-ms)
    (instance? clojure.lang.IDeref v)         @v
    :else                                     v))

(defn- unwrap
  "Chroma nests a single query's results one level deep (a batch of one), and
   returns them flat for a plain get. Take the inner vector when it is there."
  [v]
  (if (and (sequential? v) (sequential? (first v))) (first v) v))

(defn- rows
  "Transpose a column-oriented Chroma result into record maps.

   Key names are normalised to the port's singular spelling, and a column the
   vendor omitted simply contributes no key, rather than a vector of nils."
  [result]
  (let [ids   (unwrap (:ids result))
        docs  (unwrap (:documents result))
        metas (unwrap (:metadatas result))
        embs  (unwrap (:embeddings result))
        dists (unwrap (:distances result))]
    (vec
     (map-indexed
      (fn [i id]
        (cond-> {:id id}
          (seq docs)  (assoc :document  (nth docs i nil))
          (seq metas) (assoc :metadata  (nth metas i nil))
          (seq embs)  (assoc :embedding (nth embs i nil))
          (seq dists) (assoc :distance  (nth dists i nil))))
      ids))))

(defn- ->vendor-records
  "Port records -> the vendor's column-oriented add/update payload."
  [records]
  {:ids        (mapv :id records)
   :documents  (mapv :document records)
   :metadatas  (mapv :metadata records)
   :embeddings (mapv :embedding records)})

(defrecord ChromaVectorStore [transport]
  vp/IVectorCollectionStore
  (-configure [this opts]
    (client/-configure transport opts)
    this)

  (-get-collection [_ coll-name]
    (await! (client/-get-collection transport coll-name) read-timeout-ms))

  (-create-collection [_ coll-name opts]
    (await! (client/-create-collection
             transport coll-name
             (cond-> {}
               (:metadata opts)       (assoc :metadata (:metadata opts))
               (:get-or-create? opts) (assoc :get_or_create true)))
            write-timeout-ms))

  (-delete-collection [_ coll]
    (await! (client/-delete-collection transport coll) write-timeout-ms)
    nil)

  (-add [_ coll records opts]
    (await! (client/-add transport coll (->vendor-records records) (or opts {}))
            write-timeout-ms)
    nil)

  (-get [_ coll {:keys [ids where limit]}]
    (rows (await! (client/-get transport coll
                               (cond-> {}
                                 (seq ids) (assoc :ids (vec ids))
                                 where     (assoc :where where)
                                 limit     (assoc :limit limit)))
                  read-timeout-ms)))

  (-query [_ coll embedding {:keys [n-results where]}]
    ;; The port promises NEAREST FIRST on an ascending :distance. Chroma
    ;; already returns that order, so this sorts defensively rather than
    ;; trusting it: a backend that ever returned a similarity here would
    ;; otherwise reverse every caller's results silently.
    (->> (await! (client/-query transport coll embedding
                                (cond-> {}
                                  n-results (assoc :n-results n-results)
                                  where     (assoc :where where)))
                 read-timeout-ms)
         rows
         (sort-by #(or (:distance %) 0))
         vec))

  (-delete [_ coll {:keys [ids where]}]
    (await! (client/-delete transport coll
                            (cond-> {}
                              (seq ids) (assoc :ids (vec ids))
                              where     (assoc :where where)))
            write-timeout-ms)
    nil)

  (-update [_ coll records]
    (await! (client/-update transport coll (->vendor-records records))
            write-timeout-ms)
    nil))

(defn chroma-vector-store
  "A store over the active Chroma transport, or over TRANSPORT when given."
  ([] (chroma-vector-store (client/transport)))
  ([transport] (->ChromaVectorStore transport)))
