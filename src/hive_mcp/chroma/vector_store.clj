(ns hive-mcp.chroma.vector-store
  "Chroma as an `IVectorCollectionStore`.

   This is the ADAPTER, and the one place allowed to know both sides: the port
   in `hive-mcp.protocols.vector` and the vendor transport in
   `hive-mcp.chroma.client`. It lives under `chroma.*` on purpose, because a
   provider is where naming a vendor is correct. Callers depend on the port.

   The vendor client is already record-oriented: `add` takes embedding records,
   `get` and `query` return them, and `query` is documented as ordered by
   increasing distance. So this adapter does NOT reshape payloads. It absorbs
   the two things every call site was otherwise repeating by hand:

   - the client returns FUTURES, so each caller wrapped itself in
     `ws/deref-safe!` with its own timeout. Reads were 15s and writes 30s at
     the call sites; those are kept, so nothing's timeout changes.
   - the client's option names are its own (`:num-results`, `:get-or-create`).
     The port speaks `:n-results` and `:get-or-create?`, and translating
     between the two is exactly why an adapter exists rather than an alias."
  (:require [hive-mcp.chroma.client :as client]
            [hive-mcp.protocols.vector :as vp]
            [hive-weave.safe :as ws]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private read-timeout-ms 15000)
(def ^:private write-timeout-ms 30000)

(def ^:private default-include
  "What a read asks the vendor to return. The port always promises the record
   fields, so a caller never has to remember this set."
  #{:documents :metadatas :distances})

(defn- await!
  "Resolve a vendor result to a value.

   Three shapes, because three things produce them: the real client returns a
   `Future`, which must be bounded (`deref-safe!` casts to Future and is the
   only form that can time out); a test transport may return another IDeref;
   and a stub may return the value itself."
  [v timeout-ms]
  (cond
    (instance? java.util.concurrent.Future v) (ws/deref-safe! v timeout-ms)
    (instance? clojure.lang.IDeref v)         @v
    :else                                     v))

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
               (:get-or-create? opts) (assoc :get-or-create true)))
            write-timeout-ms))

  (-delete-collection [_ coll]
    (await! (client/-delete-collection transport coll) write-timeout-ms)
    nil)

  (-add [_ coll records opts]
    (await! (client/-add transport coll (vec records)
                         (select-keys (or opts {}) [:upsert?]))
            write-timeout-ms)
    nil)

  (-get [_ coll {:keys [ids where limit include]}]
    (vec (await! (client/-get transport coll
                              (cond-> {:include (or include default-include)}
                                (seq ids) (assoc :ids (vec ids))
                                where     (assoc :where where)
                                limit     (assoc :limit limit)))
                 read-timeout-ms)))

  (-query [_ coll embedding {:keys [n-results where include]}]
    ;; The port promises NEAREST FIRST on an ascending :distance, and the
    ;; vendor documents that order. Sorting anyway is the cheap guard: a
    ;; backend that ever passed a cosine SIMILARITY through :distance would
    ;; otherwise reverse every caller, which reads as slightly worse results
    ;; and never as a bug.
    (->> (await! (client/-query transport coll embedding
                                (cond-> {:include (or include default-include)}
                                  n-results (assoc :num-results n-results)
                                  where     (assoc :where where)))
                 read-timeout-ms)
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
    (await! (client/-update transport coll (vec records)) write-timeout-ms)
    nil))

(defn chroma-vector-store
  "A store over the active Chroma transport, or over TRANSPORT when given."
  ([] (chroma-vector-store (client/transport)))
  ([transport] (->ChromaVectorStore transport)))
