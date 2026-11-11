(ns hive-mcp.memory.reconcile-sources
  "An `IReconcileSources` built from two PAGED id listings.

   The entry store and the vector index each answer ids a page at a time
   (offset + limit). `paged-sources` turns two page functions into the port
   `hive-mcp.memory.reconcile/report` reads, so a live adapter only has to
   supply `(fn [offset limit] -> [id ...])` per side; nothing here knows a
   concrete store.

   Paging stops at the first page shorter than the page size. A listing that
   never ends (a buggy backend answering full pages forever) is cut at
   `:max-pages`, and the cut is visible: `exhausted?` answers false for that
   side, so a caller can refuse to act on a partial listing. Reads only."
  (:require [malli.core :as m]
            [hive-mcp.memory.reconcile :as rec]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-page-size 500)

(def default-max-pages
  "Page cap per side: 2000 pages of 500 is a million ids."
  2000)

(def Listing
  "Every id a paged listing produced and whether it reached its end."
  [:map
   [:ids [:vector :any]]
   [:pages [:int {:min 0}]]
   [:exhausted? :boolean]])

(defn read-all
  "Every id PAGE-FN answers, page by page from offset 0. Pure over PAGE-FN,
   a (fn [offset limit] -> sequential ids). Stops at the first page shorter
   than PAGE-SIZE (:exhausted? true) or after MAX-PAGES pages
   (:exhausted? false)."
  [page-fn page-size max-pages]
  (loop [offset 0 pages 0 acc []]
    (if (>= pages max-pages)
      {:ids acc :pages pages :exhausted? false}
      (let [page (vec (page-fn offset page-size))
            acc  (into acc page)]
        (if (< (count page) page-size)
          {:ids acc :pages (inc pages) :exhausted? true}
          (recur (+ offset page-size) (inc pages) acc))))))

(m/=> read-all [:=> [:cat ifn? [:int {:min 1}] [:int {:min 1}]] Listing])

(defrecord PagedSources [store-page vector-page page-size max-pages listings]
  rec/IReconcileSources
  (store-ids [_]
    (let [l (read-all store-page page-size max-pages)]
      (swap! listings assoc :store l)
      (:ids l)))
  (vector-ids [_]
    (let [l (read-all vector-page page-size max-pages)]
      (swap! listings assoc :vector l)
      (:ids l))))

(defn paged-sources
  "An IReconcileSources over STORE-PAGE and VECTOR-PAGE, each a
   (fn [offset limit] -> sequential ids).
   Opts: :page-size (default 500), :max-pages (default 2000)."
  ([store-page vector-page] (paged-sources store-page vector-page {}))
  ([store-page vector-page {:keys [page-size max-pages]
                            :or   {page-size default-page-size
                                   max-pages default-max-pages}}]
   (->PagedSources store-page vector-page page-size max-pages (atom {}))))

(m/=> paged-sources
      [:function
       [:=> [:cat ifn? ifn?] rec/ReconcileSources]
       [:=> [:cat ifn? ifn? [:map [:page-size {:optional true} [:int {:min 1}]]
                             [:max-pages {:optional true} [:int {:min 1}]]]]
        rec/ReconcileSources]])

(defn exhausted?
  "True when the last `report` over SOURCES read BOTH listings to their end,
   so its orphan and unindexed ids are complete. False before any read."
  [sources]
  (let [{:keys [store vector]} @(:listings sources)]
    (boolean (and (:exhausted? store) (:exhausted? vector)))))

(defrecord ScanSources [listing]
  rec/IReconcileSources
  (store-ids [_] @listing)
  (vector-ids [_] @listing))

(defn scan-sources
  "An IReconcileSources over SCAN, a (fn [] -> ids) that lists every entry id
   of a store whose vectors live in the same row as the entry (Milvus: one
   row holds id, payload and embedding). Both sides read ONE listing, run at
   most once per sources value, so the report is the store's own id hygiene:
   a row without a usable id shows as :nil-vector-ids, and orphan/unindexed
   stay empty because a co-located row cannot drift from itself. A store with
   a separate vector index must install its own two-sided sources instead.
   SCAN is complete or throws (`IMemoryStoreScan/scan-ids`), never short."
  [scan]
  (->ScanSources (delay (vec (scan)))))

(m/=> scan-sources [:=> [:cat ifn?] rec/ReconcileSources])

(m/=> exhausted? [:=> [:cat [:fn #(instance? PagedSources %)]] :boolean])
