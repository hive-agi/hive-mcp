(ns hive-mcp.chroma.vector-store-test
  "The Chroma IVectorCollectionStore adapter over INJECTED transports (DIP):
   a recording transport pins the option translation (port names -> vendor
   names) and the three result shapes await! resolves; the in-memory
   transport pins the port behaviour end to end. No vendor, no with-redefs."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.vector-store :as vs]
            [hive-mcp.protocols.vector :as vp]
            [hive-mcp.test.stub.chroma-transport :as transport]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- wrap
  "Answer V in the shape SHAPE: a Future, a plain IDeref, or the bare value."
  [shape v]
  (case shape
    :future (java.util.concurrent.CompletableFuture/completedFuture v)
    :deref  (delay v)
    :plain  v))

(defrecord RecordingTransport [calls shape]
  client/IChromaTransport
  (-configure [_ opts] (swap! calls conj [:configure opts]) :ok)
  (-get-collection [_ n] (swap! calls conj [:get-collection n]) (wrap shape {:name n}))
  (-create-collection [_ n opts] (swap! calls conj [:create-collection n opts]) (wrap shape {:name n}))
  (-delete-collection [_ c] (swap! calls conj [:delete-collection c]) (wrap shape :gone))
  (-add [_ c rs opts] (swap! calls conj [:add c rs opts]) (wrap shape :added))
  (-get [_ c opts] (swap! calls conj [:get c opts]) (wrap shape (list {:id "a"} {:id "b"})))
  (-query [_ c e opts] (swap! calls conj [:query c e opts])
    (wrap shape [{:id "far" :distance 0.9} {:id "near" :distance 0.1} {:id "nil-d"}]))
  (-delete [_ c opts] (swap! calls conj [:delete c opts]) (wrap shape :deleted))
  (-update [_ c rs] (swap! calls conj [:update c rs]) (wrap shape :updated)))

(defn- drive
  "Every port method once through an adapter over a recording transport
   answering in SHAPE: what the port returned and what the vendor was asked."
  [shape]
  (let [calls (atom [])
        store (vs/chroma-vector-store (->RecordingTransport calls shape))
        c     {:name "c"}
        out   {:configure (= store (vp/-configure store {:host "h"}))
               :get-coll  (vp/-get-collection store "c")
               :create    (vp/-create-collection store "c" {:metadata {:d 2} :get-or-create? true})
               :create-0  (vp/-create-collection store "c" {})
               :del-coll  (vp/-delete-collection store c)
               :add       (vp/-add store c (list {:id "a"}) {:upsert? true :junk 1})
               :add-nil   (vp/-add store c [{:id "b"}] nil)
               :get       (vp/-get store c {:ids #{"a"} :where {:t "x"} :limit 2})
               :get-0     (vp/-get store c {})
               :query     (vp/-query store c [0.0 1.0] {:n-results 3 :where {:t "x"} :include #{:metadatas}})
               :delete    (vp/-delete store c {:ids ["a"] :where {:t "x"}})
               :delete-0  (vp/-delete store c {})
               :update    (vp/-update store c (list {:id "a"}))}]
    {:out out :calls @calls}))

(deftest-golden adapter-translation-golden
  "test/golden/hive-mcp/chroma/vector-store-translation.edn"
  ;; The three result shapes must yield the same port answers; the golden
  ;; holds all three so a shape-specific regression shows as a diff.
  {:future (drive :future) :deref (drive :deref) :plain (drive :plain)})

(deftest-golden adapter-over-in-memory-transport-golden
  "test/golden/hive-mcp/chroma/vector-store-in-memory.edn"
  (let [store (vs/chroma-vector-store (transport/->transport))
        c     (vp/-create-collection store "vs" {:get-or-create? true :metadata {:dimension 2}})]
    (vp/-add store c [{:id "x" :embedding [0.0 0.0] :document "X" :metadata {:k "a"}}
                      {:id "y" :embedding [3.0 4.0] :document "Y" :metadata {:k "b"}}
                      {:id "z" :embedding [1.0 0.0] :document "Z" :metadata {:k "a"}}] {})
    (vp/-update store c [{:id "x" :document "X2"}])
    (let [near  (mapv :id (vp/-query store c [0.0 0.0] {:n-results 3}))
          where (mapv :id (vp/-query store c [3.0 4.0] {:n-results 3 :where {:k "a"}}))
          got   (mapv (juxt :id :document) (vp/-get store c {:ids ["x"]}))
          _     (vp/-delete store c {:ids ["y"]})
          left  (sort (mapv :id (vp/-get store c {})))]
      {:near near :where where :got got :left left
       :coll (:name (vp/-get-collection store "vs"))})))

(deftest default-arity-uses-the-active-transport
  (is (identical? (client/transport) (:transport (vs/chroma-vector-store)))))
