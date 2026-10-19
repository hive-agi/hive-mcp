(ns hive-mcp.chroma.client
  "Vendor-decoupled Chroma transport seam. The Chroma backend namespaces
   depend on this, not on clojure-chroma-client directly. Public fns mirror
   the clojure-chroma-client.api surface used by hive-mcp (same names + kwarg
   shapes); the active IChromaTransport is swappable via set-transport!."
  (:refer-clojure :exclude [get update])
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IChromaTransport
  "Low-level Chroma collection/document operations. `opts` maps carry what the
   vendor takes as keyword options; results are the vendor's deref-able values."
  (-configure [this opts])
  (-get-collection [this coll-name])
  (-create-collection [this coll-name opts])
  (-delete-collection [this coll])
  (-add [this coll records opts])
  (-get [this coll opts])
  (-query [this coll embedding opts])
  (-delete [this coll opts])
  (-update [this coll records]))

(def ^:private resolve-vendor
  (memoize
   (fn [sym]
     (try (requiring-resolve sym)
          (catch Exception e
            (throw (ex-info "Chroma client (clojure-chroma-client) not on classpath"
                            {:sym sym} e)))))))

(defn- kw-args [opts] (mapcat identity opts))

(defrecord SoftVendorTransport []
  IChromaTransport
  (-configure [_ opts]
    ((resolve-vendor 'clojure-chroma-client.config/configure) opts))
  (-get-collection [_ coll-name]
    ((resolve-vendor 'clojure-chroma-client.api/get-collection) coll-name))
  (-create-collection [_ coll-name opts]
    (apply (resolve-vendor 'clojure-chroma-client.api/create-collection) coll-name (kw-args opts)))
  (-delete-collection [_ coll]
    ((resolve-vendor 'clojure-chroma-client.api/delete-collection) coll))
  (-add [_ coll records opts]
    (apply (resolve-vendor 'clojure-chroma-client.api/add) coll records (kw-args opts)))
  (-get [_ coll opts]
    (apply (resolve-vendor 'clojure-chroma-client.api/get) coll (kw-args opts)))
  (-query [_ coll embedding opts]
    (apply (resolve-vendor 'clojure-chroma-client.api/query) coll embedding (kw-args opts)))
  (-delete [_ coll opts]
    (apply (resolve-vendor 'clojure-chroma-client.api/delete) coll (kw-args opts)))
  (-update [_ coll records]
    ((resolve-vendor 'clojure-chroma-client.api/update) coll records)))

(defonce ^:private -transport (atom (->SoftVendorTransport)))

(def Transport
  "Any IChromaTransport. Generates the soft vendor adapter, which resolves
   nothing until a method is called."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (->SoftVendorTransport))}
   #(satisfies? IChromaTransport %)])

(def Coll
  "A collection value as the vendor returns it (a map or a record)."
  [:maybe [:or :map :keyword :string]])

(def Records
  "Documents to add or update."
  [:sequential :map])

(def VendorResult
  "Whatever the vendor answers: usually a deref-able, sometimes a plain value.
   Opaque by design; the seam does not interpret it."
  :any)

(defn set-transport!
  "Install the active IChromaTransport (tests inject a stub). Returns it."
  [t]
  (reset! -transport t))

(m/=> set-transport! [:=> [:cat Transport] Transport])

(defn transport
  "The active IChromaTransport (default: the soft-resolving vendor adapter)."
  []
  @-transport)

(m/=> transport [:=> [:cat] Transport])

(defn configure [opts] (-configure (transport) opts))
(defn get-collection [coll-name] (-get-collection (transport) coll-name))
(defn create-collection [coll-name & {:as opts}] (-create-collection (transport) coll-name opts))
(defn delete-collection [coll] (-delete-collection (transport) coll))
(defn add [coll records & {:as opts}] (-add (transport) coll records opts))
(defn get [coll & {:as opts}] (-get (transport) coll opts))
(defn query [coll embedding & {:as opts}] (-query (transport) coll embedding opts))
(defn delete [coll & {:as opts}] (-delete (transport) coll opts))
(defn update [coll records] (-update (transport) coll records))

(m/=> configure [:=> [:cat [:maybe :map]] VendorResult])
(m/=> get-collection [:=> [:cat :string] VendorResult])
(m/=> create-collection [:=> [:cat :string [:* :any]] VendorResult])
(m/=> delete-collection [:=> [:cat Coll] VendorResult])
(m/=> add [:=> [:cat Coll Records [:* :any]] VendorResult])
(m/=> get [:=> [:cat Coll [:* :any]] VendorResult])
(m/=> query [:=> [:cat Coll [:sequential number?] [:* :any]] VendorResult])
(m/=> delete [:=> [:cat Coll [:* :any]] VendorResult])
(m/=> update [:=> [:cat Coll Records] VendorResult])
