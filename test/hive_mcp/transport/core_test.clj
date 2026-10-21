(ns hive-mcp.transport.core-test
  "Facets for hive-mcp.transport.core (cov-facets f07). No socket is opened.

   encode-msg/decode-msg are pure and are driven as a round trip from a
   bencodable schema (decode answers string keys and vectors, so the oracle
   compares against that normal form). decode-msg-from-stream is pinned on
   in-memory PushbackInputStreams: a message, EOF, and garbage. The client
   transports are checked against ITransport in their never-connected state,
   which is the only state reachable without a real endpoint."
  (:require [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [hive-mcp.transport.core :as tc]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]])
  (:import [java.io ByteArrayInputStream PushbackInputStream]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Msg
  "A bencodable message as callers send it: keyword keys, string/int values,
   one level of nesting."
  [:map-of {:gen/max 4} [:enum :type :id :op :code]
   [:or [:string {:max 6}] :int
    [:vector {:gen/max 2} [:or [:string {:max 4}] :int]]
    [:map-of {:gen/max 2} [:enum :a :b] :int]]])

(defn- normal-form
  "What bencode round-trips to: string keys, keywords as their name."
  [v]
  (walk/postwalk #(if (keyword? %) (name %) %) v))

(defn- round-trip [msg] (tc/decode-msg (tc/encode-msg msg)))

(deftrifecta-from-schema bencode-round-trip-contract round-trip
  {:in  [:cat Msg]
   :out [:map-of :string :any]
   :rel (fn [[msg] out] (= (normal-form msg) out))
   ;; the subject is the composition of two contracted vars, so the
   ;; schema-derived mutants of :map-of :string :any would only test malli.
   :mutation false
   :num-tests 200})

(deftrifecta-from-schema decode-msg-contract tc/decode-msg
  ;; No :golden-path: the input is a byte[], which has identity equality and
  ;; no stable EDN form, so a {case {:in bytes}} snapshot can never match
  ;; itself. decode is pinned on readable inputs in transport-codec-golden.
  ;; The oracle re-decodes nothing: it reads the bencode dictionary keys back
  ;; out of the raw bytes, independent of the subject.
  {:in  [:cat [:fn {:gen/schema Msg :gen/fmap tc/encode-msg} bytes?]]
   :out [:maybe tc/Decoded]
   :rel (fn [[^bytes b] out]
          (and (map? out)
               (every? string? (keys out))
               (every? #(.contains (String. b "UTF-8") (str (count %) ":" %))
                       (keys out))))})

(defn- stream-of ^PushbackInputStream [^bytes b]
  (PushbackInputStream. (ByteArrayInputStream. b)))

(deftest-golden transport-codec-golden
  "test/golden/hive-mcp/f07-transport-codec.edn"
  {:encoded-str     (String. ^bytes (tc/encode-msg {:type "ping" :id 1}) "UTF-8")
   :decode-garbage  (tc/decode-msg (.getBytes "zzz" "UTF-8"))
   :stream-message  (tc/decode-msg-from-stream (stream-of (tc/encode-msg {:op "eval" :n [1 2]})))
   :stream-eof      (tc/decode-msg-from-stream (stream-of (byte-array 0)))
   :stream-garbage  (tc/decode-msg-from-stream (stream-of (.getBytes "x" "UTF-8")))})

(deftest-port-contract unconnected-unix-transport-laws
  {:port/methods
   {tc/connected?  {:in [:cat] :out [:maybe :boolean] :rel (fn [_ out] (not out))}
    tc/send!       {:in [:cat [:map [:type :string]]] :out :nil}
    tc/recv!       {:in [:cat] :out :nil}
    tc/get-stream  {:in [:cat] :out :nil}}}
  (tc/unix-transport "/tmp/hive-f07-never-bound.sock"))

(deftest-port-contract unconnected-tcp-transport-laws
  {:port/methods
   {tc/connected?  {:in [:cat] :out [:maybe :boolean] :rel (fn [_ out] (not out))}
    tc/send!       {:in [:cat [:map [:type :string]]] :out :nil}
    tc/recv!       {:in [:cat] :out :nil}
    tc/get-stream  {:in [:cat] :out :nil}}}
  ;; port 1 is never bound by this project; nothing here connects anyway
  (tc/tcp-transport "127.0.0.1" 1))

(deftest disconnect-of-a-never-connected-transport-is-safe
  (is (nil? (tc/disconnect! (tc/unix-transport "/tmp/hive-f07-never-bound.sock"))))
  (is (satisfies? tc/ITransport (tc/tcp-transport "127.0.0.1" 1))))
