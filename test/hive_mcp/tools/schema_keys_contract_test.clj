(ns hive-mcp.tools.schema-keys-contract-test
  "Contract facets for the provider-legal property projection: a trifecta
   over legal-properties (every key legal, every legal key kept with its
   value, a `foo?` advertised as `foo_` unless `foo_` is declared) and a
   golden over legal-tool / illegal-keys."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.schema-keys :as sk]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private PropKeys
  "Legal keys, `?` keys with and without a declared `_` twin, and keys no
   spelling can make legal."
  [:enum :ok :a-b "s.t" :kg-rank? "x?" :foo? :foo_ :bad/ns "has space" "?"])

(def ^:private Props
  "Either an all-legal map (the fast path returns it as is) or a mixed one,
   so both branches of legal-properties are reached."
  [:or
   [:map-of [:enum :ok :a-b "s.t" :foo_] :int]
   [:map-of PropKeys :int]])

(defn- kname [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn- alias-of
  "`foo?` -> `foo_` in the key's own type, nil for any other key."
  [k]
  (let [n (kname k)]
    (when (and (> (count n) 1) (str/ends-with? n "?"))
      (let [a (str (subs n 0 (dec (count n))) "_")]
        (if (keyword? k) (keyword a) a)))))

(deftrifecta-from-schema legal-properties-keeps-every-legal-spelling
  #'sk/legal-properties
  {:in [:cat Props]
   :out sk/Props
   :rel (fn [[props] out]
          (let [declared (set (map kname (keys props)))]
            (and (every? sk/legal-key? (keys out))
                 (every? (fn [[k v]]
                           (cond
                             (sk/legal-key? k) (= v (get out k))
                             (and (alias-of k) (sk/legal-key? (alias-of k))
                                  (not (declared (kname (alias-of k)))))
                             (= v (get out (alias-of k)))
                             :else (not (contains? out k))))
                         props))))
   :classify (fn [[props] _] (if (every? sk/legal-key? (keys props)) :legal :rewritten))
   :classify-domain #{:legal :rewritten}
   :golden-path "test/golden/hive-mcp/tools/schema-keys-legal-properties.edn"
   :num-tests 200})

(def ^:private tool
  {:name "carto"
   :inputSchema {:properties {:kg-rank? {:type "boolean"} :scope {:type "string"}
                              "bad key" {:type "string"} :preview? {} :preview_ {}}
                 :required ["scope" "kg-rank?" "bad key"]}})

(golden/deftest-golden legal-tool-golden
  "test/golden/hive-mcp/tools/schema-keys-legal-tool.edn"
  {:tool         (sk/legal-tool tool)
   :illegal      (sk/illegal-keys tool)
   :legal-after  (sk/illegal-keys (sk/legal-tool tool))
   :no-props     (sk/legal-tool {:name "x"})
   :keys         (mapv sk/legal-key? [:a "b.c" :d? "e f" :ns/k 1])})

(deftest legal-tool-leaves-a-legal-tool-identical
  (let [t {:name "t" :inputSchema {:properties {:a {}} :required ["a"]}}]
    (is (identical? t (sk/legal-tool t)))))
