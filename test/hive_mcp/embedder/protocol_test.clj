(ns hive-mcp.embedder.protocol-test
  "The embedder L0 ports. No adapter lives in hive-mcp, so the laws are run
   against an in-test adapter injected through the protocols, the way any
   real provider would be: embed preserves order and length and yields
   vectors of the provider's dimension; provider-spec answers a ProviderSpec
   that `spec/make` accepts unchanged."
  (:require [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-mcp.embedder.protocol :as proto]
            [hive-mcp.embedder.spec :as spec]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord ConstantEmbedder [pspec]
  proto/IEmbedder
  (embed [_ texts]
    (r/ok (mapv (fn [t] (float-array (:provider/dim pspec) (float (count t)))) texts)))
  proto/IProviderMeta
  (provider-spec [_] pspec))

(def ^:private adapter
  (->ConstantEmbedder {:provider/key :test/constant :provider/impl :ollama
                       :provider/model "constant" :provider/dim 4
                       :provider/max-tokens 512}))

(deftest-port-contract embedder-port-laws
  {:port/methods
   {proto/embed         {:in  [:cat [:sequential :string]]
                         :out [:map [:ok [:vector [:fn {:gen/elements [(float-array 4)]}
                                                   #(instance? (Class/forName "[F") %)]]]]
                         :rel (fn [[texts] res]
                                (and (= (count texts) (count (:ok res)))
                                     (every? #(= 4 (alength ^floats %)) (:ok res))
                                     (= (map count texts)
                                        (map #(long (aget ^floats % 0)) (:ok res)))))}
    proto/provider-spec {:in  [:cat]
                         :out spec/schema}}}
  adapter)

(deftest the-port-methods-dispatch
  (is (satisfies? proto/IEmbedder adapter))
  (is (satisfies? proto/IProviderMeta adapter))
  (is (m/validate spec/schema (proto/provider-spec adapter))))

;; The ProviderSpec an adapter self-describes round-trips through the smart
;; constructor: what a provider answers is what the locator may key on.
(deftrifecta-from-schema provider-spec-round-trips spec/make
  {:in  [:cat spec/schema]
   :out spec/schema
   :rel (fn [[pspec] made]
          (= made (proto/provider-spec (->ConstantEmbedder pspec))))
   :num-tests 200})
