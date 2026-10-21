(ns hive-mcp.channel.activation-golden-test
  "Golden over `hive-mcp.channel.activation/drain-ctx`, with the activation
   provider injected through the extension registry (the port the namespace
   reads) as a series of stub providers: absent, well-behaved, partly
   malformed, garbage, empty and throwing."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.channel.activation :as activation]
            [hive-mcp.extensions.registry :as ext]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (let [prior (ext/get-extension activation/extension-key)]
      (try (f)
           (finally (if prior
                      (ext/register! activation/extension-key prior)
                      (ext/deregister! activation/extension-key)))))))

(def ^:private ctx {:tool-name "memory" :cues #{"axiom" "kanban"} :caller-id "ling-1"})

(def ^:private providers
  [[:absent      nil]
   [:well-formed (fn [{:keys [tool-name]}] {:pins #{"m-1"} :tokens #{"extra" tool-name} :floor-cap 3})]
   [:mixed       (fn [_] {:pins ["m-2" 7 nil] :tokens #{:kw "ok"} :floor-cap -1 :junk true})]
   [:garbage     (fn [_] "not a map")]
   [:empty-map   (fn [_] {})]
   [:throws      (fn [_] (throw (ex-info "boom" {})))]])

(defn- drain-with [p c]
  (if p (ext/register! activation/extension-key p) (ext/deregister! activation/extension-key))
  (activation/drain-ctx c))

(deftest-golden-fn drain-ctx-golden
  "test/golden/channel/activation-drain-ctx.edn"
  (fn []
    (into {}
          (for [[label p] providers]
            [label {:with-cues (drain-with p ctx)
                    :no-cues   (drain-with p (dissoc ctx :cues))}]))))

(deftest provider-reads-the-registry-test
  (ext/deregister! activation/extension-key)
  (is (nil? (activation/provider)))
  (ext/register! activation/extension-key identity)
  (is (identical? identity (activation/provider))))
