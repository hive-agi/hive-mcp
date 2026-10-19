(ns hive-mcp.multi.registry-contract-test
  "Schema-derived registry facets and complete child-registry coverage."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.multi.registry.owned :as owned]
            [hive-mcp.multi.registry.tools :as tools]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]))

(deftest-contract-coverage registry-child-contracts
  {:paths ["src/hive_mcp/multi/registry"]})

(hst/deftrifecta-from-schema owned-all-contract
  hive-mcp.multi.registry.owned/all
  {:in [:cat [:fn {:gen/elements [(atom {:by-name {"echo" {:handler identity}}
                                         :by-owner {}})]}
              #(instance? clojure.lang.Atom %)]
        [:enum :by-name :by-owner]]
   :out [:map-of :any :any]
   :rel (fn [[state index-key] out] (= out (get @state index-key)))
   :num-tests 100})

(hst/deftrifecta-from-schema owned-lookup-contract
  hive-mcp.multi.registry.owned/lookup
  {:in [:cat [:fn {:gen/elements [(atom {:by-name {"echo" {:handler identity}}
                                         :by-owner {}})]}
              #(instance? clojure.lang.Atom %)]
        [:enum :by-name :by-owner] [:enum "echo" "unknown"]]
   :out [:maybe :map]
   :rel (fn [[state index-key id] out] (= out (get-in @state [index-key id])))
   :num-tests 100})

(deftest tool-registry-factory
  (is (vector? (tools/all-names))))
