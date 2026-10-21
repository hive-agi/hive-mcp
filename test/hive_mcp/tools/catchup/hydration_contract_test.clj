(ns hive-mcp.tools.catchup.hydration-contract-test
  "catchup.hydration over the IMemoryStore PORT (seeded StubMemoryStore)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.tools.catchup.contract-fixtures :as fx]
            [hive-mcp.tools.catchup.hydration :as hydr]
            [hive-mcp.tools.catchup.shapes :as shapes]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each fx/with-seeded-store)

(deftrifecta-from-schema has-tag-membership
  hive-mcp.tools.catchup.hydration/has-tag?
  {:in  [:cat [:maybe [:map [:tags [:maybe [:vector {:gen/max 3} [:enum "a" "b" "c"]]]]]] [:enum "a" "b"]]
   :out :boolean
   :rel (fn [[e t] out] (= out (boolean (some #{t} (:tags e)))))
   :num-tests 200})

(deftest-golden hydrate-buckets-over-stub
  "test/golden/catchup/hydration-buckets.edn"
  (fx/with-seeded-store
    #(update-vals (hydr/hydrate-buckets {:axioms [{:id "e1" :distance 0.3}]
                                         :decisions [{:id "zz"} {:id "e3"}]})
                  (fn [es] (mapv (fn [e] (select-keys e [:id :content :distance])) es)))))

(deftest hydration-swaps-shells
  (let [out (hydr/hydrate-buckets {:axioms [{:id "e1" :distance 0.3}] :decisions [{:id "zz"} {:id "e3"}]})]
    (is (= "axiom one" (get-in out [:axioms 0 :content])))
    (is (= 0.3 (get-in out [:axioms 0 :distance])) "the vector score survives hydration")
    (is (= {:id "zz"} (get-in out [:decisions 0])) "a missing id passes through as its shell")
    (is (every? #(malli.core/validate shapes/Entry %) (mapcat val out))))
  (is (= {} (hydr/hydrate-buckets {})))
  (is (= {} (hydr/hydrate-buckets nil))))
