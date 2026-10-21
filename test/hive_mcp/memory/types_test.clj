(ns hive-mcp.memory.types-test
  "MemoryType ADT boundary coercions, driven by their contracts: from-string
   over known and unknown tokens, the variant extractors, and the permissive
   valid? gate (safe tokens pass, unsafe ones do not)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.memory.types :as types]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Token
  "Half known type names, half strangers (unknown, unsafe, non-strings)."
  [:any {:gen/schema [:tuple :boolean
                      [:enum {:gen/elements (vec (sort types/all-type-strings))} "note"]
                      [:any {:gen/elements ["bogus" "has space" "" :axiom nil 7]}]]
         :gen/fmap (fn [[known? k s]] (if known? k s))}])

(deftrifecta-from-schema from-string-admits-only-known-variants
  #'types/from-string
  {:in [:cat Token]
   :out [:maybe [:map [:adt/type :keyword] [:adt/variant :keyword]]]
   :rel (fn [[s] out]
          (if (and (string? s) (contains? types/all-type-strings s))
            (and (= :MemoryType (:adt/type out))
                 (= s (types/variant->string out))
                 (= (keyword s) (types/variant->keyword out)))
            (nil? out)))
   :classify (fn [_ out] (if out :known :rejected))
   :classify-domain #{:known :rejected}
   :golden-path "test/golden/hive-mcp/memory/types-from-string.edn"
   :num-tests 200})

(deftest-golden coercions-golden
  "test/golden/hive-mcp/memory/types-coercions.edn"
  {:valid?    (into (sorted-map)
                    (for [t ["axiom" "brand-new-type" "has space" ""]] [t (types/valid? t)]))
   :variant   [(types/variant->keyword (types/from-string "plan")) (types/variant->keyword {})
               (types/variant->string (types/from-string "lesson")) (types/variant->string {})]
   :core      (vec (sort types/core-types))
   :count     (count types/all-type-keywords)})

(deftest subsets-are-variants
  (doseq [s [types/core-types types/intent-types types/pattern-types types/semantic-types]]
    (is (every? (set types/all-type-keywords) s))))
