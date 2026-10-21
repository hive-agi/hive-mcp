(ns hive-mcp.resilience.protocol-test
  "Name-matched facets for `hive-mcp.resilience.protocol`: the two variant
   predicates over their contract's `:any` input (real ErrorClass values of
   every variant plus non-ADT values that must answer false), and a golden
   over the ADT constructor's runtime shape for every variant."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.resilience.protocol :as proto]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private PredicateInput
  "An ErrorClass value of any variant (each predicate's own variant forced
   one draw in eight, so its true branch is reached about a sixth of the
   time), or a non-ADT value (nil, keyword, string, empty map, a map
   carrying a variant outside the closed set)."
  [:or {:gen/schema [:tuple proto/ErrorClassValue
                     [:enum :adt :schema-mismatch :transient nil :err/transient "str" {}
                      {:adt/variant :err/made-up}]]
        :gen/fmap (fn [[err pick]]
                    (case pick
                      :adt             err
                      :schema-mismatch (assoc err :adt/variant :err/schema-mismatch)
                      :transient       (assoc err :adt/variant :err/transient)
                      pick))}
   proto/ErrorClassValue :any])

(deftrifecta-from-schema schema-mismatch?-reads-only-its-variant
  #'proto/schema-mismatch?
  {:in [:cat PredicateInput]
   :out :boolean
   :rel (fn [[x] out] (= out (and (map? x) (= :err/schema-mismatch (:adt/variant x)))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :classify-floor 20
   :num-tests 300})

(deftrifecta-from-schema transient?-reads-only-its-variant
  #'proto/transient?
  {:in [:cat PredicateInput]
   :out :boolean
   :rel (fn [[x] out] (= out (and (map? x) (= :err/transient (:adt/variant x)))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :classify-floor 20
   :num-tests 300})

(deftest-golden error-class-constructor-golden
  "test/golden/hive-mcp/resilience/protocol-error-class.edn"
  (into (sorted-map)
        (for [v (m/children (m/schema proto/ErrorClassVariant))
              :let [e (proto/error-class v {:message (name v) :details {:k 1}})]]
          [v {:value e
              :valid? (m/validate proto/ErrorClassValue e)
              :schema-mismatch? (proto/schema-mismatch? e)
              :transient? (proto/transient? e)}])))

(deftest predicates-are-mutually-exclusive
  (doseq [v (m/children (m/schema proto/ErrorClassVariant))
          :let [e (proto/error-class v {:message "x" :details {}})]]
    (is (not (and (proto/schema-mismatch? e) (proto/transient? e))) (str v))))
