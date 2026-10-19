(ns hive-mcp.protocols.kg-contract-test
  "Contract facets for the active graph-store slot.

   The slot's clear! closes the installed store, so a prior store is never
   cleared: each test parks a noop store over it, clears THAT, and puts the
   prior store back untouched."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.kg :as kg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-slot [f]
  (let [prior (when (kg/store-set?) (kg/get-store))]
    (kg/set-store! (kg/noop-store))
    (kg/clear-store!)
    (try (f)
         (finally (when (kg/store-set?)
                    (when-not (identical? prior (kg/get-store)) (kg/clear-store!)))
                  (when prior (kg/set-store! prior))))))

(use-fixtures :each own-the-slot)

(deftrifecta-from-schema set-store-installs-and-answers-it
  #'kg/set-store!
  {:in [:cat kg/KGStore]
   :out kg/KGStore
   :rel (fn [[s] out] (and (identical? s out) (identical? s (kg/get-store)) (kg/store-set?)))
   :num-tests 20})

(deftrifecta-from-schema an-installed-noop-store-is-not-temporal
  #'kg/active-temporal?
  {:in [:cat]
   :out :boolean
   :rel (fn [_ out] (= out (and (kg/store-set?) (kg/temporal-store? (kg/get-store)))))
   :num-tests 10})

(golden/deftest-golden slot-lifecycle-golden
  "test/golden/hive-mcp/protocols/kg-lifecycle.edn"
  (let [empty-throw (try (kg/get-store) :no-throw
                         (catch clojure.lang.ExceptionInfo e (ex-message e)))
        before {:set? (kg/store-set?) :temporal? (kg/active-temporal?) :get empty-throw}
        _ (kg/set-store! (kg/noop-store))
        during {:set? (kg/store-set?) :temporal? (kg/active-temporal?)
                :kg-store? (kg/kg-store? (kg/get-store))}
        cleared (kg/clear-store!)]
    {:before before :during during :cleared cleared
     :after {:set? (kg/store-set?) :temporal? (kg/active-temporal?)}}))

(deftest set-rejects-a-non-store
  (is (thrown? AssertionError (kg/set-store! {:conn nil})))
  (is (false? (kg/store-set?))))
