(ns hive-mcp.protocols.vector-contract-test
  "Contract facets for the vector write-outcome classifier and the active
   vector-collection store. Every test owns the store and puts the prior one
   back."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.vector :as pv]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-mcp.protocols.store-metrics :as sm]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-store [f]
  (let [prior (pv/get-store)]
    (pv/clear-store!)
    (try (f)
         (finally (pv/clear-store!)
                  (when prior (pv/set-store! prior))))))

(use-fixtures :each own-the-store)

(def ^:private Outcome
  "Port write returns: nil, an id, an :error-carrying map (its :error may be
   nil), or a success map without :error. Half the draws carry :error: a
   plain four-way :or left the error arm near 13 of 100 against a floor of 12."
  [:or {:gen/schema [:tuple [:or :nil :int [:map [:id :int]]]
                     [:map [:error [:maybe [:enum :io/timeout :io/refused]]]
                      [:message {:optional true} :string]]
                     :boolean]
        :gen/fmap (fn [[ok err error?]] (if error? err ok))}
   :nil :int
   [:map [:error [:maybe [:enum :io/timeout :io/refused]]] [:message {:optional true} :string]]
   [:map [:id :int]]])

(deftrifecta-from-schema error-outcome-is-a-map-carrying-error
  #'pv/error-outcome?
  {:in [:cat Outcome]
   :out :boolean
   :rel (fn [[o] out] (= out (and (map? o) (contains? o :error))))
   :classify (fn [_ out] (if out :error :success))
   :classify-domain #{:error :success}
   :num-tests 100})

(deftrifecta-from-schema write-outcome-normalises-to-a-result
  #'pv/write-outcome->result
  {:in [:cat Outcome :int]
   :out pv/WriteResult
   :rel (fn [[o v] out]
          (if (and (map? o) (contains? o :error))
            (and (= (or (:error o) ::pv/write-failed) (:error out))
                 (= (:message o) (:message out))
                 (not (contains? out :ok)))
            (= {:ok v} out)))
   :classify (fn [_ out] (if (contains? out :ok) :ok :err))
   :classify-domain #{:ok :err}
   :golden-path "test/golden/hive-mcp/protocols/vector-write-outcome.edn"
   :num-tests 100})

(golden/deftest-golden store-lifecycle-golden
  "test/golden/hive-mcp/protocols/vector-store-lifecycle.edn"
  (let [no-store (try (pv/require-store) :no-throw
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))
        before {:set? (pv/store-set?) :get (pv/get-store) :require no-store}
        refused (try (pv/set-store! {:not :a-store}) :accepted
                     (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))
        s (reify pv/IVectorCollectionStore)
        installed (pv/set-store! s)
        during {:set? (pv/store-set?)
                :same? (every? #(identical? s (sm/unwrap %))
                               [installed (pv/get-store) (pv/require-store)])}
        cleared (pv/clear-store!)]
    {:before before :refused refused :during during :cleared cleared
     :after {:set? (pv/store-set?) :get (pv/get-store)}}))

(deftest set-store-answers-the-store
  (let [s (reify pv/IVectorCollectionStore)]
    (is (identical? s (pv/set-store! s)))))
