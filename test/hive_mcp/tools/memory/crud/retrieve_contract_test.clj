(ns hive-mcp.tools.memory.crud.retrieve-contract-test
  "Contract facets for the retrieval handlers' refusal arms, which answer
   before any store is touched: a metadata lookup with no usable id and a
   batch-get with no ids."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.crud.retrieve :as retrieve]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ErrorResponse
  [:map [:type [:= "text"]] [:text :string] [:isError [:= true]]])

(deftrifecta-from-schema get-metadata-refuses-a-non-id
  #'retrieve/handle-get-metadata
  {:in [:map [:id [:or [:enum nil "" "  "] :int :keyword]]]
   :out ErrorResponse
   :rel (fn [_ out] (str/includes? (:text out) "id must be a non-empty string"))
   :num-tests 60})

(deftrifecta-from-schema batch-get-refuses-no-ids
  #'retrieve/handle-batch-get
  {:in [:map [:ids [:enum nil [] () #{}]]]
   :out ErrorResponse
   :rel (fn [_ out] (str/includes? (:text out) "ids is required"))
   :num-tests 40})

(golden/deftest-golden retrieve-refusals-golden
  "test/golden/hive-mcp/tools/memory/retrieve-refusals.edn"
  (sorted-map
   :metadata-nil   (retrieve/handle-get-metadata {:id nil})
   :metadata-blank (retrieve/handle-get-metadata {:id " "})
   :batch-nil      (retrieve/handle-batch-get {})
   :batch-empty    (retrieve/handle-batch-get {:ids []})))

(deftest refusals-never-reach-a-store
  (is (true? (:isError (retrieve/handle-get-metadata {:id 42}))))
  (is (true? (:isError (retrieve/handle-batch-get {:ids nil})))))
