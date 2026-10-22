(ns hive-mcp.tools.multi-contract-test
  "Contract facets for the multi batch wrapper: the MCP formatter, the
   input-validation refusals of handle-batch and the dry-run planner. Dry runs
   resolve no handler and fire no FX, so no registry is touched."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [hive-mcp.batch :as batch]
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.multi :as multi]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [hive-mcp.test.stub.isolated-registry :as iso]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(clojure.test/use-fixtures :each iso/with-fresh-registries)

(def ^:private OpRow
  [:map [:id [:enum "a" "b"]] [:success :boolean]
   [:result {:optional true} [:enum "done" {:n 1}]]
   [:error {:optional true} [:enum "boom"]]])

(def ^:private Count [:int {:min 0 :max 4}])

(def ^:private RunIn
  "A run result as run-multi answers it: an executed run or a dry-run plan.
   Sizes are kept small here, in the test's own schema."
  [:map
   [:success :boolean]
   [:summary [:map [:total Count] [:success Count] [:failed Count] [:waves Count]]]
   [:dry-run {:optional true} [:= true]]
   [:waves [:map-of {:max 2} [:int {:min 1 :max 3}]
            [:map [:ops [:vector {:max 2} [:map [:id [:enum "a" "b"]]]]]
             [:results [:vector {:max 2} OpRow]]]]]
   [:errors {:optional true} [:vector {:max 2} [:enum "cycle" "missing"]]]])

(deftrifecta-from-schema format-results-renders-the-run-as-json
  #'multi/format-results
  {:in [:cat RunIn]
   :out core/McpJsonResponse
   :rel (fn [[r] out]
          (let [j (json/read-str (:text out))]
            (and (= (:success r) (get j "success"))
                 (= (boolean (:dry-run r)) (contains? j "plan"))
                 (= (not (:dry-run r)) (contains? j "waves"))
                 (= (contains? r :errors) (contains? j "errors")))))
   :classify (fn [[r] _] (if (:dry-run r) :plan :executed))
   :classify-domain #{:plan :executed}
   :golden-path "test/golden/hive-mcp/tools/multi-format-results.edn"
   :num-tests 150})

(def ^:private BadParams
  [:or [:map [:operations [:enum [] nil]]] [:map [:x :int]]
   [:map [:operations [:enum "op" {:tool "x"}]]]])

(deftrifecta-from-schema handle-batch-refuses-malformed-operations
  #'multi/handle-batch
  {:in [:cat BadParams]
   :out core/McpResponse
   :rel (fn [[p] out]
          (and (:isError out)
               (if (seq (:operations p))
                 (= "operations must be an array of operation objects" (:text out))
                 (= 0 (.indexOf ^String (:text out) "Batch requires non-empty")))))
   :num-tests 100})

(golden/deftest-golden run-multi-dry-run-golden
  "test/golden/hive-mcp/tools/multi-dry-run.edn"
  {:plan  (multi/run-multi [{:id "a" :tool "zz" :command "x"}
                            {:id "b" :tool "zz" :command "y" :depends_on ["a"]}]
                           :dry-run true)
   :cycle (multi/run-multi [{:id "a" :tool "zz" :command "x" :depends_on ["b"]}
                            {:id "b" :tool "zz" :command "y" :depends_on ["a"]}]
                           :dry-run true)})

(deftest scalar-operations-throw-instead-of-refusing
  ;; BUG (src/hive_mcp/tools/multi.clj validate-batch-input): (empty? 7) runs
  ;; before the sequential? check, so a scalar :operations throws
  ;; IllegalArgumentException instead of answering the invalid-ops error.
  ;; Recorded at current behaviour; not fixed in this coverage batch.
  (is (= :refused (try (multi/handle-batch {:operations 7})
                       (catch Throwable _ :refused)))))

(deftest run-results-validate-against-the-batch-contract
  (is (m/validate batch/RunResult
                  (multi/run-multi [{:id "a" :tool "zz" :command "x"}] :dry-run true)))
  (is (nil? (multi/resolve-consolidated-handler "m24-no-such-tool")))
  (is (nil? (multi/resolve-tool-handler "m24-no-such-tool"))))
