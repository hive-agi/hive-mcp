(ns hive-mcp.tools.memory.crud.query-contract-test
  "Contract facets for the pure helpers of memory query: the temporal
   threshold predicate and the refusal arms of handle-query, which answer
   before any store is touched."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.memory.crud.query :as q]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Stamp
  [:maybe [:enum "2026-01-01T00:00:00Z" "2026-06-01T00:00:00Z" "2026-12-31T23:59:59Z"]])

(deftrifecta-from-schema entry-after-is-a-strict-iso-compare
  #'q/entry-after?
  {:in [:cat [:map [:created Stamp] [:updated Stamp]] [:enum :created :updated] Stamp]
   :out :boolean
   :rel (fn [[entry k threshold] out]
          (= out (or (nil? threshold)
                     (let [ts (get entry k)]
                       (boolean (and ts (pos? (compare ts threshold))))))))
   :classify (fn [[entry k threshold] out]
               (cond (nil? threshold) :no-threshold
                     (nil? (get entry k)) :no-stamp
                     out :after
                     :else :not-after))
   :classify-domain #{:no-threshold :no-stamp :after :not-after}
   :classify-floor 4
   :num-tests 300})

(golden/deftest-golden handle-query-refusals-golden
  "test/golden/hive-mcp/tools/memory/query-refusals.edn"
  (into (sorted-map)
        (map (fn [[k params]] [k (q/handle-query params)]))
        {:text-query   {:type "note" :query "free text"}
         :unfiltered   {}
         :limit-high   {:type "note" :limit 1001}
         :limit-neg    {:type "note" :limit -1}
         :limit-coerce {:type "note" :limit "abc"}}))

(deftest every-refusal-is-an-error-response
  (doseq [p [{:type "note" :query "x"} {} {:type "note" :limit 5000}]]
    (is (true? (:isError (q/handle-query p))) (pr-str p))))
