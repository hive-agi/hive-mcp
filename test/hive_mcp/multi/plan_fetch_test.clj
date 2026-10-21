(ns hive-mcp.multi.plan-fetch-test
  "Name-matched facets for `hive-mcp.multi.plan`, driven through the memory
   store PORT (hive-spi registry) with the shared stub store: no with-redefs.

     fetch  trifecta: a plan id drawn from a table of stored entries (valid
            plan, EDN without :ops, :ops not sequential, non-EDN text, a
            non-map EDN value) or an absent id; the oracle re-derives the
            Result from the table
     fetch  golden over the same table

   compile-and-persist! and run! reach `memory.crud/handle-add` and
   `tools.multi/run-multi` by requiring-resolve; run! is covered here only on
   its fetch-failure arm, which needs no run-multi."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.multi.plan :as plan]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.memory.registry :as sreg]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private table
  {"plan-ok"      "{:ops [{:id \"a\" :tool \"memory\" :command \"search\"}] :wave-count 1}"
   "plan-ok-2"    "{:ops [] :wave-count 0 :reason \"empty\"}"
   "plan-no-ops"  "{:no-ops true}"
   "plan-ops-map" "{:ops {:a 1}}"
   "plan-not-edn" "(( not edn"
   "plan-vector"  "[1 2 3]"})

(use-fixtures :each
  (fn [f]
    (let [prior (sreg/registered-stores)]
      (try
        (stub/install! (stub/->stub nil {:get-entry-fn
                                         (fn [id] (when-let [c (table id)] {:id id :type "note" :content c}))}))
        (log/with-min-level :warn (f))
        (finally
          (sreg/reset-registry!)
          (doseq [[k s] prior] (sreg/register-store! k s)))))))

(defn- expected
  "Oracle: the Result fetch must answer for PLAN-ID, read off the table."
  [plan-id]
  (let [content (table plan-id)
        parsed  (when content (try (edn/read-string content) (catch Exception _ ::bad)))]
    (cond
      (nil? content)
      {:error :multi/plan-not-found :plan-id plan-id :message (str "no plan with id: " plan-id)}
      (or (= ::bad parsed) (nil? parsed))
      {:error :multi/plan-malformed :plan-id plan-id :message "plan content is not valid EDN"}
      (not (and (map? parsed) (sequential? (:ops parsed))))
      {:error :multi/plan-malformed :plan-id plan-id :message "plan content missing :ops vector"}
      :else {:ok parsed})))

(def ^:private PlanId
  (into [:enum "absent" "plan-ok"] (keys table)))

(deftrifecta-from-schema fetch-classifies-the-stored-entry
  #'plan/fetch
  {:in [:cat PlanId]
   :out :map
   :rel (fn [[id] out] (= out (expected id)))
   :classify (fn [_ out] (or (:error out) :ok))
   :classify-domain #{:ok :multi/plan-not-found :multi/plan-malformed}
   :classify-floor 15
   :golden-path "test/golden/hive-mcp/multi/plan-fetch.edn"
   :num-tests 200})

(deftest run!-passes-a-fetch-failure-through
  (is (= (expected "absent") (plan/run! "absent" {})))
  (is (= (expected "plan-not-edn") (plan/run! "plan-not-edn" {}))))
