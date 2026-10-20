(ns hive-mcp.agent.budget-router-trifecta-test
  "Facets for the budget router, derived from its contracts: the tier
   ordering law of `tier-allowed?`, the free-model rule of
   `classify-model-tier`, and a golden over the pure tier arithmetic."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-mcp.agent.budget-router :as br]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private order {:free 0 :economy 1 :standard 2 :premium 3})

(deftrifecta-from-schema tier-allowed-trifecta
  hive-mcp.agent.budget-router/tier-allowed?
  {:in [:cat br/Tier br/Tier]
   :out :boolean
   :rel (fn [[tier max-tier] out] (= out (<= (order tier) (order max-tier))))
   :num-tests 200})

(deftrifecta-from-schema classify-free-model-trifecta
  hive-mcp.agent.budget-router/classify-model-tier
  {:in [:string {:min 1 :gen/fmap (fn [s] (str "vendor/" (apply str (filter #(Character/isLetterOrDigit ^char %) s)) ":free"))}]
   :out br/Tier
   :rel (fn [_ out] (= :free out))
   :num-tests 100})

(golden/deftest-golden-fn budget-router-golden
  "test/golden/agent/budget-router-tiers.edn"
  (fn []
    {:classify (mapv br/classify-model-tier ["anthropic/claude-opus" "x:free" "gpt-4o" nil])
     :max-tier (mapv br/max-tier-for-budget [0 0.5 100])
     :allowed [(br/tier-allowed? :premium :free) (br/tier-allowed? :free :free)
               (br/tier-allowed? :economy :standard)]}))

(deftest-contract-coverage budget-router-contract-coverage
  {:paths ["src/hive_mcp/agent/budget_router.clj"]})
