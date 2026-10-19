(ns hive-mcp.context.budget-test
  "Noop fallbacks of the context budget stubs (no :cb/* extension registered)
   and the contract coverage gate for src/hive_mcp/context."
  (:require [clojure.test :refer [deftest use-fixtures]]
            [hive-mcp.context.budget :as budget]
            [hive-mcp.extensions.registry :as ext]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private cb-keys [:cb/a :cb/b :cb/c :cb/e])

(defn- without-extensions [f]
  (doseq [k cb-keys] (ext/deregister! k))
  (f)
  (doseq [k cb-keys] (ext/deregister! k)))

(use-fixtures :each without-extensions)

(deftrifecta-from-schema truncate-passthrough hive-mcp.context.budget/truncate-to-budget
  {:in [:cat [:maybe :string] :int]
   :out budget/TruncatedText
   :rel (fn [[text _] out] (= {:content (or text "") :tokens 0 :truncated? false} out))
   :num-tests 300})

(deftrifecta-from-schema ling-context-passthrough hive-mcp.context.budget/allocate-ling-context
  {:in [:cat budget/LingContextInput]
   :out budget/LingContext
   :rel (fn [[in] out]
          (and (= (or (:preset in) "") (:preset out))
               (= (or (:history in) "") (:history out))
               (= (or (:total-budget in) 0) (get-in out [:metadata :remaining]))))
   :num-tests 300})

(deftest noop-allocations
  (golden/assert-golden
   (golden/anchor 'hive-mcp.context.budget-test "test/golden/context/budget-noop.edn")
   {:tokens  (budget/estimate-tokens "some text")
    :ling    (budget/allocate-ling-context {:total-budget 4000 :axioms "ax"})
    :unified (budget/allocate-unified-entries {:total-budget 100
                                               :conventions [{:id "c"}]
                                               :snippets [1 2]})}))

(deftest-contract-coverage context-contracts
  {:paths ["src/hive_mcp/context"]})
