(ns hive-mcp.agent.cider-trifecta-test
  "Facets for the CIDER backend factory and session pool, derived from
   their contracts. No nREPL is needed: the pool is read uninitialised and
   the factory is driven on its ollama arm, which builds a value only."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-mcp.agent.cider :as cider]
            [hive-mcp.agent.openrouter :as openrouter]
            [hive-mcp.agent.protocol :as proto]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema make-ollama-backend-trifecta
  hive-mcp.agent.cider/make-backend
  {:in [:cat [:= :ollama] [:map [:model [:string {:min 1}]]]]
   :out openrouter/Backend
   :rel (fn [[_ opts] out] (= (:model opts) (proto/model-name out)))
   ;; The output is a protocol handle ([:fn] with :gen/elements): malli
   ;; derives no structural mutant from it, so the mutation facet is off.
   :mutation false
   :num-tests 50})

(golden/deftest-golden-fn cider-pool-golden
  "test/golden/agent/cider-pool.edn"
  (fn []
    (cider/shutdown-pool!)
    {:status (cider/pool-status)
     :release-unknown (cider/release-session! "no-such-session")
     :ollama-model (proto/model-name (cider/make-backend :ollama {:model "m"}))}))

(deftest-contract-coverage cider-contract-coverage
  {:paths ["src/hive_mcp/agent/cider.clj"]})
