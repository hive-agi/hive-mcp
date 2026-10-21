(ns hive-mcp.agent.context-envelope-trifecta-test
  "Facets for context-envelope, derived from its contracts. With no
   extension registered, every entry point answers nil, which is the
   degraded path the namespace documents."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-mcp.agent.context-envelope :as ce]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema enrich-context-trifecta
  hive-mcp.agent.context-envelope/enrich-context
  {:in [:cat [:maybe [:map-of :keyword :string]] [:maybe [:vector :string]] [:maybe :string]]
   :out [:maybe [:string {:min 1}]]
   :rel (fn [_ out] (or (nil? out) (pos? (count out))))
   :num-tests 100})

(golden/deftest-golden-fn context-envelope-golden
  "test/golden/agent/context-envelope.edn"
  (fn []
    {:enrich (ce/enrich-context nil nil nil)
     :spawn (ce/prepare-spawn-context nil)
     :dispatch (ce/from-dispatch-context nil)}))

(deftest-contract-coverage context-envelope-contract-coverage
  {:paths ["src/hive_mcp/agent/context_envelope.clj"]})
