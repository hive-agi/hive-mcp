(ns hive-mcp.agent.headless-capability-trifecta-test
  "Facets for the HeadlessCapability ADT, derived from its contracts:
   every capability keyword survives from-keyword then to-keyword."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-mcp.agent.headless-capability :as hc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema from-keyword-roundtrip-trifecta
  hive-mcp.agent.headless-capability/from-keyword
  {:in hc/CapabilityKw
   :out [:maybe hc/HeadlessCapabilityValue]
   :rel (fn [k out] (= k (hc/to-keyword out)))
   :num-tests 100})

(golden/deftest-golden-fn headless-capability-golden
  "test/golden/agent/headless-capability.edn"
  (fn []
    {:coerce (mapv hc/from-keyword [:cap/hooks "cap/saa" :hooks nil])}))

(deftest-contract-coverage headless-capability-contract-coverage
  {:paths ["src/hive_mcp/agent/headless_capability.clj"]})
