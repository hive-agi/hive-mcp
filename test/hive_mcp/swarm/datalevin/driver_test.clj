(ns hive-mcp.swarm.datalevin.driver-test
  "Facets for the driver compat shim (hive-mcp.swarm.datalevin.driver): every public fn
   delegates to the same-named var of hive-agent.swarm.datalevin.driver. Offline (no swarm
   library, this tree) a call raises addon-missing naming the var it reached
   for, and a delegated load-time value is nil; the golden, the property and
   the mutants pin that. The behaviour itself runs in test-swarm/.
   q is exempt from the contract gate (its result is shaped by the query), so
   it sits outside the delegation table."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.datalevin.driver]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.swarm.datalevin.driver)
(def ^:private target "hive-agent.swarm.datalevin.driver")
(def ^:private overrides {})
(def ^:private golden-path "test/golden/hive-mcp/swarm/datalevin/driver-delegation.edn")

(golden/deftest-golden driver-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.swarm.datalevin.driver-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-driver-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (sf/reaches-expected? shim target overrides sym args)))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations transact-delegation-mutations
    hive-mcp.swarm.datalevin.driver/transact!
    (sf/delegation-mutants shim 'transact! 'pull)
    (sf/delegation-check shim 'transact! (sf/expected target overrides 'transact!))))
