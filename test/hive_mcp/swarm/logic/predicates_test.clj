(ns hive-mcp.swarm.logic.predicates-test
  "Facets for the predicates compat shim (hive-mcp.swarm.logic.predicates): every public fn
   delegates to the same-named var of hive-agent.swarm.logic.predicates. Offline (no swarm
   library, this tree) a call raises addon-missing naming the var it reached
   for, and a delegated load-time value is nil; the golden, the property and
   the mutants pin that. The behaviour itself runs in test-swarm/."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.logic.predicates]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.swarm.logic.predicates)
(def ^:private target "hive-agent.swarm.logic.predicates")
(def ^:private overrides {})
(def ^:private golden-path "test/golden/hive-mcp/swarm/logic/predicates-delegation.edn")

(golden/deftest-golden predicates-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.swarm.logic.predicates-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-predicates-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (sf/reaches-expected? shim target overrides sym args)))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations would-deadlocko-delegation-mutations
    hive-mcp.swarm.logic.predicates/would-deadlocko
    (sf/delegation-mutants shim 'would-deadlocko 'reachable-fromo)
    (sf/delegation-check shim 'would-deadlocko (sf/expected target overrides 'would-deadlocko))))
