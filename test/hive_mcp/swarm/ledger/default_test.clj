(ns hive-mcp.swarm.ledger.default-test
  "Facets for the default compat shim (hive-mcp.swarm.ledger.default): every public fn
   delegates to the same-named var of hive-agent.swarm.ledger.default. Offline (no swarm
   library, this tree) a call raises addon-missing naming the var it reached
   for, and a delegated load-time value is nil; the golden, the property and
   the mutants pin that. The behaviour itself runs in test-swarm/."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.ledger.default]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.swarm.ledger.default)
(def ^:private target "hive-agent.swarm.ledger.default")
(def ^:private overrides {})
(def ^:private golden-path "test/golden/hive-mcp/swarm/ledger/default-delegation.edn")

(golden/deftest-golden default-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.swarm.ledger.default-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-default-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (sf/reaches-expected? shim target overrides sym args)))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations append-delegation-mutations
    hive-mcp.swarm.ledger.default/append!
    (sf/delegation-mutants shim 'append! 'set-store!)
    (sf/delegation-check shim 'append! (sf/expected target overrides 'append!))))
