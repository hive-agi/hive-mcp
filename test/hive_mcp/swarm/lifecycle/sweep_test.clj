(ns hive-mcp.swarm.lifecycle.sweep-test
  "Facets for the sweep compat shim (hive-mcp.swarm.lifecycle.sweep): every public fn
   delegates to the same-named var of hive-agent.swarm.lifecycle.sweep. Offline (no swarm
   library, this tree) a call raises addon-missing naming the var it reached
   for, and a delegated load-time value is nil; the golden, the property and
   the mutants pin that. The behaviour itself runs in test-swarm/."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.lifecycle.sweep]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.swarm.lifecycle.sweep)
(def ^:private target "hive-agent.swarm.lifecycle.sweep")
(def ^:private overrides {})
(def ^:private golden-path "test/golden/hive-mcp/swarm/lifecycle/sweep-delegation.edn")

(golden/deftest-golden sweep-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.swarm.lifecycle.sweep-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-sweep-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (sf/reaches-expected? shim target overrides sym args)))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations sweep-once-delegation-mutations
    hive-mcp.swarm.lifecycle.sweep/sweep-once!
    (sf/delegation-mutants shim 'sweep-once! 'sweep-on-boot!)
    (sf/delegation-check shim 'sweep-once! (sf/expected target overrides 'sweep-once!))))
