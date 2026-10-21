(ns hive-mcp.swarm.datascript.coordination.config-test
  "Facets for the config compat shim (hive-mcp.swarm.datascript.coordination.config): every public fn
   delegates to the same-named var of hive-datascript.swarm.coordination.config. Offline (no swarm
   library, this tree) a call raises addon-missing naming the var it reached
   for, and a delegated load-time value is nil; the golden, the property and
   the mutants pin that. The behaviour itself runs in test-swarm/."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.datascript.coordination.config]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.swarm.datascript.coordination.config)
(def ^:private target "hive-datascript.swarm.coordination.config")
(def ^:private overrides {})
(def ^:private golden-path "test/golden/hive-mcp/swarm/datascript/coordination/config-delegation.edn")

(golden/deftest-golden config-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.swarm.datascript.coordination.config-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-config-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (sf/reaches-expected? shim target overrides sym args)))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations coordination-config-delegation-mutations
    hive-mcp.swarm.datascript.coordination.config/coordination-config
    (sf/delegation-mutants shim 'coordination-config 'resolve-CoordinationConfig)
    (sf/delegation-check shim 'coordination-config (sf/expected target overrides 'coordination-config))))
