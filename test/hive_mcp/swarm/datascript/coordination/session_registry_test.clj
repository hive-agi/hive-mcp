(ns hive-mcp.swarm.datascript.coordination.session-registry-test
  "Facets for the session-registry compat shim (hive-mcp.swarm.datascript.coordination.session-registry): every public fn
   delegates to the same-named var of hive-datascript.swarm.coordination.session-registry. Offline (no swarm
   library, this tree) a call raises addon-missing naming the var it reached
   for, and a delegated load-time value is nil; the golden, the property and
   the mutants pin that. The behaviour itself runs in test-swarm/."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.datascript.coordination.session-registry]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.swarm.datascript.coordination.session-registry)
(def ^:private target "hive-datascript.swarm.coordination.session-registry")
(def ^:private overrides {})
(def ^:private golden-path "test/golden/hive-mcp/swarm/datascript/coordination/session-registry-delegation.edn")

(golden/deftest-golden session-registry-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.swarm.datascript.coordination.session-registry-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-session-registry-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (sf/reaches-expected? shim target overrides sym args)))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations register-completed-task-delegation-mutations
    hive-mcp.swarm.datascript.coordination.session-registry/register-completed-task!
    (sf/delegation-mutants shim 'register-completed-task! 'register-kanban-movement!)
    (sf/delegation-check shim 'register-completed-task! (sf/expected target overrides 'register-completed-task!))))
