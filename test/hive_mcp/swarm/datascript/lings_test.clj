(ns hive-mcp.swarm.datascript.lings-test
  "Facets for the lings compat shim (hive-mcp.swarm.datascript.lings): every public fn
   delegates to the same-named var of hive-datascript.swarm.lings. Offline (no swarm
   library, this tree) a call raises addon-missing naming the var it reached
   for, and a delegated load-time value is nil; the golden, the property and
   the mutants pin that. The behaviour itself runs in test-swarm/.
   claim-file! is the exception: it checks for a live foreign holder FIRST,
   through hive-agent.swarm.logic, so offline it reaches that var."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.swarm.datascript.lings]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.swarm.datascript.lings)
(def ^:private target "hive-datascript.swarm.lings")
(def ^:private overrides '{claim-file! {:ns "hive-agent.swarm.logic", :var "get-logic-db-atom"}})

(def ^:private delegating
  "The contracted fns that delegate by name. holder-live? composes
   queries/get-slave with the dead-status rule rather than delegating, so it is
   not a namesake call; its behaviour runs in test-swarm (claim gate_test)."
  (vec (remove '#{holder-live?} (sf/contracted shim))))
(def ^:private golden-path "test/golden/hive-mcp/swarm/datascript/lings-delegation.edn")

(golden/deftest-golden lings-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.swarm.datascript.lings-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-lings-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim delegating)]
    (sf/reaches-expected? shim target overrides sym args)))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations update-slave-delegation-mutations
    hive-mcp.swarm.datascript.lings/update-slave!
    (sf/delegation-mutants shim 'update-slave! 'add-slave!)
    (sf/delegation-check shim 'update-slave! (sf/expected target overrides 'update-slave!))))
