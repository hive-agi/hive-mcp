(ns hive-mcp.swarm.datascript.get-claim-info-test
  "Trifecta for `hive-mcp.swarm.datascript.lings/get-claim-info`, a compat shim
   that delegates by name to `hive-datascript.swarm.lings/get-claim-info`
   (5 production callers: claim registry, coordinator, claim tool).

   Offline (no swarm library on this tree) a call raises addon-missing naming
   the var it reached for; that is the observable the facets pin through
   `hive-mcp.test.shim-facets`. With the library present the facets step aside:
   the behaviour itself runs in test-swarm/.

     golden    : the outcome for contract-generated arguments
     property  : any entity id reaches the namesake var
     mutation  : answering without delegating, delegating to the wrong library
                 or swallowing the call is caught"
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
(def ^:private golden-path "test/golden/hive-mcp/swarm/datascript/get-claim-info.edn")

(golden/deftest-golden get-claim-info-golden
  golden-path
  (sf/offline-golden 'hive-mcp.swarm.datascript.get-claim-info-test golden-path
                     #(sf/delegation-table shim ['get-claim-info])))

(defspec get-claim-info-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim ['get-claim-info])]
    (sf/reaches-expected? shim target {} sym args)))

(when (sf/offline?)
  (hmut/deftest-mutations get-claim-info-delegation-mutations
    hive-mcp.swarm.datascript.lings/get-claim-info
    (sf/delegation-mutants shim 'get-claim-info nil)
    (sf/delegation-check shim 'get-claim-info (sf/expected target {} 'get-claim-info))))
