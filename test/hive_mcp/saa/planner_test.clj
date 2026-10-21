(ns hive-mcp.saa.planner-test
  "Facets for the no-op plan synthesizer: the IPlanSynthesizer law suite run
   against the adapter ->noop-planner builds. The default synthesizes
   nothing, whatever it is given; real planning is an extension's job."
  (:require [hive-mcp.protocols.saa :as psaa]
            [hive-mcp.saa.planner :as planner]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-port-contract plan-synthesizer-port-laws
  {:port/methods
   {psaa/synthesize {:in  [:cat [:vector {:max 3} [:map [:observation :string] [:score :double]]]
                       [:maybe :string]]
                     :out :nil}}}
  (planner/->noop-planner))

(golden/deftest-golden noop-planner-golden
  "test/golden/hive-mcp/saa/planner-noop.edn"
  (let [p (planner/->noop-planner)]
    {:satisfies? (satisfies? psaa/IPlanSynthesizer p)
     :type (str (type p))
     :empty (psaa/synthesize p [] nil)
     :scored (psaa/synthesize p [{:observation "x" :score 4.0}] "task")}))
