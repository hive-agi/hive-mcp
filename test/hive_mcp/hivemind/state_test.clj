(ns hive-mcp.hivemind.state-test
  "Facets for the hivemind state compat shim: its fns delegate to the
   same-named vars of hive-agent.swarm.hivemind.state, except
   current-agent-registry, which derefs the resolved registry and answers
   nil without the library. Offline (this tree) a delegating call raises
   addon-missing naming the var it reached for; the golden, property and
   mutants pin that. The state itself is driven in test-swarm/."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.hivemind.state :as state]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.hivemind.state)
(def ^:private target "hive-agent.swarm.hivemind.state")
(def ^:private overrides '{current-agent-registry {:returned nil}})
(def ^:private golden-path "test/golden/hive-mcp/hivemind/state-delegation.edn")

(golden/deftest-golden state-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.hivemind.state-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-state-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (sf/reaches-expected? shim target overrides sym args)))

(deftest current-agent-registry-is-resolved-per-call
  (if (sf/offline?)
    (is (nil? (state/current-agent-registry)) "no library: nil, never a throw")
    (is (some? (state/current-agent-registry)))))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations record-ling-result-delegation-mutations
    hive-mcp.hivemind.state/record-ling-result!
    (sf/delegation-mutants shim 'record-ling-result! 'remove-ling-result!)
    (sf/delegation-check shim 'record-ling-result! (sf/expected target overrides 'record-ling-result!))))
