(ns hive-mcp.tools.swarm.registry-contract-test
  "Contract facets for the lings registry's event sync: start answers the
   running SyncState, stop returns to not-running, and both are idempotent.
   The DataScript-backed reads need hive-datascript and are covered by the
   test-swarm suites."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.registry :as reg]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn sync-cycle
  "Subject: start sync N times, read the flag, stop it, read it again."
  [n]
  (reg/stop-registry-sync!)
  (let [states (vec (repeatedly n reg/start-registry-sync!))
        running (reg/registry-sync-running?)]
    (reg/stop-registry-sync!)
    {:started (mapv :running states) :running running
     :stopped (boolean (reg/registry-sync-running?))}))

(deftrifecta-from-schema registry-sync-starts-once-and-stops
  #'sync-cycle
  {:in [:int {:min 1 :max 3}]
   :out [:map [:started [:vector :boolean]] [:running :boolean] [:stopped :boolean]]
   :rel (fn [n out] (= out {:started (vec (repeat n true)) :running true :stopped false}))
   :num-tests 10})

(golden/deftest-golden registry-sync-golden
  "test/golden/hive-mcp/tools/swarm/registry-sync.edn"
  (sync-cycle 2))

(deftest stopping-a-stopped-sync-is-a-noop
  (reg/stop-registry-sync!)
  (is (nil? (reg/stop-registry-sync!))))
