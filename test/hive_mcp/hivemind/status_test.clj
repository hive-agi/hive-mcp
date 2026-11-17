(ns hive-mcp.hivemind.status-test
  "Facets for hivemind status.

   Every fn resolves the swarm registry and hivemind state per call (no
   load-time capture). Offline (no library, this tree) each raises the
   delegate's documented :swarm/addon-missing ex-info naming the var it
   reached for; it used to throw a NullPointerException on the nil values
   captured at load (COV-FACETS-FINDINGS-W1). What these facets pin is that
   offline contract: every contracted fn, for any argument its m/=> input
   generates, FAILS with addon-missing (never a fabricated status), and the
   value schemas the contracts are built from discriminate. The behaviour is
   driven in test-swarm/ with the library present."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.hivemind.status :as status]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.hivemind.status)
(defn- addon-missing?
  "An offline outcome that names the addon var reached ({:ns :var}), the
   shape sf/call gives :swarm/addon-missing; never an NPE."
  [outcome]
  (and (string? (:ns outcome)) (string? (:var outcome))))

(deftest-golden status-offline-outcomes-golden
  "test/golden/hive-mcp/hivemind/status-offline.edn"
  (if (sf/offline?)
    (sf/delegation-table shim (sf/contracted shim))
    (do (is true "the swarm library is present; test-swarm/ drives status")
        (hive-test.golden/read-golden
         (hive-test.golden/anchor 'hive-mcp.hivemind.status-test
                                  "test/golden/hive-mcp/hivemind/status-offline.edn")))))

(defspec every-status-fn-fails-offline-for-any-contract-input 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (or (not (sf/offline?))
        (addon-missing? (sf/call shim sym args)))))

(when (sf/offline?)
  (hmut/deftest-mutations get-status-never-fabricates-a-status
    hive-mcp.hivemind.status/get-status
    [["answers an empty status offline"
      (fn [& [pid]] {:agents {} :project-id pid :pending-asks [] :pending-swarm-prompts []
                     :channel-connected false :ws-connected false :ws-clients 0
                     :delivery-channels {} :delivery-up? false})]
     ["answers nil offline" (constantly nil)]]
    (fn []
      (is (addon-missing? (sf/call shim 'get-status [])))
      (is (addon-missing? (sf/call shim 'get-status ["p"]))))))

(deftest status-schema-requires-every-documented-key
  (let [ok {:agents {"a" {:status :idle :name "a" :presets [] :cwd nil :project-id nil
                          :current-task nil :messages [] :last-seen nil}}
            :project-id nil :pending-asks [] :pending-swarm-prompts []
            :channel-connected false :ws-connected false :ws-clients 0
            :delivery-channels {:nats true} :delivery-up? true}]
    (is (m/validate status/Status ok))
    (doseq [k (keys ok)]
      (is (not (m/validate status/Status (dissoc ok k))) (str "missing " k)))
    (is (not (m/validate status/Status (assoc ok :ws-clients -1))))
    (is (not (m/validate status/AgentView (dissoc (get-in ok [:agents "a"]) :messages))))))
