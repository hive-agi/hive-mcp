(ns hive-mcp.agora.signal-test
  "Golden over the delegation table of the `hive-mcp.agora.signal` compat
   shim: every contracted fn routes to the same-named var of
   `hive-agent.swarm.agora.signal`. The behaviour lives in hive-agent and
   its trifectas run under the :test-swarm alias
   (test-swarm/hive_mcp/agora/signal_trifecta_test.clj); this pins the
   routing that :test-unit, which carries no hive-agent, can observe."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.agora.signal :as signal]
            [hive-mcp.swarm.shim-routing :as routing]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private addon "hive-agent.swarm.agora.signal")

(deftest-golden-fn signal-shim-routing-golden
  "test/golden/agora/signal-shim-routing.edn"
  (fn [] (routing/routing 'hive-mcp.agora.signal addon
                          (routing/same-name-targets 'hive-mcp.agora.signal addon) {})))

(deftest signal-type-schema-is-the-documented-set-test
  (is (= #{:propose :counter :no-change :approve :defer :lgtm}
         (set (rest signal/SignalType)))))
