(ns hive-mcp.agora.consensus-test
  "Golden over the delegation table of the `hive-mcp.agora.consensus` compat
   shim: every contracted fn routes to the same-named var of
   `hive-agent.swarm.agora.consensus`. The behaviour lives in hive-agent and
   its trifectas run under the :test-swarm alias
   (test-swarm/hive_mcp/agora/consensus_trifecta_test.clj); this pins the
   routing that :test-unit, which carries no hive-agent, can observe."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.agora.consensus]
            [hive-mcp.swarm.shim-routing :as routing]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private addon "hive-agent.swarm.agora.consensus")

(deftest-golden-fn consensus-shim-routing-golden
  "test/golden/agora/consensus-shim-routing.edn"
  (fn [] (routing/routing 'hive-mcp.agora.consensus addon
                          (routing/same-name-targets 'hive-mcp.agora.consensus addon) {})))

(deftest every-contracted-fn-is-routed-test
  (is (= 22 (count (routing/contracted-fns 'hive-mcp.agora.consensus)))))
