(ns hive-mcp.agora.schema-test
  "Golden over the delegation table of the `hive-mcp.agora.schema` compat
   shim: every contracted fn routes to the same-named var of
   `hive-agent.swarm.agora.schema`. The behaviour lives in hive-agent and
   its trifectas run under the :test-swarm alias
   (test-swarm/hive_mcp/agora/schema_trifecta_test.clj); this pins the
   routing that :test-unit, which carries no hive-agent, can observe."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.agora.schema :as schema]
            [hive-mcp.swarm.shim-routing :as routing]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private addon "hive-agent.swarm.agora.schema")

(deftest-golden-fn schema-shim-routing-golden
  "test/golden/agora/schema-shim-routing.edn"
  (fn [] (routing/routing 'hive-mcp.agora.schema addon
                          (routing/same-name-targets 'hive-mcp.agora.schema addon) {})))

(deftest value-schemas-are-closed-enums-test
  (is (= #{:active :consensus :timeout :aborted} (set (rest schema/DialogueStatus)))))
