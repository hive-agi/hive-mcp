(ns hive-mcp.system.shutdown.in-core-contract-test
  "Contract facet for hive-mcp.system.shutdown.in-core: the hooks it registers,
   observed through the registry snapshot. The registries are captured and
   restored around every test; no hook is run."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.system.registry :as reg]
            [hive-mcp.system.shutdown.in-core :as in-core]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (let [saved (reg/capture-all)]
      (try (f) (finally (reg/restore-all! saved))))))

(golden/deftest-golden in-core-hooks-golden
  "test/golden/hive-mcp/system/in-core-hooks.edn"
  (do
    (reg/reset-all!)
    {:ret (in-core/register-in-core-shutdown! (atom nil) (atom {}))
     :snapshot (reg/registry-snapshot)}))
