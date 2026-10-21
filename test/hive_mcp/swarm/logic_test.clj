(ns hive-mcp.swarm.logic-test
  "Facets for hive-mcp.swarm.logic, a compat shim over hive-agent.swarm.logic.

   Without hive-agent on the unit classpath, the golden pins where each of the
   sixteen public fns delegates (see hive-mcp.test.swarm-shim). The logic
   engine itself runs in test-swarm/hive_mcp/swarm/logic_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.logic :as logic]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest recorded-without-the-addon-test
  (is (shim/addon-absent? "hive-agent.swarm.logic")
      "the golden below is the unit-classpath answer"))

(deftest-golden logic-shim-targets-golden
  "test/golden/hive-mcp/swarm/logic-shim-targets.edn"
  (merge
   (shim/targets [#'logic/db-stats #'logic/get-all-claims #'logic/get-logic-db-atom #'logic/reset-db!])
   {"add-claim!"                (shim/target logic/add-claim! "f" "s")
    "add-slave!"                (shim/target logic/add-slave! "s" :idle)
    "add-task-file!"            (shim/target logic/add-task-file! "t" "f")
    "check-file-conflicts"      (shim/target logic/check-file-conflicts "s" ["f"])
    "check-would-deadlock"      (shim/target logic/check-would-deadlock "t" "d")
    "get-claim-for-file"        (shim/target logic/get-claim-for-file "f")
    "get-files-for-task"        (shim/target logic/get-files-for-task "t")
    "release-claim-for-file!"   (shim/target logic/release-claim-for-file! "f")
    "release-claims-for-slave!" (shim/target logic/release-claims-for-slave! "s")
    "release-claims-for-task!"  (shim/target logic/release-claims-for-task! "t")
    "remove-claim!"             (shim/target logic/remove-claim! "f" "s")
    "slave-exists?"             (shim/target logic/slave-exists? "s")}))

(deftest logic-schemas-test
  (is (m/validate logic/FileConflict {:file "a.clj" :held-by "s1"}))
  (is (not (m/validate logic/Claim {:file "a.clj"})) "a claim names its slave"))
