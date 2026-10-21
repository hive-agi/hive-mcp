(ns hive-mcp.swarm.bootstrap.datahike-driver-test
  "Facets for hive-mcp.swarm.bootstrap.datahike-driver, a late-bound facade
   (compat shim) over hive-agent.swarm.bootstrap.datahike-driver.

   Without hive-agent on the unit classpath, the golden pins where each of
   the eight driver fns delegates (see hive-mcp.test.swarm-shim)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.bootstrap.datahike-driver :as driver]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest recorded-without-the-addon-test
  (is (shim/addon-absent? "hive-agent.swarm.bootstrap.datahike-driver")
      "the golden below is the unit-classpath answer"))

(deftest-golden driver-shim-targets-golden
  "test/golden/hive-mcp/swarm/bootstrap/datahike-driver-shim-targets.edn"
  (shim/targets [#'driver/create-database #'driver/connect #'driver/transact #'driver/q
                 #'driver/pull #'driver/db #'driver/release #'driver/database-exists?]))
