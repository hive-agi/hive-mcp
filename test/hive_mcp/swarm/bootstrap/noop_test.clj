(ns hive-mcp.swarm.bootstrap.noop-test
  "Facets for hive-mcp.swarm.bootstrap.noop, a compat shim over
   hive-agent.swarm.bootstrap.noop.

   Without hive-agent on the unit classpath, the golden pins where every
   public var delegates (see hive-mcp.test.swarm-shim). The NoopBootstrap's
   own behaviour is exercised in test-swarm/ against hive-agent."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.bootstrap.noop :as noop]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest recorded-without-the-addon-test
  (is (shim/addon-absent? "hive-agent.swarm.bootstrap.noop")
      "the golden below is the unit-classpath answer"))

(deftest-golden noop-shim-targets-golden
  "test/golden/hive-mcp/swarm/bootstrap/noop-shim-targets.edn"
  (shim/targets [#'noop/->NoopBootstrap #'noop/map->NoopBootstrap #'noop/make-noop-bootstrap]))
