(ns hive-mcp.swarm.bootstrap.datahike-test
  "Facets for hive-mcp.swarm.bootstrap.datahike, a compat shim over
   hive-agent.swarm.bootstrap.datahike.

   Without hive-agent on the unit classpath, the golden pins where every
   public var delegates (see hive-mcp.test.swarm-shim), and the Config
   schema the constructors promise is checked to generate the documented
   store shape. The DatahikeBootstrap itself is exercised in
   test-swarm/hive_mcp/swarm/bootstrap/datahike_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.bootstrap.datahike :as datahike]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest recorded-without-the-addon-test
  (is (shim/addon-absent? "hive-agent.swarm.bootstrap.datahike")
      "the golden below is the unit-classpath answer"))

(deftest-golden datahike-shim-targets-golden
  "test/golden/hive-mcp/swarm/bootstrap/datahike-shim-targets.edn"
  (shim/targets [#'datahike/->DatahikeBootstrap #'datahike/map->DatahikeBootstrap
                 #'datahike/make-datahike-bootstrap]))

(deftest config-and-conn-schemas-test
  (is (every? #(m/validate datahike/Config %) (mg/sample datahike/Config {:size 5})))
  (is (m/validate datahike/Config {:store {:backend :file :path "data/swarm/datahike"}}))
  (is (not (m/validate datahike/Config {:store {:backend :file}})) "a store needs its path")
  (is (m/validate datahike/ConnAtom (atom nil)))
  (is (not (m/validate datahike/ConnAtom nil))))
