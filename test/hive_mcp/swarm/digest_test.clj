(ns hive-mcp.swarm.digest-test
  "Facets for hive-mcp.swarm.digest, a compat shim over hive-agent.swarm.digest.

   Without hive-agent on the unit classpath, the golden pins where every
   public fn delegates (see hive-mcp.test.swarm-shim) and the load-time value
   the shim captured. The digest projection itself runs in
   test-swarm/hive_mcp/swarm/digest_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.digest :as digest]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest recorded-without-the-addon-test
  (is (shim/addon-absent? "hive-agent.swarm.digest")
      "the golden below is the unit-classpath answer"))

(deftest-golden digest-shim-golden
  "test/golden/hive-mcp/swarm/digest-shim.edn"
  {:agent-row           (shim/target digest/agent-row 0 "a" nil)
   :render              (shim/target digest/render [])
   :roster              (shim/target digest/roster 0 {})
   :default-message-cap digest/default-message-cap})

(deftest row-schema-test
  (is (m/validate digest/Row {:a "ling-1" :e "progress" :shouts 1 :idle-s 0}))
  (is (not (m/validate digest/Row {:a "ling-1" :e nil :shouts 0 :idle-s 0})) "a row has a shout"))
