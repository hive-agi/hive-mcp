(ns hive-mcp.swarm.claim.graph-test
  "Facets for hive-mcp.swarm.claim.graph, a compat shim over
   hive-agent.swarm.claim.graph (the carto-backed callers-fn).

   Without hive-agent on the unit classpath, the golden pins where every
   public var delegates (see hive-mcp.test.swarm-shim)."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.claim.graph :as graph]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest recorded-without-the-addon-test
  (is (shim/addon-absent? "hive-agent.swarm.claim.graph")
      "the golden below is the unit-classpath answer"))

(deftest-golden graph-shim-targets-golden
  "test/golden/hive-mcp/swarm/claim/graph-shim-targets.edn"
  {:no-args (shim/targets [#'graph/available? #'graph/reset-cache!])
   :callers (shim/target graph/callers "scope" "a.b/c")
   :callers-fn (shim/target graph/callers-fn "scope")})

(deftest callers-schemas-test
  (is (m/validate graph/Callers ["a/b" "c/d"]))
  (is (not (m/validate graph/Callers '("a/b"))) "callers answers a vector"))
