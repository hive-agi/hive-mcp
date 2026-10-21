(ns hive-mcp.swarm.claim.registry-test
  "Facets for hive-mcp.swarm.claim.registry on the unit classpath.

   The registry reads and writes claim rows through hive-mcp.swarm.datascript
   .lings and reasons about them through hive-mcp.swarm.claim.span, both
   compat shims over libraries absent here (hive-datascript, hive-agent). What
   runs without them is pinned by the golden: acquiring nothing succeeds
   without touching the store or the lock, and every other entry point names
   the store or span fn it reaches first (see hive-mcp.test.swarm-shim).
   The span-claim algebra over real rows runs in
   test-swarm/hive_mcp/swarm/claim/registry_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.claim.registry :as registry]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest recorded-without-the-addon-test
  (is (shim/addon-absent? "hive-agent.swarm.claim.span"))
  (is (shim/addon-absent? "hive-datascript.swarm.lings")))

(deftest-golden claim-registry-golden
  "test/golden/hive-mcp/swarm/claim/registry-unit.edn"
  {:acquire-nothing        (registry/acquire! [] "ling-a")
   :acquire-nothing-set    (registry/acquire! #{} "ling-a" {:task-id "t"})
   :acquire-one            (shim/target registry/acquire! ["a.clj"] "ling-a")
   :held-spans             (shim/target registry/held-spans)
   :conflicts-for          (shim/target registry/conflicts-for "a.clj" "ling-a")
   :available?             (shim/target registry/available? "a.clj" "ling-a")
   :claim-span!            (shim/target registry/claim-span! "a.clj" "ling-a")
   :release-span!          (shim/target registry/release-span! "a.clj")
   :row->span-file         (shim/target registry/row->span {:claim/file "a.clj"})
   :row->span-parsed-qn    (shim/target registry/row->span {:claim/file "a.clj#a/b"})})

(deftest acquire-result-schema-test
  (is (m/validate registry/AcquireResult (registry/acquire! [] "ling-a")))
  (is (m/validate registry/ClaimRow {}) "every claim-row key is optional")
  (is (not (m/validate registry/ClaimRow {:claim/mode :line}))))
