(ns hive-mcp.agent.ling.status-test
  "Golden over what `hive-mcp.agent.ling.status` reads: each ling lookup
   goes to `hive-mcp.swarm.datascript.queries`, a shim over
   hive-datascript, and the table pins which query each fn reaches.

   The namespace has no injectable seam for the slave store (the queries
   are called directly, not through a port), so its behaviour over real
   slaves is exercised under the :test-swarm alias with hive-datascript on
   the classpath. This golden is the part :test-unit can observe."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.agent.ling.status]
            [hive-mcp.swarm.shim-routing :as routing]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private addon "hive-datascript.swarm.queries")

(def ^:private expected
  {'get-ling          (str addon "/get-slave")
   'get-ling-for-task (str addon "/get-slave-by-kanban-task")
   'interrupt-ling!   (str addon "/get-slave")
   'list-lings        (str addon "/get-all-slaves")})

(def ^:private args
  {'get-ling ["ling-f02"] 'get-ling-for-task ["task-f02"] 'interrupt-ling! ["ling-f02"]})

(deftest-golden-fn status-store-routing-golden
  "test/golden/agent/ling-status-store-routing.edn"
  (fn [] (routing/routing 'hive-mcp.agent.ling.status addon expected args)))

(deftest every-contracted-fn-has-an-expected-route-test
  (is (= (set (keys expected)) (set (routing/contracted-fns 'hive-mcp.agent.ling.status)))))
