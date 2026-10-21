(ns hive-mcp.swarm.registry-test
  "Facets for hive-mcp.swarm.registry, the stable wrapper API over the swarm
   DataScript store (ADR-004).

   Every wrapper forwards to hive-mcp.swarm.datascript, whose store lives in
   hive-datascript; that library is absent on the unit classpath, so the
   golden pins the store fn each wrapper reaches (see hive-mcp.test.swarm-shim),
   including list-slaves' choice of query per filter. The store-backed
   behaviour runs in test-swarm/hive_mcp/swarm/registry_protocol_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.registry :as registry]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest recorded-without-the-store-test
  (is (shim/addon-absent? "hive-datascript.swarm.queries")
      "the golden below is the unit-classpath answer"))

(deftest-golden registry-delegation-golden
  "test/golden/hive-mcp/swarm/registry-delegation.edn"
  {:get-slave                (shim/target registry/get-slave "s")
   :slave-exists?            (shim/target registry/slave-exists? "s")
   :list-slaves              (shim/target registry/list-slaves)
   :list-slaves-by-status    (shim/target registry/list-slaves {:status :idle})
   :list-slaves-by-project   (shim/target registry/list-slaves {:project-id "p"})
   :list-slaves-status-wins  (shim/target registry/list-slaves {:status :idle :project-id "p"})
   :get-slave-ids-by-project (shim/target registry/get-slave-ids-by-project "p")
   :add-slave!               (shim/target registry/add-slave! "s" {:cwd "/nonexistent-f00/p"})
   :update-slave!            (shim/target registry/update-slave! "s" {})
   :remove-slave!            (shim/target registry/remove-slave! "s")
   :reset-conn!              (shim/target registry/reset-conn!)
   :can-kill?                (shim/target registry/can-kill? "s")
   :add-task!                (shim/target registry/add-task! "t" "s" {})
   :get-task                 (shim/target registry/get-task "t")
   :claim-file!              (shim/target registry/claim-file! "f" "s" "t")
   :has-conflict?            (shim/target registry/has-conflict? ["f"] "s")
   :check-file-conflicts     (shim/target registry/check-file-conflicts ["f"] "s")
   :db-stats                 (shim/target registry/db-stats)})

(deftest add-slave-opts-schema-test
  (is (m/validate registry/AddSlaveOpts {:name "w" :cwd "/p" :presets ["a"]}))
  (is (not (m/validate registry/AddSlaveOpts {:presets "a"})) "presets is a collection"))
