(ns hive-mcp.agent.hivemind-role-trifecta-test
  "Facets for the HivemindRole ADT, derived from its contracts: keyword
   coercion recognises exactly the three qualified variants, and only
   :role/hivemind is in hivemind mode."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [hive-mcp.agent.hivemind-role :as hr]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private RoleInput
  [:or hr/RoleKw [:enum :worker "role/worker" "role/hivemind" "bogus" nil 1] :keyword])

(deftrifecta-from-schema from-keyword-trifecta
  hive-mcp.agent.hivemind-role/from-keyword
  {:in RoleInput
   :out [:maybe hr/HivemindRoleValue]
   :rel (fn [x out]
          (let [k (cond (keyword? x) x (string? x) (keyword x))]
            (if (contains? #{:role/hivemind :role/worker :role/standalone} k)
              (= k (hr/to-keyword out))
              (nil? out))))
   :num-tests 200})

(deftrifecta-from-schema hivemind-mode-trifecta
  hive-mcp.agent.hivemind-role/hivemind-mode?
  {:in hr/HivemindRoleValue
   :out :boolean
   :rel (fn [role out] (= out (= :role/hivemind (:adt/variant role))))
   :num-tests 100})

(golden/deftest-golden-fn hivemind-role-golden
  "test/golden/agent/hivemind-role.edn"
  (fn []
    {:coerce (mapv hr/from-keyword [:role/worker "role/hivemind" :worker nil])
     :tools-worker (hr/hivemind-tools (hr/from-keyword :role/worker))
     :pool [(hr/worker-tool-pool (hr/from-keyword :role/hivemind) [:t])
            (hr/worker-tool-pool (hr/from-keyword :role/worker) [:t])]}))

(deftest-contract-coverage hivemind-role-contract-coverage
  {:paths ["src/hive_mcp/agent/hivemind_role.clj"]})
