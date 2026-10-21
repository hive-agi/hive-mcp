(ns hive-mcp.agent.identity-trifecta-test
  "Facets for the identity wiring, derived from its contracts: the mode
   parser reads only \"enforce\" (trimmed) as :enforce, `child-env` always
   forces the slave id, and an unknown caller is :unknown. The registry port
   is a plain fn or map, never a redef."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [clojure.string :as str]
            [hive-mcp.agent.identity :as id]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema parse-mode-trifecta
  hive-mcp.agent.identity/parse-mode
  {:in [:or [:enum :enforce :observe "enforce" "ENFORCE" " enforce " "observe" nil 3] :string]
   :out id/Mode
   :rel (fn [x out]
          (= out (if (and (or (string? x) (keyword? x)) (= "enforce" (str/trim (name x))))
                   :enforce :observe)))
   :num-tests 200})

(deftrifecta-from-schema child-env-forces-slave-id-trifecta
  hive-mcp.agent.identity/child-env
  {:in [:cat [:maybe [:map-of [:enum "FOO" "CLAUDE_SWARM_SLAVE_ID" :BAR] :string]]
        [:string {:min 1}] [:maybe [:string {:min 1}]]]
   :out [:map-of :string :string]
   :rel (fn [[_ ling-id cred] out]
          (and (= ling-id (get out "CLAUDE_SWARM_SLAVE_ID"))
               (or (nil? cred) (= cred (get out "HIVE_AGENT_CREDENTIAL")))))
   :num-tests 200})

(golden/deftest-golden-fn identity-golden
  "test/golden/agent/identity-wiring.edn"
  (fn []
    {:modes (mapv id/parse-mode ["enforce" "ENFORCE" :enforce nil])
     :env (id/child-env {"FOO" "x" :BAR "y"} "ling-1" "cred")
     :claimant [(id/claimant (constantly nil) "x") (id/claimant (constantly nil) nil)]}))

(deftest-contract-coverage identity-contract-coverage
  {:paths ["src/hive_mcp/agent/identity.clj"]})
