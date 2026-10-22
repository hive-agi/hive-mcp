(ns hive-mcp.contract-coverage-test
  "Repo-wide gate: every public defn under src/hive_mcp carries an `m/=>`
   contract, with NO exemption. A new public defn anywhere in hive-mcp
   without a contract fails here, whatever directory it lands in.

   The per-directory gates (one per coverage batch, m00..m31) stay: they name
   the batch that owns a failure. The last 51 exemptions this gate carried
   (port-method re-exports, the breaker facade, the driver passthroughs, the
   two process entry points and mount-host/current) now carry contracts.
   Var-alias facades (`(def x #'other/x)`, events.core, plan.parser) and
   defmultis are not defns, so they are outside the universe.

   Run it with `-M:test-unit:test-focus -n hive-mcp.contract-coverage-test`:
   it loads every namespace under src/hive_mcp."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as coverage]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(coverage/deftest-contract-coverage hive-mcp-contracts-gate
  {:paths  ["src/hive_mcp"]
   :exempt {}})

(deftest the-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp"] :exempt {}})]
    (is (< 4000 (count (:universe report))) "the whole src tree is in the universe")
    (is (= (count (:universe report)) (count (:covered report))) "every public defn is contracted")))
