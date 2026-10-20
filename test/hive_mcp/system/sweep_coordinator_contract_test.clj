(ns hive-mcp.system.sweep-coordinator-contract-test
  "Contract facets for hive-mcp.system.sweep-coordinator: status is read over a
   registry of stub sweeps; the coordinator is never started here. The
   registries are captured and restored around every test."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.registry :as reg]
            [hive-mcp.system.sweep-coordinator :as sc]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (let [saved (reg/capture-all)]
      (try (f) (finally (reg/restore-all! saved))))))

(defn- sweep [n s]
  (reify lifecycle/ISweepable
    (sweep-interval-s [_] s) (sweep-name [_] n) (sweep! [_ _] {:swept 0 :errors []})))

(defn- status-for
  "The stable part of status (not the run history) over `sweeps`."
  [sweeps]
  (reg/reset-all!)
  (doseq [s sweeps] (reg/register-sweep! s))
  (select-keys (sc/status) [:heartbeat-s :registered]))

(golden/deftest-golden coordinator-status-golden
  "test/golden/hive-mcp/system/sweep-coordinator-status.edn"
  (when-not (:running? (sc/status))
    {:empty    (status-for [])
     :floored  (status-for [(sweep "c/fast" 5)])
     :gcd      (status-for [(sweep "c/a" 60) (sweep "c/b" 90)])
     :one-slow (status-for [(sweep "c/slow" 600)])}))

(deftest coordinator-status-and-idempotent-stop
  (reg/reset-all!)
  (reg/register-sweep! (sweep "c/s" 5))
  (let [st (sc/status)]
    (is (m/validate sc/CoordinatorStatus st))
    (is (= 10 (:heartbeat-s st)) "the cadence is floored at 10 s")
    (is (= ["c/s"] (:registered st))))
  (when-not (:running? (sc/status))
    (is (false? (sc/stop!)) "stop! of a stopped coordinator is a no-op")))
