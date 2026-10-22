(ns hive-mcp.protocols.contract-coverage-test
  "Contract coverage gate for src/hive_mcp/protocols: every public defn
   carries an m/=> contract or a prose exemption, and every contract in the
   tree generates on both sides."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as coverage]
            [malli.core :as m]
            [malli.generator :as mg]
            [hive-mcp.protocols.agent-bridge]
            [hive-mcp.protocols.delivery-channel]
            [hive-mcp.protocols.dispatch]
            [hive-mcp.protocols.editor]
            [hive-mcp.protocols.event-backbone]
            [hive-mcp.protocols.kg]
            [hive-mcp.protocols.memory]
            [hive-mcp.protocols.memory-liveness]
            [hive-mcp.protocols.registry]
            [hive-mcp.protocols.vector]
            [hive-mcp.protocols.vessel]
            [hive-mcp.protocols.workflow]
            [hive-mcp.protocols.store-metrics]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private memory-port-methods
  "The hive-spi.memory.ports methods hive-mcp.protocols.memory re-exports as
   defns, each with the m/=> contract the port's docstring states."
  ["connect!" "disconnect!" "connected?" "health-check" "add-entry!" "get-entry"
   "update-entry!" "delete-entry!" "query-entries" "search-similar"
   "supports-semantic-search?" "cleanup-expired!" "entries-expiring-soon"
   "find-duplicate" "store-status" "reset-store!"
   "log-access!" "record-feedback!" "get-helpfulness-ratio"
   "get-entries" "update-metadata!"
   "update-staleness!" "get-stale-entries" "propagate-staleness!"
   "target-collection-for" "relocate-entry!"
   "asof-entry" "history-entry" "asof-query" "between-query"])

(coverage/deftest-contract-coverage protocols-contracts
  {:paths ["src/hive_mcp/protocols"]
   :exempt {}})

(deftest the-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/protocols"] :exempt {}})]
    (is (= 124 (count (:universe report))) "every public defn of the tree")
    (is (= 124 (count (:covered report))))))

(deftest every-port-method-re-export-is-contracted
  (let [fs (get (m/function-schemas) 'hive-mcp.protocols.memory)]
    (is (= 30 (count memory-port-methods)))
    (doseq [f memory-port-methods]
      (is (contains? fs (symbol f)) f))))

(def ^:private gated-namespaces
  '[hive-mcp.protocols.agent-bridge hive-mcp.protocols.delivery-channel
    hive-mcp.protocols.dispatch hive-mcp.protocols.editor
    hive-mcp.protocols.event-backbone hive-mcp.protocols.kg
    hive-mcp.protocols.memory hive-mcp.protocols.memory-liveness
    hive-mcp.protocols.registry hive-mcp.protocols.store-metrics
    hive-mcp.protocols.vector hive-mcp.protocols.vessel
    hive-mcp.protocols.workflow])

(deftest every-protocols-contract-generates-both-sides
  (doseq [ns-sym gated-namespaces
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
