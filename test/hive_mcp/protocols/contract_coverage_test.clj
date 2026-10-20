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
            [hive-mcp.protocols.workflow]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private port-method-reason
  (str "protocol-method re-export: a defn (not a def alias, so an impl extended "
       "after load is still dispatched) that calls the hive-spi.memory.ports method "
       "of the same name and nothing else. Its contract is the port's, executable as "
       "hive-spi.memory.conformance; the host fns that compute over these methods "
       "(active-store-status, get-entries-projected, the role predicates) carry "
       "the m/=> contracts"))

(def ^:private memory-port-methods
  ["connect!" "disconnect!" "connected?" "health-check" "add-entry!" "get-entry"
   "update-entry!" "delete-entry!" "query-entries" "search-similar"
   "supports-semantic-search?" "cleanup-expired!" "entries-expiring-soon"
   "find-duplicate" "store-status" "reset-store!"
   "log-access!" "record-feedback!" "get-helpfulness-ratio"
   "get-entries" "update-metadata!"
   "update-staleness!" "get-stale-entries" "propagate-staleness!"
   "target-collection-for" "relocate-entry!"
   "asof-entry" "history-entry" "asof-query" "between-query"])

(def ^:private exempt
  (into {} (map (fn [f] [(symbol "hive-mcp.protocols.memory" f) port-method-reason]))
        memory-port-methods))

(coverage/deftest-contract-coverage protocols-contracts
  {:paths ["src/hive_mcp/protocols"]
   :exempt exempt})

(deftest the-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/protocols"] :exempt exempt})]
    (is (= 105 (count (:universe report))) "every public defn of the tree")
    (is (= 75 (count (:covered report))))
    (is (= 30 (count exempt)))))

(def ^:private gated-namespaces
  '[hive-mcp.protocols.agent-bridge hive-mcp.protocols.delivery-channel
    hive-mcp.protocols.dispatch hive-mcp.protocols.editor
    hive-mcp.protocols.event-backbone hive-mcp.protocols.kg
    hive-mcp.protocols.memory hive-mcp.protocols.memory-liveness
    hive-mcp.protocols.registry hive-mcp.protocols.vector
    hive-mcp.protocols.vessel hive-mcp.protocols.workflow])

(deftest every-protocols-contract-generates-both-sides
  (doseq [ns-sym gated-namespaces
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
