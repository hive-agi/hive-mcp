(ns hive-mcp.contract-coverage-test
  "Repo-wide gate: every public defn under src/hive_mcp carries an `m/=>`
   contract or a REASONED exemption, and no exemption is stale. A new public
   defn anywhere in hive-mcp without a contract fails here, whatever directory
   it lands in.

   The per-directory gates (one per coverage batch, m00..m31) stay: they name
   the batch that owns a failure. This gate's exemptions are the union of
   theirs, with their reasons, plus the residue no batch owned. Var-alias
   facades (`(def x #'other/x)`, events.core, plan.parser) and defmultis are
   not defns, so they are outside the universe and need no exemption.

   Run it with `-M:test-unit:test-focus -n hive-mcp.contract-coverage-test`:
   it loads every namespace under src/hive_mcp."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.coverage :as coverage]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- exempt-all
  "{ns/name reason} for every name in NAMES."
  [ns-str names reason]
  (into {} (map (fn [n] [(symbol ns-str n) reason])) names))

;; From hive-mcp.extensions.contract-coverage-test.
(def ^:private extensions-exemptions
  {'hive-mcp.extensions.mount-host/current
   "Identity on every value that is not a stale AddonRegistryHost: it is handed any IMountHost (or :not-a-host) and answers that same value, so its only true contract is [:=> [:cat :any] :any], which contracts nothing. The rebuild arm is pinned by mount-host-contract-test and mount_host_test."})

;; From hive-mcp.knowledge-graph.m14-contract-coverage-test.
(def ^:private kg-slots-breaker-exemptions
  (exempt-all "hive-mcp.knowledge-graph.slots.breaker"
              ["fresh" "on-success" "on-failure" "maybe-recover" "decision" "attempt"
               "record-success!" "record-failure!" "snapshot" "reset!"]
              (str "call-through facade over the hive-mcp.resilience.breaker kernel var: its "
                   "result is whatever the kernel var returns, and a redef of the kernel must "
                   "reach callers of this name (hive-mcp.kernel.misfiled-facade-test), so the "
                   "contract belongs on the kernel fn, outside this batch")))

;; From hive-mcp.protocols.contract-coverage-test.
(def ^:private protocols-memory-exemptions
  (exempt-all "hive-mcp.protocols.memory"
              ["connect!" "disconnect!" "connected?" "health-check" "add-entry!" "get-entry"
               "update-entry!" "delete-entry!" "query-entries" "search-similar"
               "supports-semantic-search?" "cleanup-expired!" "entries-expiring-soon"
               "find-duplicate" "store-status" "reset-store!"
               "log-access!" "record-feedback!" "get-helpfulness-ratio"
               "get-entries" "update-metadata!"
               "update-staleness!" "get-stale-entries" "propagate-staleness!"
               "target-collection-for" "relocate-entry!"
               "asof-entry" "history-entry" "asof-query" "between-query"]
              (str "protocol-method re-export: a defn (not a def alias, so an impl extended "
                   "after load is still dispatched) that calls the hive-spi.memory.ports method "
                   "of the same name and nothing else. Its contract is the port's, executable as "
                   "hive-spi.memory.conformance; the host fns that compute over these methods "
                   "(active-store-status, get-entries-projected, the role predicates) carry "
                   "the m/=> contracts")))

;; From hive-mcp.swarm.store-contract-coverage-test.
(def ^:private swarm-store-exemptions
  {'hive-mcp.swarm.datalevin.driver/q
   "Passthrough to datalevin.core/q: the result shape is chosen by the
    query's :find spec (a set of tuples, a scalar, a collection, a pulled map),
    so no schema states the domain; its argument list is query-then-inputs,
    which is datalevin's own contract."})

;; Residue no batch owned (COV-MCP-GATE).
(def ^:private swarm-bootstrap-exemptions
  (exempt-all "hive-mcp.swarm.bootstrap.datahike-driver"
              ["create-database" "connect" "transact" "q" "pull" "db" "release"]
              (str "variadic passthrough (requiring-resolve) to "
                   "hive-agent.swarm.bootstrap.datahike-driver, itself a facade over "
                   "datahike.api: the result is a connection, a db value, a tx report or a "
                   "query result chosen by the call, an opaque host value with no stable "
                   "shape, and the argument list is datahike's own contract. "
                   "database-exists?, the one driver fn with a stable shape, is contracted")))

(def ^:private entry-point-exemptions
  {'hive-mcp.server.core/-main
   "Process entry point: CLI strings in, then it blocks in block-until-shutdown! until the JVM stops, so it never returns a value a contract could state. start!, the part with a result, carries the contract."
   'hive-mcp.scripts.aot-boot/compile-boot-closure!
   "-X entry point of the :aot-boot alias: it compiles, prints a report and ends in System/exit, so it never returns. ctor-namespaces, the one part with a result, carries the contract."})

(def ^:private exempt
  (merge extensions-exemptions
         kg-slots-breaker-exemptions
         protocols-memory-exemptions
         swarm-store-exemptions
         swarm-bootstrap-exemptions
         entry-point-exemptions))

(coverage/deftest-contract-coverage hive-mcp-contracts-gate
  {:paths  ["src/hive_mcp"]
   :exempt exempt})

(deftest exemptions-carry-a-reason
  (is (= 51 (count exempt)))
  (is (every? #(> (count %) 40) (vals exempt))))
