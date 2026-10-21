(ns hive-mcp.hivemind.event-registry-test
  "Facets for the event-registry compat shim: its fns delegate to the
   same-named vars of hive-agent.swarm.hivemind.event-registry, except
   event-type-schema, which answers a JSON schema itself (with the :enum
   only when the library is loaded). Offline (this tree) a delegating call
   raises addon-missing naming the var it reached for; the golden, property
   and mutants pin that. The registry itself is driven in test-swarm/."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.hivemind.event-registry :as er]
            [hive-mcp.test.shim-facets :as sf]
            [hive-test.golden :as golden]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shim 'hive-mcp.hivemind.event-registry)
(def ^:private target "hive-agent.swarm.hivemind.event-registry")
(def ^:private overrides
  '{event-type-schema {:returned "clojure.lang.PersistentArrayMap"}})
(def ^:private golden-path "test/golden/hive-mcp/hivemind/event-registry-delegation.edn")

(golden/deftest-golden event-registry-delegation-golden
  golden-path
  (sf/offline-golden 'hive-mcp.hivemind.event-registry-test golden-path
                     #(sf/delegation-table shim (sf/contracted shim))))

(defspec every-event-registry-call-reaches-its-namesake 100
  (prop/for-all [[sym args] (sf/call-gen shim (sf/contracted shim))]
    (sf/reaches-expected? shim target overrides sym args)))

(deftest event-type-schema-omits-the-enum-offline
  (if (sf/offline?)
    (is (= {:type "string" :description "d"} (er/event-type-schema "d")))
    (is (seq (:enum (er/event-type-schema "d"))))))

;; Offline only: with the library present the call does real work.
(when (sf/offline?)
  (hmut/deftest-mutations severity-delegation-mutations
    hive-mcp.hivemind.event-registry/severity
    (sf/delegation-mutants shim 'severity 'slave-status)
    (sf/delegation-check shim 'severity (sf/expected target overrides 'severity))))
