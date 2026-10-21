(ns hive-mcp.swarm.adapters.agent-context-test
  "Facets for hive-mcp.swarm.adapters.agent-context.

   The adapter is a reify over hive-mcp.context.request and the budget hook,
   so the port is driven with the request context it delegates to BOUND
   (with-request-context), never redefined:

     property  for any bound agent-id / directory, the adapter answers exactly
               the bound pair, and nil outside any binding
     golden    the budget guardrail round trip (register, deregister, an
               unknown agent) over the host budget hook
     port      the IAgentCallContext law suite with the adapter injected"
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.swarm.adapters.agent-context :as agent-context]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-spi.swarm.ports.agent-context :as ac]
            [hive-test.golden :refer [deftest-golden]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (ac/clear-agent-context!) (try (f) (finally (ac/clear-agent-context!)))))

(def ^:private Bound
  [:map [:agent-id [:string {:min 1}]] [:directory [:string {:min 1}]]])

(defspec adapter-answers-the-bound-request-context 100
  (prop/for-all [{:keys [agent-id directory]} (mg/generator Bound)]
    (let [a (agent-context/make-adapter)]
      (and (= [agent-id directory]
              (ctx/with-request-context (ctx/make-request-ctx {:agent-id agent-id :directory directory})
                [(ac/current-agent-id a) (ac/current-directory a)]))
           (= [nil nil] [(ac/current-agent-id a) (ac/current-directory a)])))))

(defn- budget-round-trip
  "Register then deregister a budget through the adapter; the answers, with
   the timestamp fields dropped so the snapshot is stable."
  []
  (let [a     (agent-context/make-adapter)
        clean #(some-> % (dissoc :registered-at :last-check-at))]
    {:register        (clean (ac/register-budget! a "f00-budget-probe" 1.5 {}))
     :deregister      (clean (ac/deregister-budget! a "f00-budget-probe"))
     :deregister-gone (clean (ac/deregister-budget! a "f00-budget-probe"))
     :noop-register   (ac/register-budget! ac/noop "f00-budget-probe" 1.5 {})}))

(deftest-golden budget-guardrail-round-trip-golden
  "test/golden/hive-mcp/swarm/adapters/agent-context-budget.edn"
  (budget-round-trip))

(deftest install-and-uninstall-test
  (let [a (agent-context/install!)]
    (is (identical? a (ac/get-agent-context)))
    (is (nil? (agent-context/uninstall!)))
    (is (identical? ac/noop (ac/get-agent-context)))))

(deftest-port-contract agent-call-context-port-laws
  {:port/methods
   {ac/current-agent-id  {:in [:cat] :out [:maybe :string]}
    ac/current-directory {:in [:cat] :out [:maybe :string]}}}
  (agent-context/make-adapter))
