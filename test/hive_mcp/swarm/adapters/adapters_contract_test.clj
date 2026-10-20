(ns hive-mcp.swarm.adapters.adapters-contract-test
  "Contract facets for the hive-mcp adapters behind the swarm host ports.

   Each adapter is a reify over host namespaces, so its facet is a law suite:
   the adapter satisfies its port, install! puts THAT adapter in the slot,
   and the methods that degrade (no host, no handler, no scope) answer the
   port's documented shapes without throwing. Every test owns the port slots
   it drives: the fixture clears all five slots and the piggyback sink
   before and after, so goldens do not depend on test order. Nothing here
   delivers to a real agent.

   Also the coverage gate for src/hive_mcp/swarm/adapters (inbox included)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.adapters.agent-context :as agent-context]
            [hive-mcp.swarm.adapters.boot :as boot]
            [hive-mcp.swarm.adapters.events :as events]
            [hive-mcp.swarm.adapters.inbox.piggyback :as piggyback]
            [hive-mcp.swarm.adapters.ling-host :as ling-host]
            [hive-mcp.swarm.adapters.memory-scope :as memory-scope]
            [hive-mcp.swarm.adapters.messaging :as messaging]
            [hive-mcp.swarm.adapters.store-hooks :as store-hooks]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :as hst]
            [hive-spi.swarm.ports.agent-context :as ac]
            [hive-spi.swarm.ports.events :as ev]
            [hive-spi.swarm.ports.ling-host :as lh]
            [hive-spi.swarm.ports.memory-scope :as ms]
            [hive-spi.swarm.ports.messaging :as msg]
            [hive-test.golden :as golden]
            [hive-test.mutation :as mut]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- clear-ports! []
  (ac/clear-agent-context!)
  (ev/clear-events!)
  (lh/clear-ling-host!)
  (ms/clear-memory-scope!)
  (msg/clear-messaging!)
  (msg/unregister-sink! :piggyback))

(use-fixtures :each (fn [f] (clear-ports!) (try (f) (finally (clear-ports!)))))

;;; ---------------------------------------------------------------------------
;;; Each adapter satisfies its port and install! puts it in the slot
;;; ---------------------------------------------------------------------------

(deftest adapters-satisfy-their-ports-test
  (is (m/validate agent-context/Adapter (agent-context/make-adapter)))
  (is (m/validate events/Adapter (events/make-adapter)))
  (is (m/validate ling-host/Adapter (ling-host/make-adapter)))
  (is (m/validate memory-scope/Adapter (memory-scope/make-adapter)))
  (is (m/validate messaging/Adapter (messaging/make-adapter)))
  (is (m/validate piggyback/Sink (piggyback/make-sink))))

(deftest install-fills-each-slot-test
  (testing "agent-context: installed, then uninstalled back to the noop"
    (let [a (agent-context/install!)]
      (is (ac/agent-context-set?))
      (is (identical? a (ac/get-agent-context)))
      (is (nil? (agent-context/uninstall!)))
      (is (not (ac/agent-context-set?)))))
  (testing "events"
    (is (identical? (events/install!) (ev/get-events)))
    (is (ev/events-set?)))
  (testing "ling-host"
    (is (identical? (ling-host/install!) (lh/get-ling-host)))
    (is (lh/ling-host-set?)))
  (testing "memory-scope"
    (is (identical? (memory-scope/install!) (ms/get-memory-scope)))
    (is (ms/memory-scope-set?)))
  (testing "messaging"
    (is (identical? (messaging/install!) (msg/get-messaging)))
    (is (msg/messaging-set?)))
  (testing "piggyback sink registers under its id"
    (let [s (piggyback/install!)]
      (is (some #(identical? s %) (msg/sinks))))))

(mut/deftest-mutations install!-mutants-caught
  hive-mcp.swarm.adapters.events/install!
  [["installs-nothing" (fn [] (events/make-adapter))]
   ["installs-the-noop" (fn [] (ev/set-events! ev/noop))]]
  (fn []
    (ev/clear-events!)
    (let [a (events/install!)]
      (is (ev/events-set?))
      (is (identical? a (ev/get-events)))
      (is (not (identical? ev/noop (ev/get-events))) "the host adapter, not the noop"))))

;;; ---------------------------------------------------------------------------
;;; Degraded answers: no host, no handler, no scope
;;; ---------------------------------------------------------------------------

(defn- degraded []
  (let [e (events/make-adapter)
        l (ling-host/make-adapter)
        s (piggyback/make-sink)]
    {:events/handler-registered? (ev/handler-registered? e :no-such/event)
     :events/dispatch-unregistered (ev/dispatch! e [:no-such/event {}])
     :events/trigger-hooks-without-registry (ev/trigger-hooks e :task-complete {})
     :ling-host/readiness-unknown-ling (select-keys (lh/wait-for-ling-ready l "no-such-ling" :bogus-mode)
                                                    [:ready? :phase])
     :sink/id (msg/sink-id s)
     :sink/accepts (mapv #(msg/accepts? s %) ["ling-a" "" "   " nil :kw])
     :sink/priority (:inbox/priority (meta s))}))

(golden/deftest-golden degraded-answers-golden
  "test/golden/hive-mcp/swarm/adapters/degraded.edn"
  (degraded))

(deftest messaging-frontend-status-shape-test
  (let [st (msg/frontend-status (messaging/make-adapter))]
    (is (m/validate [:map [:channel-connected? :boolean] [:ws-connected? :boolean]
                     [:ws-clients :int]]
                    st))))

;;; ---------------------------------------------------------------------------
;;; store-hooks: scope-rows is pure
;;; ---------------------------------------------------------------------------

(def ^:private Rows [:vector {:max 4} [:map [:k [:enum "s1" "s2"]]]])

(hst/deftrifecta-from-schema scope-rows-passes-through-without-a-session
  hive-mcp.swarm.adapters.store-hooks/scope-rows
  {:in [:cat [:map [:session-ref {:optional true} [:enum nil "" {} {:session-id nil}]]
              [:parent-of {:optional true} [:maybe [:map-of :string :string]]]]
        [:enum :k]
        Rows]
   :out [:sequential :map]
   :rel (fn [[_ _ rows] out] (= out rows))
   :golden-path "test/golden/hive-mcp/swarm/adapters/scope-rows.edn"})

(deftest store-hooks-install-test
  (let [hooks (store-hooks/install!)]
    (is (m/validate [:maybe store-hooks/Hooks] hooks))
    (when hooks (is (identical? store-hooks/scope-rows (:scope-rows hooks))))))

;;; ---------------------------------------------------------------------------
;;; boot
;;; ---------------------------------------------------------------------------

(deftest boot-installs-every-slot-test
  (is (= :installed (boot/install-all!)))
  (is (every? true? [(ac/agent-context-set?) (ev/events-set?) (lh/ling-host-set?)
                     (ms/memory-scope-set?) (msg/messaging-set?)]))
  (is (some #(= :piggyback (msg/sink-id %)) (msg/sinks)))
  (is (= :installed (boot/ensure!))))

;;; ---------------------------------------------------------------------------
;;; schemas and the gate
;;; ---------------------------------------------------------------------------

(deftest adapter-schemas-generate-test
  (doseq [s [agent-context/Adapter events/Adapter ling-host/Adapter memory-scope/Adapter
             messaging/Adapter piggyback/Sink piggyback/Receipt store-hooks/Hooks]]
    (is (seq (mg/sample s {:size 2})) (pr-str (m/form s)))))

(deftest every-adapter-contract-generates-both-sides-test
  (doseq [ns-sym '[hive-mcp.swarm.adapters.agent-context hive-mcp.swarm.adapters.events
                   hive-mcp.swarm.adapters.ling-host hive-mcp.swarm.adapters.memory-scope
                   hive-mcp.swarm.adapters.messaging hive-mcp.swarm.adapters.inbox.piggyback
                   hive-mcp.swarm.adapters.store-hooks hive-mcp.swarm.adapters.boot]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          :let [s (m/schema schema)]
          arity (if (= :function (m/type s)) (m/children s) [s])
          side (take 2 (m/children arity))]
    (is (seq (mg/sample side {:size 2 :seed 7})) (str ns-sym "/" fn-sym))))

(deftest-contract-coverage swarm-adapters-contracts-gate
  {:paths ["src/hive_mcp/swarm/adapters"]
   :exempt {}})
