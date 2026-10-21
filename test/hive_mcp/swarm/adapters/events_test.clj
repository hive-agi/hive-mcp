(ns hive-mcp.swarm.adapters.events-test
  "Facets for hive-mcp.swarm.adapters.events.

   The adapter reaches the hook registry through the seam the host already
   owns (hive-mcp.swarm.sync/set-hooks-registry!), so the hook trigger is
   driven with a real hooks.core registry injected there and restored after:

     golden    trigger-hooks with no registry, with a registry whose handlers
               answer and throw, and an event with no handler; the no-throw
               degraded answers of dispatch!, dag-status and the publishers
     port      IEventDispatch / IHookTrigger / IDagWaveScheduler law suite
               with the adapter injected"
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.hooks.core :as hooks]
            [hive-mcp.swarm.adapters.events :as events]
            [hive-mcp.swarm.sync :as sync]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-spi.swarm.ports.events :as ev]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (let [before (sync/get-hooks-registry)]
      (sync/set-hooks-registry! nil)
      (try (f) (finally (sync/set-hooks-registry! before))))))

(defn- hook-answers
  "trigger-hooks through the adapter, without and with an injected registry.
   A throwing handler is reported by hooks.core as {:error e}; only its
   message is kept, so the snapshot is stable."
  []
  (let [a        (events/make-adapter)
        no-reg   (ev/trigger-hooks a :task-complete {:task "t"})
        registry (hooks/create-registry)]
    (hooks/register-hook registry :task-complete (fn [ctx] {:seen (:task ctx)}))
    (hooks/register-hook registry :task-complete (fn [_] (throw (ex-info "boom" {}))))
    (sync/set-hooks-registry! registry)
    {:no-registry no-reg
     :answered    (mapv #(if-let [e (:error %)] {:error (ex-message e)} %)
                        (ev/trigger-hooks a :task-complete {:task "t"}))
     :no-handler  (ev/trigger-hooks a :session-end {:task "t"})}))

(deftest-golden trigger-hooks-golden
  "test/golden/hive-mcp/swarm/adapters/events-trigger-hooks.edn"
  (hook-answers))

(deftest-golden degraded-answers-golden
  "test/golden/hive-mcp/swarm/adapters/events-degraded.edn"
  (let [a (events/make-adapter)]
    {:dispatch-unregistered (ev/dispatch! a [:f00/no-such-event {}])
     :handler-registered?   (ev/handler-registered? a :f00/no-such-event)
     :dag-status-keys       (vec (sort (keys (ev/dag-status a))))
     :dag-active?           (:active (ev/dag-status a))
     :noop-start-dag        (ev/start-dag! ev/noop "p" {})
     :vessel-context        (ev/resolve-agent-context a "f00-no-such-agent")}))

(deftest install-puts-the-adapter-in-the-slot-test
  (try
    (let [a (events/install!)]
      (is (identical? a (ev/get-events)))
      (is (not (identical? ev/noop (ev/get-events)))))
    (finally (ev/clear-events!))))

(deftest-port-contract events-port-laws
  {:port/methods
   {ev/handler-registered? {:in [:cat [:qualified-keyword {:namespace :f00}]] :out :boolean
                            :rel (fn [_ out] (false? out))}
    ev/dispatch!           {:in [:cat [:tuple [:qualified-keyword {:namespace :f00}] :map]] :out :nil}
    ev/trigger-hooks       {:in [:cat [:enum :task-complete :session-end :file-changed] :map]
                            :out [:sequential :any]
                            :rel (fn [_ out] (= [] out))}
    ev/dag-status          {:in [:cat] :out [:map [:active :boolean]]}}}
  (events/make-adapter))
