(ns hive-mcp.swarm.adapters.boot-test
  "Facets for hive-mcp.swarm.adapters.boot.

   install-all! fills every swarm host slot. The golden records, for each
   slot, whether it is set and whether it holds the port's noop, first with
   every slot cleared and then after install-all!, so a boot that skipped a
   slot (or installed a noop) changes the snapshot. The mutation facet
   checks that skipping any single install is caught."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.swarm.adapters.agent-context :as agent-context]
            [hive-mcp.swarm.adapters.boot :as boot]
            [hive-mcp.swarm.adapters.events :as events]
            [hive-mcp.swarm.adapters.ling-host :as ling-host]
            [hive-mcp.swarm.adapters.memory-scope :as memory-scope]
            [hive-mcp.swarm.adapters.messaging :as messaging]
            [hive-spi.swarm.ports.agent-context :as ac]
            [hive-spi.swarm.ports.events :as ev]
            [hive-spi.swarm.ports.ling-host :as lh]
            [hive-spi.swarm.ports.memory-scope :as ms]
            [hive-spi.swarm.ports.messaging :as msg]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as mut]))

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

(defn- slots
  "Each slot: [set? holds-the-noop?], and whether the piggyback sink is registered."
  []
  {:agent-context [(ac/agent-context-set?) (identical? ac/noop (ac/get-agent-context))]
   :events        [(ev/events-set?) (identical? ev/noop (ev/get-events))]
   :ling-host     [(lh/ling-host-set?) (identical? lh/noop (lh/get-ling-host))]
   :memory-scope  [(ms/memory-scope-set?) (identical? ms/noop (ms/get-memory-scope))]
   :messaging     [(msg/messaging-set?) (identical? msg/noop (msg/get-messaging))]
   :piggyback     (boolean (some #(= :piggyback (msg/sink-id %)) (msg/sinks)))})

(deftest-golden install-all-fills-every-slot-golden
  "test/golden/hive-mcp/swarm/adapters/boot-slots.edn"
  (let [before (slots)
        ret    (boot/install-all!)]
    {:before before :returned ret :after (slots)}))

(defn- every-slot-filled? []
  (= {:agent-context [true false] :events [true false] :ling-host [true false]
      :memory-scope [true false] :messaging [true false] :piggyback true}
     (slots)))

(mut/deftest-mutations install-all-mutants-caught
  hive-mcp.swarm.adapters.boot/install-all!
  [["skips-events"    (fn [] (agent-context/install!) (ling-host/install!) (memory-scope/install!)
                        (messaging/install!) :installed)]
   ["skips-messaging" (fn [] (agent-context/install!) (events/install!) (ling-host/install!)
                        (memory-scope/install!) :installed)]
   ["does-nothing"    (constantly :installed)]]
  (fn []
    (clear-ports!)
    (is (= :installed (boot/install-all!)))
    (is (every-slot-filled?))))

(deftest ensure-is-installed-test
  (is (= :installed (boot/ensure!))))
