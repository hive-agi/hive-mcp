(ns hive-mcp.hivemind.messaging.shout-unknown-event-test
  "A shout with an event type the registry does not know leaves the slave's
   status alone.

   Why: event-registry/slave-status defaults an unknown type to :idle, so a
   ling that shouted a typo'd :working mid-task was reported as available.
   shout-slave-status now answers nil (no transition) for an unregistered
   type, the same answer it gives a :status-neutral? shout.

   Needs the swarm addon on the classpath (the registry delegates to
   hive-agent), hence test-swarm/."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.hivemind.messaging :as msg]
            [hive-test.trifecta :refer [deftrifecta]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private unknown-types
  [:working :running :thinking :idle "working"])

(defn status-for
  "Subject: one shout case -> the status it would set."
  [{:keys [event-type data]}]
  (msg/shout-slave-status event-type data))

(deftrifecta shout-slave-status-contract
  hive-mcp.hivemind.messaging.shout-unknown-event-test/status-for
  {:golden-path "test/golden/hivemind/shout-unknown-event.edn"
   :cases       {:started        {:event-type :started :data {}}
                 :progress       {:event-type :progress :data {}}
                 :blocked        {:event-type :blocked :data {}}
                 :completed      {:event-type :completed :data {}}
                 :string-started {:event-type "started" :data {}}
                 :neutral        {:event-type :blocked :data {:status-neutral? true}}
                 :unknown        {:event-type :working :data {}}
                 :unknown-string {:event-type "running" :data {}}}
   :gen         (gen/hash-map :event-type (gen/elements unknown-types)
                              :data (gen/elements [{} {:message "m"}]))
   :pred        nil?
   :num-tests   50
   :mutations   [["registry default — unknown type falls through to :idle"
                  (fn [_] :idle)]]})

(deftest a-known-type-still-transitions
  (testing "registered types keep their mapped status"
    (is (= :working (msg/shout-slave-status :progress {})))
    (is (= :blocked (msg/shout-slave-status :blocked {:message "stuck"})))))
