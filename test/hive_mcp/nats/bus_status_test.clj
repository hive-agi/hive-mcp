(ns hive-mcp.nats.bus-status-test
  "Bus status must say plainly whether a working bus exists: a NoopBackbone,
   or a NatsBackbone whose connection is down, must never read as working."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-mcp.nats.bus :as bus]
            [hive-mcp.nats.streams :as streams]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord FakeBackbone [id up?]
  eb/IEventBackbone
  (backbone-id [_] id)
  (connected? [_] @up?)
  (publish! [_ _ _] nil)
  (subscribe! [_ _ _] nil)
  (unsubscribe! [_ _] nil))

(use-fixtures :each (fn [t] (eb/clear-backbone!) (t) (eb/clear-backbone!)))

(deftest noop-is-never-working
  (let [s (eb/backbone-status)]
    (is (= :noop (:backbone-id s)))
    (is (false? (:installed? s)))
    (is (false? (:connected? s)))
    (is (false? (:working? s)))
    (is (re-find #"(?i)no event bus" (:summary s)))))

(deftest installed-but-disconnected-is-not-working
  (let [up? (atom false)]
    (eb/set-backbone! (->FakeBackbone :nats up?))
    (testing "down"
      (let [s (eb/backbone-status)]
        (is (= :nats (:backbone-id s)))
        (is (true? (:installed? s)))
        (is (false? (:working? s)))
        (is (re-find #"NOT connected" (:summary s)))))
    (testing "up"
      (reset! up? true)
      (is (true? (:working? (eb/backbone-status)))))))

(deftest bus-status-folds-backbone-client-and-streams
  (let [s (bus/bus-status)]
    (is (contains? s :backbone))
    (is (contains? s :nats))
    (is (contains? s :streams))
    (is (false? (:working? s)) "no backbone installed in a unit JVM")
    (is (= #{"HIVE_EVENTS" "HIVE_INBOX"} (set (map :name streams/default-streams))))))
