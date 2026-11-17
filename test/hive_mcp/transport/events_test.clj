(ns hive-mcp.transport.events-test
  "The event backbone on the :events chain [:nats :local-channel].

   NATS is scripted through hive-mcp.nats.client redefs (an atom says whether
   the broker is up), so nothing connects. The switch sink is the real
   announcement path with only the sense shout captured."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.nats.client :as client]
            [hive-mcp.protocols.event-backbone :as eb]
            [hive-mcp.transport.events :as events]
            [hive-mcp.transport.registry :as reg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private up (atom false))
(def ^:private published (atom []))
(def ^:private senses (atom []))

(def ^:private fast {:retry {:attempts 1 :backoff-ms [0]} :probe-interval-ms 0})

(defn- fixture
  [t]
  (reg/clear!)
  (reset! up false)
  (reset! published [])
  (reset! senses [])
  (let [prev-bb   (when (eb/backbone-set?) (eb/get-backbone))
        announce  @#'events/announce-switch!
        prev-sink (reg/set-switch-sink! (fn [sw] (announce nil sw)))]
    (with-redefs [client/connected? (fn [] @up)
                  client/publish!   (fn [subject payload] (swap! published conj [subject payload]))
                  client/subscribe! (fn [_ _] ::sub)
                  events/emit-sense! (fn [sw] (swap! senses conj sw))]
      (reg/rebind! {:defaults fast :events {:chain [:nats :local-channel]}})
      (eb/set-backbone! (events/chain-backbone))
      (try (t)
           (finally
             (reg/set-switch-sink! prev-sink)
             (reg/clear!)
             (if prev-bb (eb/set-backbone! prev-bb) (eb/clear-backbone!)))))))

(use-fixtures :each fixture)

(defn- view [] (get-in (reg/status) [:ports :events]))

(deftest nats-down-at-boot-then-up-switches-to-nats
  (let [bb (eb/get-backbone)]
    (testing "boot with the broker down: the chain serves :local-channel and says so"
      (is (false? (eb/connected? bb)))
      (is (= :local-channel (:active (view))))
      (is (= :fallback (:resolved-by (view))))
      (let [s (eb/backbone-status)]
        (is (true? (:degraded? s)))
        (is (false? (:working? s)))
        (is (re-find #"DEGRADED" (:summary s)))
        (is (= [:nats :local-channel] (get-in s [:chain :chain]))))
      (eb/publish! bb "hive.v1.test" {:a 1})
      (is (empty? @published) "nothing reaches NATS while it is down"))
    (testing "the broker comes up: a probe hands :active back to :nats"
      (reset! up true)
      (reg/probe-port! :events {:force? true})
      (is (= :nats (:active (view))))
      (is (= :primary (:resolved-by (view))))
      (eb/publish! bb "hive.v1.test" {:a 2})
      (is (= [["hive.v1.test" {:a 2}]] @published))
      (let [s (eb/backbone-status)]
        (is (true? (:working? s)))
        (is (false? (:degraded? s)))
        (is (= :nats (:backbone-id s)))))))

(deftest nats-drop-switches-to-local-channel-and-announces
  (let [bb (eb/get-backbone)]
    (reset! up true)
    (is (true? (eb/connected? bb)))
    (is (= :nats (:active (view))))
    (reset! senses [])
    (reset! up false)
    (eb/publish! bb "hive.v1.test" {:a 3})
    (testing "the drop moves :active to :local-channel"
      (is (= :local-channel (:active (view))))
      (is (true? (:degraded? (eb/backbone-status)))))
    (testing "the switch is recorded and emitted as a sense"
      (let [sw (last (:switches (view)))]
        (is (= [:nats :local-channel :failover] ((juxt :from :to :reason) sw))))
      (is (= 1 (count @senses)))
      (is (= :events (:port (first @senses)))))
    (testing "the broker returns: recovery is announced too"
      (reset! up true)
      (reg/probe-port! :events {:force? true})
      (is (= :nats (:active (view))))
      (is (= :recovered (:reason (last @senses)))))))

(deftest named-slots-map-to-their-own-ports
  (is (= :events (:port (eb/chain-status (events/chain-backbone)))))
  (is (= :events (:port (eb/chain-status (events/chain-backbone :default)))))
  (is (= :events/kafka (:port (events/chain-backbone :kafka)))))

(deftest unregistered-slot-is-an-addressing-error-not-a-crash
  (let [bb (events/chain-backbone :missing)]
    (is (nil? (eb/publish! bb "s" {})))
    (is (false? (eb/connected? bb)))
    (is (nil? (eb/subscribe! bb "s" identity)))))
