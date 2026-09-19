;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.adapters.messaging-emit-test
  "IFrontendPush/emit! on the hive-mcp messaging adapter.

   The port promises one verb over possibly two transports and no throw.
   The adapter is the boundary where the port meets the two concrete
   transports, so the transports' own entry points are what gets recorded
   here: both are plain functions the adapter calls through their vars."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-spi.swarm.ports.messaging :as spi]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.channel.websocket :as ws]
            [hive-mcp.swarm.adapters.messaging :as messaging]))

(defn- recording
  "A transport entry point that appends [LEG event-type data] to CALLS."
  [calls leg]
  (fn [event-type data]
    (swap! calls conj [leg event-type data])
    :transport-return-value))

(defn- throwing
  [_event-type _data]
  (throw (ex-info "transport down" {})))

(deftest emit-reaches-both-transports
  (let [calls (atom [])]
    (with-redefs [channel/emit-event! (recording calls :channel)
                  ws/emit! (recording calls :websocket)]
      (let [ret (spi/emit! (messaging/make-adapter) :agora/turn {:dialogue-id "d1"})]
        (testing "the port's emit! returns nil, never a transport's value"
          (is (nil? ret)))
        (testing "each transport receives the event once, channel first"
          (is (= [[:channel :agora/turn {:dialogue-id "d1"}]
                  [:websocket :agora/turn {:dialogue-id "d1"}]]
                 @calls)))))))

(deftest a-failing-transport-neither-throws-nor-silences-the-other
  (testing "channel leg down: the websocket still gets the event"
    (let [calls (atom [])]
      (with-redefs [channel/emit-event! throwing
                    ws/emit! (recording calls :websocket)]
        (is (nil? (spi/emit! (messaging/make-adapter) :agora/created {:n 1})))
        (is (= [[:websocket :agora/created {:n 1}]] @calls)))))
  (testing "websocket leg down: the channel still gets the event"
    (let [calls (atom [])]
      (with-redefs [channel/emit-event! (recording calls :channel)
                    ws/emit! throwing]
        (is (nil? (spi/emit! (messaging/make-adapter) :agora/created {:n 1})))
        (is (= [[:channel :agora/created {:n 1}]] @calls)))))
  (testing "both legs down: still nil, still no throw"
    (with-redefs [channel/emit-event! throwing
                  ws/emit! throwing]
      (is (nil? (spi/emit! (messaging/make-adapter) :agora/created {:n 1}))))))
