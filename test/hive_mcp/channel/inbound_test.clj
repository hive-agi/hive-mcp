(ns hive-mcp.channel.inbound-test
  "An operator's answer sent over the WebSocket channel reaches the ask.

   Why: the channel was started without :on-message, so Emacs' typed answer
   to a hivemind ask was read and dropped and the agent sat out its timeout."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.inbound :as inbound]
            [hive-mcp.hivemind.messaging :as messaging]
            [hive-mcp.server.transport.ws-channel :as ws-channel]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest reads-the-answer-off-an-emacs-message
  (testing "keyword keys, as the WebSocket channel parses JSON"
    (is (= ["ask-1" "use v2, keep a v1 view" "human"]
           (inbound/hivemind-response {:type "hivemind-response" :ask-id "ask-1"
                                       :decision "use v2, keep a v1 view" :by "human"}))))
  (testing "string keys, as the bencode channel keeps them"
    (is (= ["ask-1" "yes" "human"]
           (inbound/hivemind-response {"type" "hivemind-response" "ask-id" "ask-1"
                                       "answer" "yes"}))))
  (testing "answer wins over decision; by defaults to human"
    (is (= ["a" "typed" "human"]
           (inbound/hivemind-response {:type "hivemind-response" :ask-id "a"
                                       :answer "typed" :decision "old"})))))

(deftest ignores-what-is-not-an-answer
  (is (nil? (inbound/hivemind-response {:type "ping"})))
  (is (nil? (inbound/hivemind-response {:type "hivemind-response" :ask-id "a"})))
  (is (nil? (inbound/hivemind-response {:type "hivemind-response" :ask-id "a" :decision "  "})))
  (is (nil? (inbound/hivemind-response {:type "hivemind-response" :decision "x"})))
  (is (nil? (inbound/hivemind-response "not a map"))))

(deftest delivers-to-respond-ask
  (let [calls (atom [])]
    (with-redefs [messaging/respond-ask! (fn [& args] (swap! calls conj args) true)]
      (is (true? (inbound/handle-client-message!
                  {:type "hivemind-response" :ask-id "ask-7" :decision "go ahead"} "ws-1")))
      (is (nil? (inbound/handle-client-message! {:type "ping"} "ws-1")))
      (is (= [["ask-7" "go ahead" :by "human"]] (vec @calls))))))

(deftest a-failing-delivery-never-throws
  (with-redefs [messaging/respond-ask! (fn [& _] (throw (Exception. "boom")))]
    (is (nil? (inbound/handle-client-message!
               {:type "hivemind-response" :ask-id "a" :decision "x"} "ws-1")))))

(deftest the-channel-starts-with-the-inbound-handler
  (is (= inbound/handle-client-message! (:on-message (ws-channel/channel-opts 9999))))
  (is (= 9999 (:port (ws-channel/channel-opts 9999)))))
