(ns hive-mcp.nats.inbox-test
  "Unit tests for the pure half of hive-mcp.nats.inbox and the `inbox` verbs'
   refusals. The JetStream half runs in hive-mcp.nats.inbox-live-test."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.tools.consolidated.inbox :as tool]
            [hive-mcp.tools.consolidated.swarm :as swarm]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest inbox-ids-are-one-subject-token
  (is (inbox/valid-inbox-id? "coordinator-s36605"))
  (is (inbox/valid-inbox-id? "A_b-9"))
  (doseq [bad [nil "" "a.b" "a b" "a>" "*" "x/y" (apply str (repeat 65 "a")) 42]]
    (is (not (inbox/valid-inbox-id? bad)) (pr-str bad))))

(deftest naming-is-stable
  (is (= "hive.v1.inbox.alice" (inbox/subject "alice")))
  (is (= "inbox-alice" (inbox/durable "alice")))
  (is (= "HIVE_INBOX" inbox/stream-name)))

(deftest envelope-keeps-a-given-id-and-mints-one-otherwise
  (let [e (inbox/envelope {:to "bob" :body {:x 1} :from "alice" :ts 7})]
    (is (= {:to "bob" :body {:x 1} :from "alice" :ts 7} (dissoc e :id)))
    (is (string? (:id e))))
  (is (= "m-1" (:id (inbox/envelope {:id "m-1" :to "bob" :body "hi"}))))
  (is (not (contains? (inbox/envelope {:to "bob" :body "hi"}) :from))))

(deftest decode-round-trips-an-envelope-and-keeps-foreign-payloads
  (let [env (inbox/envelope {:id "m-2" :to "bob" :from "alice" :body {:k "v"} :ts 9})
        d   (inbox/decode (.getBytes ^String (json/write-str env) "UTF-8") 5)]
    (is (= {:seq 5 :id "m-2" :from "alice" :body {:k "v"} :ts 9} d)))
  (is (= {:seq 3 :body "plain text"} (inbox/decode (.getBytes "plain text" "UTF-8") 3))))

(deftest resolve-inbox-id-precedence
  (is (= "a" (tool/resolve-inbox-id {:inbox "a" :id "b"} "c")))
  (is (= "b" (tool/resolve-inbox-id {:inbox " " :id "b"} "c")))
  (is (= "c" (tool/resolve-inbox-id {} "c")))
  (is (nil? (tool/resolve-inbox-id {} nil))))

(defn- err-text [resp] (when (:isError resp) (:text resp)))

(deftest verbs-refuse-plainly
  (with-redefs [hive-mcp.tools.consolidated.inbox/connection (constantly nil)]
    (testing "no bus"
      (is (re-find #"no connected event bus" (err-text (tool/handle-arm {:inbox "x"}))))
      (is (re-find #"no connected event bus" (err-text (tool/handle-status {})))))
    (testing "missing params are named"
      (is (re-find #"`to`" (err-text (tool/handle-send {:body "x"}))))
      (is (re-find #"`body`" (err-text (tool/handle-send {:to "x"})))))))

(deftest swarm-routes-the-inbox-subdomain
  (is (= #{:arm :send :read :status} (set (keys (:inbox swarm/canonical-handlers))))))
