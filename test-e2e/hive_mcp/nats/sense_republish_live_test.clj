(ns hive-mcp.nats.sense-republish-live-test
  "Sense republish against a REAL throwaway nats-server -js container
   (hive-mcp.nats.inbox-live-test/with-broker). Tagged :integration; skipped
   without docker."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.nats.inbox-live-test :as live]
            [hive-mcp.nats.sense-adapters :as adapters]
            [hive-mcp.nats.sense-republish :as sr]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord ListFeed [ls*]
  sr/ISenseFeed
  (feed-id [_] :list)
  (subscribe! [_ k f] (swap! ls* assoc k f) true)
  (unsubscribe! [_ k] (swap! ls* dissoc k) nil))

(defn- sense [cls id parent]
  {:sense/id id :sense/class cls :sense/agent "ling-a" :sense/project "hive"
   :sense/parent parent :sense/text (str cls) :sense/at 1})

(deftest ^:e2e senses-land-in-the-parent-inbox-once
  (live/with-broker
    (fn [conn]
      (let [pub   (adapters/nats-inbox-publisher (constantly conn))
            route #(sr/inbox-for {"coordinator:abc" "pedro"} %)
            r     (sr/republisher (->ListFeed (atom {})) pub {:resolve-inbox route})]
        (inbox/arm! conn "pedro")
        (testing "terminal and attention senses arrive; progress does not"
          (doseq [[c id] [[:sense/ask "a1"] [:sense/progress "p1"] [:sense/completed "c1"]]]
            (sr/republish! r (sense c id "coordinator:abc-hive")))
          (let [msgs (:messages (inbox/read! conn "pedro" {:n 10}))]
            (is (= ["sense-a1" "sense-c1"] (mapv :id msgs)))
            (is (= ["ask" "completed"] (mapv #(get-in % [:body :class]) msgs)))
            (is (= "sixth-sense" (:from (first msgs))))))
        (testing "a restarted republisher resending the same sense is stored once"
          (let [fresh (sr/republisher (->ListFeed (atom {})) pub {:resolve-inbox route})]
            (is (= :duplicate (:outcome (sr/republish! fresh (sense :sense/completed "c1" "coordinator:abc")))))
            (is (= [] (:messages (inbox/read! conn "pedro"))))))
        (testing "a ling parent reads the inbox of its own name"
          (inbox/arm! conn "ling-parent")
          (sr/republish! r (sense :sense/error "e1" "ling-parent"))
          (is (= ["sense-e1"] (mapv :id (:messages (inbox/read! conn "ling-parent"))))))))))

(deftest ^:e2e a-closed-connection-degrades
  (live/with-broker
    (fn [conn]
      (let [state (atom conn)
            r     (sr/republisher (->ListFeed (atom {}))
                                  (adapters/nats-inbox-publisher #(deref state))
                                  {:resolve-inbox (constantly "pedro")})]
        (reset! state nil)
        (is (= :degraded (:outcome (sr/republish! r (sense :sense/blocked "b1" "x")))))
        (reset! state conn)
        (is (= :sent (:outcome (sr/republish! r (sense :sense/blocked "b2" "x")))))))))
