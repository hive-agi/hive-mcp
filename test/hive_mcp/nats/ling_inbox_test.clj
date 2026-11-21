(ns hive-mcp.nats.ling-inbox-test
  "The ling inbox's three separate facts and its fail-soft lifecycle, with
   no broker: the inbox fns are redefined, the consumer port is scripted."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.extensions.soft :as soft]
            [hive-mcp.nats.inbox :as inbox]
            [hive-mcp.nats.ling-inbox :as li]
            [hive-mcp.transport.chain :as chain]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest route-separates-bus-inbox-and-consumer
  (with-redefs [inbox/status (fn [_ id] {:inbox id :armed? (= id "ling-1")})]
    (testing "no bus"
      (binding [li/*connection* (constantly nil)
                li/*consumer-attached?* (constantly true)]
        (is (= {:nats-up? false :armed? false :consumer-attached? true :reachable? false}
               (dissoc (li/route "ling-1") :ling-id)))))
    (testing "bus up, inbox armed, no consumer"
      (binding [li/*connection* (constantly :conn)
                li/*consumer-attached?* (constantly false)]
        (is (= {:nats-up? true :armed? true :consumer-attached? false :reachable? false}
               (dissoc (li/route "ling-1") :ling-id)))))
    (testing "all three"
      (binding [li/*connection* (constantly :conn)
                li/*consumer-attached?* #(= "ling-1" %)]
        (is (:reachable? (li/route "ling-1")))
        (is (not (:reachable? (li/route "ling-2"))) "ling-2 is not armed")))
    (testing "an id that is no subject token is never reachable"
      (binding [li/*connection* (constantly :conn)
                li/*consumer-attached?* (constantly true)]
        (is (not (:reachable? (li/route "a b"))))))))

(deftest the-default-consumer-port-asks-hive-agent-softly
  (binding [soft/*resolve* (constantly nil)]
    (is (false? (li/*consumer-attached?* "ling-1")) "no hive-agent: never attached"))
  (binding [soft/*resolve* (fn [sym] (when (= sym 'hive-agent.loop.nats-inbox/attached?)
                                       #(= "ling-1" %)))]
    (is (true? (li/*consumer-attached?* "ling-1")))
    (is (false? (li/*consumer-attached?* "ling-2")))))

(deftest arm-and-disarm-are-fail-soft-and-nil-without-a-bus
  (binding [li/*connection* (constantly nil)]
    (is (nil? (li/arm! "ling-1")))
    (is (nil? (li/disarm! "ling-1"))))
  (let [calls (atom [])]
    (with-redefs [inbox/status  (fn [_ _] {:armed? false})
                  inbox/arm!    (fn [_ id] (swap! calls conj [:arm id]) {:inbox id :armed? true})
                  inbox/disarm! (fn [_ id] (swap! calls conj [:disarm id])
                                  {:inbox id :armed? false :deleted? true})]
      (binding [li/*connection* (constantly :not-a-connection)]
        (is (= {:inbox "ling-1" :armed? true} (li/arm! "ling-1"))
            "the subject purge fails on a fake connection and is rescued")
        (is (:deleted? (li/disarm! "ling-1")))
        (is (= [[:arm "ling-1"] [:disarm "ling-1"]] @calls))))
    (with-redefs [inbox/status (fn [_ _] (throw (ex-info "broker gone" {})))]
      (binding [li/*connection* (constantly :conn)]
        (is (nil? (li/arm! "ling-1")) "a broker failure never fails the spawn")))))

(deftest send-sequence-names-the-last-accepted-send
  (doseq [l ["seq-l1" "seq-l2"]] (li/seq-forget! l))
  (try
    (testing "seqs count up per ling, in one epoch"
      (let [a (li/seq-stamp! "seq-l1")
            c (li/seq-stamp! "seq-l2")]
        (is (= 1 (:seq a) (:seq c)) "each ling has its own sequence")
        (is (= li/seq-epoch (:epoch a)))
        (is (nil? (:after a)) "the first message waits for nothing")
        (li/seq-accepted! "seq-l1" a)
        (let [b (li/seq-stamp! "seq-l1")]
          (is (= [2 1] [(:seq b) (:after b)]))
          (testing "a send that failed is never accepted, so nothing waits for it"
            (is (= 1 (:after (li/seq-stamp! "seq-l1")))))
          (li/seq-accepted! "seq-l1" b)
          (is (= 2 (:after (li/seq-stamp! "seq-l1")))))))
    (testing "acceptance never moves backwards or across epochs"
      (li/seq-accepted! "seq-l1" {:epoch li/seq-epoch :seq 1})
      (li/seq-accepted! "seq-l1" {:epoch "another-host" :seq 99})
      (is (= 2 (:after (li/seq-stamp! "seq-l1")))))
    (testing "forget restarts the ling"
      (li/seq-forget! "seq-l1")
      (is (= {:seq 1 :after nil} (select-keys (li/seq-stamp! "seq-l1") [:seq :after]))))
    (finally (doseq [l ["seq-l1" "seq-l2"]] (li/seq-forget! l)))))

(deftest a-call-served-past-inapplicable-adapters-elects-nothing
  (is (nil? (chain/past-inapplicable-active nil :b [{:adapter :a :reason :inapplicable}])))
  (is (= :a (chain/past-inapplicable-active :a :b [{:adapter :a :reason :inapplicable}])))
  (is (= :b (chain/past-inapplicable-active :a :b [{:adapter :a :reason :failed}]))))
