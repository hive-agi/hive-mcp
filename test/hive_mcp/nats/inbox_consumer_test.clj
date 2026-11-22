(ns hive-mcp.nats.inbox-consumer-test
  "hive-mcp.nats.inbox-consumer through its ports: an in-test broker with
   explicit ack/nak and a stand-in for hive-agent.loop.inbox (the same
   open/closed offer! contract, without depending on hive-agent). No NATS,
   no with-redefs. Acceptance of card 20261116182711-40333bab: a message
   offered to a live run is taken (and acked); an ended run refuses it and
   the message stays unacked; consuming wakes within 2 s."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.nats.inbox-consumer :as ic]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- broker
  "Messages stay in `q*` until acked; a nak makes them fetchable again."
  []
  (let [q* (atom (sorted-map)) inflight* (atom #{}) acked* (atom []) naks* (atom 0)
        arms* (atom 0)]
    {:q* q* :acked* acked* :naks* naks* :arms* arms*
     :send! (fn [seq body] (swap! q* assoc seq {:seq seq :id (str "m" seq) :body body}))
     :arm!  (fn [_] (swap! arms* inc) {:armed? true})
     :source
     (fn [_ {:keys [n wait-ms]}]
       (let [ready (->> @q* vals (remove #(@inflight* (:seq %))) (take n) vec)]
         (if (empty? ready)
           (do (Thread/sleep (long (min 10 (or wait-ms 10)))) [])
           (do (swap! inflight* into (map :seq ready))
               (mapv (fn [msg]
                       {:message msg
                        :ack! (fn [] (swap! q* dissoc (:seq msg)) (swap! inflight* disj (:seq msg))
                                (swap! acked* conj (:seq msg)) true)
                        :nak! (fn [] (swap! naks* inc) (swap! inflight* disj (:seq msg)) true)})
                     ready)))))}))

;; hive-agent.loop.inbox's contract, restated
(defn- run-inbox [] (atom {:open? true :msgs []}))
(defn- offer! [inbox msg]
  (let [[before _] (swap-vals! inbox (fn [{:keys [open?] :as s}]
                                       (if open? (update s :msgs conj msg) s)))]
    (boolean (:open? before))))

(defn- wait-until [pred ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (while (and (not (pred)) (< (System/currentTimeMillis) deadline)) (Thread/sleep 5))
    (pred)))

(defn- opts [b & {:as more}]
  (merge {:source (:source b) :arm! (:arm! b) :wait-ms 10 :retry-ms 10 :nak-ms 10} more))

(use-fixtures :each (fn [f] (ic/stop-all!) (try (f) (finally (ic/stop-all!)))))

(deftest verdicts
  (is (= :ack (ic/verdict true)))
  (is (= :ack (ic/verdict nil)))
  (is (= :ack (ic/verdict {:x 1})))
  (is (= :nak (ic/verdict :nak)))
  (is (= :stop (ic/verdict :stop))))

(deftest a-live-run-takes-each-message-in-order-and-it-is-acked
  (let [b (broker) inbox (run-inbox)]
    (doseq [i [1 2 3]] ((:send! b) i {:n i}))
    (let [stop! (ic/consume! "ling-a" (ic/offer-handler #(offer! inbox %)) (opts b))]
      (is (wait-until #(= [1 2 3] @(:acked* b)) 2000))
      (is (= [1 2 3] (mapv (comp :n :body) (:msgs @inbox))))
      (is (= 1 @(:arms* b)) "armed once")
      (testing "a message sent to the idle inbox is handed over within 2 s"
        (let [t0 (System/currentTimeMillis)]
          ((:send! b) 4 {:n 4})
          (is (wait-until #(= 4 (count (:msgs @inbox))) 2000))
          (is (< (- (System/currentTimeMillis) t0) 2000))))
      (let [st (first (ic/consumers))]
        (is (m/validate ic/ConsumerStatus st))
        (is (= "ling-a" (:inbox st)))
        (is (= 4 (:acked st))))
      (is (nil? (stop!)))
      (is (empty? (ic/consumers))))))

(deftest an-ended-run-refuses-and-the-message-stays-unacked
  (let [b (broker) inbox (run-inbox) stopped (promise)]
    (swap! inbox assoc :open? false)
    ((:send! b) 1 {:n 1})
    ((:send! b) 2 {:n 2})
    (ic/consume! "ling-b" (ic/offer-handler #(offer! inbox %))
                 (opts b :on-stop #(deliver stopped %)))
    (is (= :handler (deref stopped 2000 ::timeout)) "the consumer stopped itself")
    (is (empty? @(:acked* b)))
    (is (= 2 @(:naks* b)) "the refused message and the rest of its batch")
    (is (= [1 2] (keys @(:q* b))) "both wait in the durable for the next reader")
    (testing "a new run's consumer gets them"
      (let [inbox2 (run-inbox)
            stop!  (ic/consume! "ling-b" (ic/offer-handler #(offer! inbox2 %)) (opts b))]
        (is (wait-until #(= [1 2] @(:acked* b)) 2000))
        (stop!)))))

(deftest a-nak-or-throw-is-redelivered-after-a-backoff
  (let [b (broker) seen (atom [])]
    ((:send! b) 1 :x)
    (let [stop! (ic/consume! "ling-c"
                             (fn [d] (swap! seen conj (:seq d))
                               (case (count @seen) 1 :nak 2 (throw (ex-info "boom" {})) :ack))
                             (opts b))]
      (is (wait-until #(= [1] @(:acked* b)) 2000))
      (is (= [1 1 1] @seen) "nak, throw, then taken")
      (is (= 1 (:failed (first (ic/consumers)))))
      (stop!))))

(deftest no-bus-is-retried-not-fatal
  (let [b (broker) up? (atom false) inbox (run-inbox)]
    ((:send! b) 1 :x)
    (let [stop! (ic/consume! "ling-d" (ic/offer-handler #(offer! inbox %))
                             (opts b :arm! (fn [id] (if @up? ((:arm! b) id)
                                                        (throw (ex-info "no connected event bus" {}))))))]
      (Thread/sleep 50)
      (is (empty? (:msgs @inbox)))
      (is (= "no connected event bus" (:last-error (first (ic/consumers)))))
      (reset! up? true)
      (is (wait-until #(= 1 (count (:msgs @inbox))) 2000))
      (stop!))))

(deftest a-second-consumer-of-one-inbox-replaces-the-first
  (let [b (broker) a (run-inbox) c (run-inbox) stopped (promise)]
    (ic/consume! "ling-e" (ic/offer-handler #(offer! a %)) (opts b :on-stop #(deliver stopped %)))
    (let [stop! (ic/consume! "ling-e" (ic/offer-handler #(offer! c %)) (opts b))]
      (is (= :stopped (deref stopped 2000 ::timeout)))
      ((:send! b) 1 :x)
      (is (wait-until #(= 1 (count (:msgs @c))) 2000))
      (is (empty? (:msgs @a)))
      (is (= 1 (count (ic/consumers))))
      (stop!))))

(deftest a-bad-inbox-id-is-refused-up-front
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"invalid inbox id"
                        (ic/consume! "a.b" identity {}))))
