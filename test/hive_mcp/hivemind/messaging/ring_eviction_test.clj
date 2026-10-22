(ns hive-mcp.hivemind.messaging.ring-eviction-test
  "The per-agent piggyback ring evicts telemetry before a deliberate shout,
   so a burst of per-turn telemetry cannot push a milestone shout out of the
   ring before the coordinator's next read, and the ring stays bounded."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.piggyback :as pb]
            [hive-mcp.hivemind.messaging :as msg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- clean-pb-state [f]
  (let [original-source @pb/message-source-fn]
    (pb/reset-all-cursors!)
    (pb/clear-backbone-buffer!)
    (try (f)
         (finally
           (pb/reset-all-cursors!)
           (pb/clear-backbone-buffer!)
           (pb/register-message-source! original-source)))))

(use-fixtures :each clean-pb-state)

(defn- row [ts text & {:keys [deliberate?]}]
  (cond-> {:event-type :progress :message text :timestamp ts
           :project-id "global" :shout-id (str "s-" ts)}
    deliberate? (assoc :deliberate? true)))

(defn- fill [rows]
  (reduce msg/ring-append [] rows))

(def ^:private milestone (row 1 "milestone reached" :deliberate? true))

(def ^:private telemetry
  (mapv #(row (+ 2 %) (str "bb-ling turn " %)) (range 30)))

(deftest deliberate-survives-30-telemetry-rows-test
  (let [ring (fill (cons milestone telemetry))]
    (testing "the per-agent bound still holds"
      (is (= msg/ring-size (count ring))))
    (testing "the deliberate shout is still in the ring"
      (is (= milestone (first ring))))
    (testing "the rest is the newest telemetry, in order"
      (is (= (take-last (dec msg/ring-size) telemetry) (rest ring))))))

(deftest next-piggyback-read-delivers-the-deliberate-shout-test
  (let [ring (fill (cons milestone telemetry))
        snapshot {"ling-a" {:data {:messages ring}}}]
    (pb/register-message-source! (fn [] (msg/registry->messages snapshot)))
    (let [out (pb/get-messages "coordinator" :project-id "global")]
      (is (some #(= "milestone reached" (:m %)) out)))))

(deftest new-message-is-always-kept-test
  (testing "an all-deliberate ring still admits the new row, oldest evicted"
    (let [rows (mapv #(row % (str "d" %) :deliberate? true) (range 15))
          ring (fill rows)]
      (is (= msg/ring-size (count ring)))
      (is (= (take-last msg/ring-size rows) ring))))
  (testing "a telemetry row arriving at a ring full of deliberate rows is kept"
    (let [delib (mapv #(row % (str "d" %) :deliberate? true) (range 10))
          ring (msg/ring-append delib (row 99 "turn"))]
      (is (= 10 (count ring)))
      (is (= "turn" (:message (peek ring))))
      (is (= (rest delib) (pop ring))))))

(deftest under-the-bound-nothing-is-evicted-test
  (let [rows (mapv #(row % (str "t" %)) (range 5))]
    (is (= rows (fill rows)))))

(deftest bound-holds-for-any-mix-test
  (doseq [seed (range 20)]
    (let [r (java.util.Random. seed)
          rows (mapv #(row % (str %) :deliberate? (.nextBoolean r)) (range 40))
          ring (fill rows)]
      (is (<= (count ring) msg/ring-size))
      (is (= (peek rows) (peek ring)))
      (is (apply < (map :timestamp ring))))))
