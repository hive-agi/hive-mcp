(ns hive-mcp.hivemind.messaging.shout-stages-test
  "Golden + property tests for the pure stages `messaging/shout!*` is
   decomposed into (routing extras, ring message, backbone payload, verdict)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.hivemind.messaging :as msg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private directed
  {:project-id "p" :capped-message "m" :capped-task "t" :payload-data {:k 1}
   :parent-id "boss" :to "ling-b" :context-id "ctx-1" :deliberate? true})

(deftest routing-extras-golden
  (is (= {} (msg/shout-routing-extras {})))
  (is (= {:parent-id "boss" :to "ling-b" :context-id "ctx-1" :deliberate? true}
         (msg/shout-routing-extras directed)))
  (is (= {:broadcast? true :broadcast-reason :halt :ref "r"}
         (msg/shout-routing-extras {:broadcast? true :broadcast-reason :halt :payload-ref "r"}))))

(deftest ring-message-and-backbone-payload-agree-on-routing
  (let [ring (msg/shout-ring-message directed :progress "sid" 42)
        bb   (msg/shout-backbone-payload directed "ling-a" :progress "sid" 42)]
    (testing "ring row shape"
      (is (= {:event-type :progress :timestamp 42 :project-id "p" :shout-id "sid"
              :data {:k 1} :task "t" :message "m"
              :parent-id "boss" :to "ling-b" :context-id "ctx-1" :deliberate? true}
             ring)))
    (testing "backbone payload is flat and names the sender"
      (is (= "ling-a" (:agent-id bb)))
      (is (= "m" (:message bb)))
      (is (= "t" (:task bb))))
    (testing "both carry identical routing keys"
      (let [ks [:parent-id :to :context-id :deliberate? :broadcast? :ref]]
        (is (= (select-keys ring ks) (select-keys bb ks)))))))

(deftest ring-message-omits-absent-text
  (let [ring (msg/shout-ring-message {:project-id "p" :payload-data {}} :progress "s" 1)]
    (is (not (contains? ring :task)))
    (is (not (contains? ring :message)))))

(deftest verdict-golden
  (is (= {:delivered true :shout-id "s" :routing :spawner}
         (msg/shout-verdict {} "s")))
  (is (= {:delivered true :shout-id "s" :routing :direct :to "x" :context-id "c"}
         (msg/shout-verdict {:to "x" :context-id "c"} "s")))
  (is (= {:delivered true :shout-id "s" :routing :broadcast :broadcast-refused :budget-exhausted}
         (msg/shout-verdict {:broadcast? true :broadcast-refused :budget-exhausted} "s")))
  (is (= "ghost" (:to-unresolved (msg/shout-verdict {:to "ghost" :to-unresolved? true} "s")))))

(defspec verdict-routing-precedence 100
  (prop/for-all [to (gen/one-of [(gen/return nil) gen/string-alphanumeric])
                 bcast gen/boolean]
    (let [to (when (seq to) to)
          v  (msg/shout-verdict {:to to :broadcast? bcast} "s")]
      (= (:routing v) (cond to :direct bcast :broadcast :else :spawner)))))
