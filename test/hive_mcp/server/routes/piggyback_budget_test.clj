(ns hive-mcp.server.routes.piggyback-budget-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.server.routes.middleware :as middleware]
            [hive-mcp.channel.async-result :as async]
            [hive-mcp.extensions.registry :as ext]))

(deftrifecta priority-rollover-exhaustion
  hive-mcp.server.routes.middleware/spend-piggyback-budget
  {:gen (gen/tuple (gen/choose 0 1000) (gen/choose 0 1000) (gen/choose 0 1000))
   :apply? true
   :pred (fn [result] (and (map? result)
                           (<= 0 (:remaining result))
                           (<= 0 (:allowance result))))
   :num-tests 100
   :mutations [["split-evenly" (fn [remaining cap cost]
                                   {:allowance (quot remaining 2)
                                    :remaining (max 0 (- remaining cost))})]
               ["discard-rollover" (fn [remaining cap cost]
                                      {:allowance (min remaining cap)
                                       :remaining (max 0 (- remaining cap))})]]
   :assert (fn []
             (let [spend middleware/spend-piggyback-budget
                   a (spend 100 80 60)
                   m (spend (:remaining a) 30 0)
                   b (spend (:remaining m) 20 20)
                   c (spend (:remaining b) 10 10)]
               (is (= [80 30 20 10] (mapv :allowance [a m b c])))
               (is (= [40 40 20 10] (mapv :remaining [a m b c])))
               (is (= {:allowance 0 :remaining 0} (spend 0 4000 100)))))} )

(deftest async-budget-defers-without-advancing-cursor
  (let [caller "tw5-budget-test"]
    (try
      (swap! async/buffers assoc caller
             {:entries [{:task-id "a" :result "hello"}
                        {:task-id "b" :result "world"}] :cursor 0})
      (is (nil? (async/drain! caller 1)))
      (is (= 0 (get-in @async/buffers [caller :cursor])))
      (finally (async/clear-buffer! caller)))))

(deftest oversized-budgeted-result-is-marked-and-drains
  (let [caller "tw5-oversized"]
    (try
      (swap! async/buffers assoc caller
             {:entries [{:task-id "huge" :status :completed
                         :result (apply str (repeat 90000 "x"))}] :cursor 0})
      (let [drained (async/drain! caller 400)]
        (is (= "huge" (get-in drained [:results 0 :task-id])))
        (is (<= (count (pr-str (first (:results drained)))) 400))
        (is (re-find #"truncated" (get-in drained [:results 0 :result])))
        (is (:done drained))
        (is (nil? (async/drain! caller 400))))
      (finally (async/clear-buffer! caller)))))

(deftest catchup-first-candidate-respects-budget-and-rolls-over
  (let [caller "tw5-catchup-budget"
        calls (atom 0)
        drained (fn [budget]
                  (#'middleware/drain-catchup-piggyback
                   caller {:char-budget budget}))]
    (hive-mcp.extensions.registry/register!
     :cu/piggyback-drain
     (fn [_ _] (swap! calls inc) {:large (apply str (repeat 500 "L"))}))
    (try
      (is (nil? (drained 120)))
      (is (= {:large (apply str (repeat 500 "L"))} (drained 600)))
      (is (= 1 @calls) "the provider must not be called twice for a deferred block")
      (finally (hive-mcp.extensions.registry/deregister! :cu/piggyback-drain)))))
