(ns hive-mcp.server.routes.piggyback-budget-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.server.routes.middleware :as middleware]
            [hive-mcp.channel.async-result :as async]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.channel.memory-piggyback]
            [hive-mcp.channel.piggyback]))

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

(deftest middleware-four-channel-wire-budget-and-deferral
  ;; Run the wrapper with real buffers/cursors and injected source ports.
  (let [caller "tw5-budget-integration"
        previous-source @hive-mcp.channel.piggyback/message-source-fn
        block-calls (atom 0)
        catchup-calls (atom 0)
        handler (middleware/wrap-handler-piggybacks
                 (constantly [{:type "text" :text "answer"}]) "test")
        args {:_caller_id caller :project_id "hive-mcp"}
        text (fn [response] (apply str (map :text response)))
        budgeted (fn [wire]
                   (let [hm (.indexOf wire "\n\n---HIVEMIND---")]
                     (subs wire (count "answer") (if (neg? hm) (count wire) hm))))]
    (try
      (swap! async/buffers assoc caller
             {:entries [{:task-id "large-async" :status :completed
                         :result (apply str (repeat 29500 "A"))}] :cursor 0})
      (hive-mcp.channel.memory-piggyback/enqueue!
       caller [{:id "budget-memory" :type "axiom"
                :content (apply str (repeat 1000 "M"))}])
      (ext/register! :block/tw5-budget
                     (fn [_] (swap! block-calls inc)
                       (when (= 1 @block-calls) (apply str (repeat 3500 "B")))))
      (ext/register! :cu/piggyback-drain
                     (fn [_ _] (swap! catchup-calls inc)
                       {:tw5_budget (apply str (repeat 3500 "C"))}))
      (hive-mcp.channel.piggyback/register-message-source!
       (fn [] [{:agent-id "tw5-peer" :event-type :progress
                :to "tw5-budget-integration-hive-mcp"
                :message "untouched-hivemind" :timestamp 1000
                :project-id "hive-mcp"}]))
      (let [first-wire (text (handler args))
            first-budgeted (budgeted first-wire)
            second-wire (text (handler args))
            second-budgeted (budgeted second-wire)]
        (is (<= (count first-budgeted) middleware/response-piggyback-char-budget)
            "actual delimited wire, not the sum of channel body estimates")
        (is (<= (count second-budgeted) middleware/response-piggyback-char-budget))
        (is (<= 0 (.indexOf first-budgeted "---TOOLRESULT---")))
        (is (< (.indexOf first-budgeted "---TOOLRESULT---")
               (.indexOf first-budgeted "---MEMORY---"))
            "async is spent before memory")
        (is (neg? (.indexOf first-budgeted "---TW5-BUDGET---"))
            "block cannot fit after higher priority channels")
        (is (neg? (.indexOf first-budgeted "---TW5_BUDGET---"))
            "catchup cannot fit after higher priority channels")
        (is (<= 0 (.indexOf second-budgeted "---TW5-BUDGET---"))
            "one-shot emitted block was retained")
        (is (< (.indexOf second-budgeted "---TW5-BUDGET---")
               (.indexOf second-budgeted "---TW5_BUDGET---"))
            "unused async and memory allowances roll forward to blocks then catchup")
        (is (= 1 @catchup-calls) "deferred catchup must not invoke its source again")
        (is (= 1 @block-calls) "deferred one-shot block must not invoke its source again")
        (is (<= 0 (.indexOf first-wire "untouched-hivemind"))
            "hivemind is delivered outside the four-channel ceiling")
        (is (neg? (.indexOf second-wire "untouched-hivemind"))
            "hivemind cursor still advances normally"))
      (finally
        (async/clear-buffer! caller)
        (hive-mcp.channel.memory-piggyback/clear-buffer! caller)
        (ext/deregister! :block/tw5-budget)
        (ext/deregister! :cu/piggyback-drain)
        (hive-mcp.channel.piggyback/register-message-source! previous-source)))))
