(ns hive-mcp.knowledge-graph.connection-writer-thread-test
  "The coalescing consumer runs on its own thread: a transact that blocks does
   not hold a core.async dispatch thread, so unrelated go blocks still run."
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.connection.writer :as writer]))

(def ^:private start-loop #'writer/start-writer-loop!)

(defn- wait-until [pred ms]
  (let [end (+ (System/currentTimeMillis) ms)]
    (while (and (not (pred)) (< (System/currentTimeMillis) end))
      (Thread/sleep 5))))

(deftest blocked-transact-does-not-starve-go-blocks
  ;; More blocked consumers than core.async has dispatch threads (8 by
  ;; default): under a go-loop each would pin one and the probe below would
  ;; never be scheduled.
  (let [release  (promise)
        entered  (atom 0)
        blocking (fn [_tx] (swap! entered inc) @release)
        loops    (vec (repeatedly 16 #(start-loop blocking)))]
    (try
      (doseq [{:keys [tx-chan]} loops]
        (async/>!! tx-chan [[:db/add -1 :kg-edge/id "e"]]))
      (wait-until #(= 16 @entered) 2000)
      (is (= 16 @entered) "every consumer is inside its blocking transact")
      (let [probe (async/go :ran)]
        (is (= :ran (first (async/alts!! [probe (async/timeout 1000)])))
            "an unrelated go block still gets a dispatch thread"))
      (finally
        (deliver release true)
        (doseq [{:keys [tx-chan go-chan]} loops]
          (async/close! tx-chan)
          (async/alts!! [go-chan (async/timeout 2000)]))))))

(deftest consumer-still-coalesces-and-accounts
  (let [seen   (atom [])
        before @writer/in-flight
        loopm  (start-loop (fn [tx] (swap! seen conj (count tx))))]
    (try
      (swap! writer/in-flight + 3)
      (doseq [i (range 3)]
        (async/>!! (:tx-chan loopm) [[:db/add -1 :kg-edge/id (str "e" i)]]))
      (wait-until #(= before @writer/in-flight) 2000)
      (is (= 3 (reduce + @seen)) "all three items written")
      (is (= 1 (count @seen)) "within the 25 ms window they flush as one batch")
      (is (= before @writer/in-flight) "in-flight is decremented by the batch")
      (finally
        (async/close! (:tx-chan loopm))
        (async/alts!! [(:go-chan loopm) (async/timeout 2000)])
        (reset! writer/in-flight before)))))
