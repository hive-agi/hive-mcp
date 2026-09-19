(ns hive-mcp.channel.async-result-loss-test
  "A queued async result must reach the caller or be reported, never vanish.

   The ack for an async call is `{:queued true}`, so the buffered result is the
   only report that call will ever make. An entry dropped here is a silent
   loss, and for a `memory add` that is knowledge gone with no signal."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.async-result :as ar]
            [clojure.set :as set]))

(use-fixtures :each (fn [t] (ar/reset-all!) (t) (ar/reset-all!)))

(def ^:private caller "loss-test-caller")

(defn- drain-everything
  "Drain until the buffer reports nothing left, collecting every task-id seen.
   Bounded so a non-terminating drain fails the test instead of hanging."
  [caller-id]
  (loop [seen [] guard 0]
    (if (> guard 1000)
      (throw (ex-info "drain! did not terminate" {:seen (count seen)}))
      (if-let [{:keys [results]} (ar/drain! caller-id)]
        (recur (into seen (map :task-id) results) (inc guard))
        seen))))

(deftest every-enqueued-result-is-eventually-delivered
  (doseq [n [1 5 50]]
    (ar/reset-all!)
    (testing (str n " sequential results")
      (doseq [i (range n)]
        (ar/enqueue-result! caller {:task-id (str "t" i) :tool "memory" :status :completed
                                    :result {:id (str "id" i)}}))
      (let [seen (drain-everything caller)]
        (is (= n (count seen)) "an enqueued result went missing")
        (is (= (set (map #(str "t" %) (range n))) (set seen)))
        (is (apply distinct? seen) "a result was delivered twice")))))

(deftest a-result-enqueued-while-a-drain-runs-is-not-destroyed
  (testing "the read-modify-write race: enqueue lands between snapshot and write"
    ;; Repeated because the window is small; the pre-fix code loses entries
    ;; here within a few iterations.
    (dotimes [round 40]
      (ar/reset-all!)
      (let [before  20
            during  20
            _       (doseq [i (range before)]
                      (ar/enqueue-result! caller {:task-id (str "b" i) :tool "memory"
                                                  :status :completed :result {:i i}}))
            writer  (future
                      (doseq [i (range during)]
                        (ar/enqueue-result! caller {:task-id (str "d" i) :tool "memory"
                                                    :status :completed :result {:i i}})))
            ;; Drain concurrently with the writer, then keep draining until the
            ;; writer is done and the buffer is empty.
            early   (loop [seen []]
                      (if (and (future-done? writer) (not (ar/has-pending? caller)))
                        seen
                        (recur (if-let [{:keys [results]} (ar/drain! caller)]
                                 (into seen (map :task-id) results)
                                 seen))))
            _       @writer
            seen    (into early (drain-everything caller))
            expect  (into (set (map #(str "b" %) (range before)))
                          (map #(str "d" %) (range during)))]
        (is (= expect (set seen))
            (str "round " round ": lost " (count (set/difference expect (set seen)))
                 " result(s) to the drain race"))
        (is (apply distinct? seen)
            (str "round " round ": a result was delivered twice"))))))

(deftest a-failure-result-survives-the-same-race
  (testing "an :error result is the only report a failed write makes"
    (dotimes [_ 20]
      (ar/reset-all!)
      (ar/enqueue-result! caller {:task-id "ok" :tool "memory" :status :completed :result {}})
      (let [writer (future (ar/enqueue-result! caller {:task-id "boom" :tool "memory"
                                                       :status :error :error "backend down"}))
            seen   (loop [seen []]
                     (if (and (future-done? writer) (not (ar/has-pending? caller)))
                       seen
                       (recur (if-let [{:keys [results]} (ar/drain! caller)]
                                (into seen results)
                                seen))))]
        @writer
        (let [seen (into seen (:results (ar/drain! caller)))]
          (is (some #(= "boom" (:task-id %)) seen)
              "the failure report was silently dropped")
          (is (= "backend down" (some #(when (= "boom" (:task-id %)) (:error %)) seen))))))))

(deftest draining-an-unknown-caller-is-nil-not-a-crash
  (is (nil? (ar/drain! "nobody-here")))
  (is (false? (ar/has-pending? "nobody-here"))))

(deftest an-oversized-single-result-still-drains
  (testing "one entry over the char budget must not wedge the buffer forever"
    (ar/enqueue-result! caller {:task-id "huge" :tool "memory" :status :completed
                                :result {:blob (apply str (repeat (* 2 ar/drain-char-budget) "x"))}})
    (let [{:keys [results done]} (ar/drain! caller)]
      (is (= 1 (count results)))
      (is (true? done))
      (is (nil? (ar/drain! caller)) "buffer should be empty afterwards"))))

(deftest the-cursor-and-the-payload-cannot-disagree
  (testing ":delivered must equal what was actually handed over"
    (doseq [i (range 7)]
      (ar/enqueue-result! caller {:task-id (str "c" i) :tool "memory" :status :completed :result {}}))
    (let [{:keys [results delivered total remaining]} (ar/drain! caller)]
      (is (= (count results) delivered))
      (is (= 7 total))
      (is (= (- total delivered) remaining)))))
