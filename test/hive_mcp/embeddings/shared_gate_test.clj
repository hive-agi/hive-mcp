(ns hive-mcp.embeddings.shared-gate-test
  "Every provider operation crosses the same gated decorator; interactive
   work wins admission over already-waiting batch work."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.shared-gate :as shared]))

(defn next-lane
  "Unary adapter for the pure admission policy."
  [[interactive batch]]
  (shared/next-lane interactive batch))

(deftrifecta priority-admission
  hive-mcp.embeddings.shared-gate-test/next-lane
  {:golden-path "test/golden/hive-mcp/embeddings/priority-admission.edn"
   :cases {:both [1 100] :interactive [2 0] :batch [0 4] :empty [0 0]}
   :gen (gen/tuple (gen/choose 0 100) (gen/choose 0 100))
   :pred (fn [lane] (or (nil? lane) (#{:interactive :batch} lane)))
   :num-tests 80
   :mutations [["batch-wins" (fn [[i b]] (cond (pos? b) :batch (pos? i) :interactive))]
               ["always-interactive" (fn [_] :interactive)]]})

(defn recording-provider [seen behavior]
  (reify proto/EmbeddingProvider
    (embed-text [_ text] (swap! seen conj [:text text]) (behavior text))
    (embed-batch [_ texts] (swap! seen conj [:batch texts]) (mapv behavior texts))
    (embedding-dimension [_] 3)))

(deftest single-and-batch-use-the-same-decorator
  (let [events (atom [])
        raw (recording-provider events (constantly [1.0 2.0 3.0]))
        port (shared/gated-provider raw (shared/new-gate 1 1000))]
    (is (= [1.0 2.0 3.0] (proto/embed-text port "one")))
    (is (= [[1.0 2.0 3.0] [1.0 2.0 3.0]]
           (proto/embed-batch port ["two" "three"])))
    (is (= [[:text "one"] [:batch ["two" "three"]]] @events))
    (is (= 3 (proto/embedding-dimension port)))))

(deftest faults-are-not-mistaken-for-vectors
  (doseq [fault [:timeout :forbidden]]
    (let [provider (recording-provider (atom [])
                      (fn [_] (throw (ex-info "fault" {:status (if (= fault :forbidden) 403 504)}))))
          gate (shared/gated-provider provider (shared/new-gate 1 100))]
      (is (thrown? Exception (proto/embed-text gate "fault")))
      (is (thrown? Exception (proto/embed-batch gate ["fault"]))))))

(deftest timed-out-batch-wakes-the-next-batch
  (let [entered (promise)
        release (promise)
        seen (atom [])
        gate (shared/new-gate 1 140)
        port (shared/gated-provider
               (recording-provider seen (fn [text]
                                          (when (= text "hold")
                                            (deliver entered true)
                                            @release)
                                          [1.0])) gate)
        hold (future (proto/embed-text port "hold"))]
    (try
      (is (= true (deref entered 1000 false)))
      (let [first-waiter (future (try (proto/embed-text port "timeout")
                                      (catch Exception e (ex-data e))))]
        (is (shared/await-waiters gate :batch 1 100))
        (is (= :embedder/gate-timeout (:error (deref first-waiter 1000 nil))))
        (is (zero? (:batch @(:waiting gate))))
        (deliver release true)
        (is (= [1.0] (deref hold 1000 nil)))
        (is (= [1.0] (proto/embed-text port "after")))
        (is (= [[:text "hold"] [:text "after"]] @seen)))
      (finally (deliver release true)))))

(deftest interactive-wins-over-waiting-batch
  (let [entered (promise)
        release (promise)
        events (atom [])
        raw (recording-provider events
              (fn [text]
                (when (= text "hold")
                  (deliver entered true)
                  @release)
                [1.0 2.0 3.0]))
        gate (shared/new-gate 1 2000)
        port (shared/gated-provider raw gate)
        hold (future (proto/embed-text port "hold"))]
    @entered
    (let [batch (future (proto/embed-batch port ["batch"]))]
      (try
        (is (shared/await-waiters gate :batch 1 1000))
        (let [interactive (future (binding [shared/*lane* :interactive]
                                    (proto/embed-text port "add")))]
          (is (shared/await-waiters gate :interactive 1 1000))
          (deliver release true)
          (is (= [1.0 2.0 3.0] @interactive))
          (is (= [[1.0 2.0 3.0]] @batch))
          (is (= [[:text "hold"] [:text "add"] [:batch ["batch"]]] @events)))
        (finally
          (deliver release true)
          (deref hold 2000 nil))))))
