(ns hive-mcp.embeddings.concurrency-bound-test
  "N concurrent batch callers never put more embed calls in flight than the
   shared gate admits, whether they reach the provider through the gate
   decorator or through the resilient failover chain."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.resilient :as resilient]
            [hive-mcp.embeddings.shared-gate :as shared]))

(defn- peak-recorder
  "Stub EmbeddingProvider that counts its in-flight calls and keeps the peak."
  [active peak hold-ms]
  (let [enter! (fn [] (swap! peak max (swap! active inc)) (Thread/sleep (long hold-ms)))
        leave! (fn [] (swap! active dec))]
    (reify proto/EmbeddingProvider
      (embed-text [_ _] (enter!) (try [1.0 2.0] (finally (leave!))))
      (embed-batch [_ texts] (enter!) (try (mapv (constantly [1.0 2.0]) texts) (finally (leave!))))
      (embedding-dimension [_] 2))))

(defn- run-callers!
  "Start `n` callers at once; caller i embeds one two-text batch through
   (port-of i) in lane (lane-of i). Returns the callers' results in order."
  [n port-of lane-of]
  (let [start (promise)
        futs (mapv (fn [i]
                     (future
                       @start
                       (shared/with-lane (lane-of i)
                         (proto/embed-batch (port-of i) [(str "a" i) (str "b" i)]))))
                   (range n))]
    (deliver start true)
    (mapv #(deref % 20000 ::pending) futs)))

(def ^:private two-vectors [[1.0 2.0] [1.0 2.0]])

(deftest harness-detects-unbounded-fan-out
  (testing "ungated, the same callers overlap, so the bounds below are measured, not vacuous"
    (let [active (atom 0) peak (atom 0)
          port (peak-recorder active peak 40)
          results (run-callers! 8 (constantly port) (constantly :batch))]
      (is (every? #{two-vectors} results))
      (is (> @peak 2)))))

(deftest gated-provider-bounds-concurrent-batch-callers
  (let [active (atom 0) peak (atom 0)
        gate (shared/new-gate 2 10000)
        port (shared/gated-provider (peak-recorder active peak 20) gate)
        results (run-callers! 12 (constantly port) (constantly :batch))]
    (is (every? #{two-vectors} results))
    (is (<= 1 @peak 2) "never more batch calls in flight than :permits")
    (is (zero? @(:active gate)) "every permit returned")))

(deftest interactive-reserve-is-the-only-headroom
  (let [active (atom 0) peak (atom 0)
        gate (shared/new-gate 2 10000 {:interactive-reserve 1})
        port (shared/gated-provider (peak-recorder active peak 20) gate)
        results (run-callers! 12 (constantly port) #(if (even? %) :interactive :batch))]
    (is (every? #{two-vectors} results))
    (is (<= @peak 3) "permits plus the interactive reserve, never more")
    (is (zero? @(:active gate)))))

(deftest resilient-chain-shares-the-gate-bound
  (testing "chain attempts and direct gated calls draw on one pool of permits"
    (let [active (atom 0) peak (atom 0)
          gate (shared/new-gate 2 10000)
          gated (shared/gated-provider (peak-recorder active peak 20) gate)
          chain (fn [] (resilient/resilient-embedder
                        [{:provider gated :provider-key :stub}]
                        {:total-budget-ms 15000 :budget-ms 5000 :cold-budget-ms 5000
                         :warmth (atom #{}) :health (atom {})}))
          ;; a fresh chain per caller, as the host builds one per write
          results (run-callers! 12 #(if (even? %) (chain) gated) (constantly :batch))]
      (is (every? #{two-vectors} results))
      (is (<= 1 @peak 2))
      (is (zero? @(:active gate))))))
