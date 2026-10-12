(ns hive-mcp.embeddings.resilient-cold-start-test
  "A provider whose first call pays a model load must still answer, inside the
   chain's total budget; past that budget the failure names provider, model and
   elapsed time. The stub is injected through the EmbeddingProvider port and the
   warmth atom the chain reads at call time."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.resilient :as res]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(clojure.test/use-fixtures :each
  (fn [test-fn]
    (reset! res/provider-health {})
    (test-fn)))

(defrecord ColdStartProvider [model load-ms calls]
  proto/EmbeddingProvider
  (embed-text [_ _]
    (when (= 1 (swap! calls inc))
      (Thread/sleep (long load-ms)))
    [1.0 2.0 3.0])
  (embed-batch [this ts] (mapv #(proto/embed-text this %) ts))
  (embedding-dimension [_] 3))

(defn- cold-start [model load-ms]
  (->ColdStartProvider model load-ms (atom 0)))

(defn- timed [f]
  (let [t0 (System/nanoTime)
        r  (try {:ok (f)} (catch Throwable e {:ex e}))]
    [(long (/ (- (System/nanoTime) t0) 1e6)) r]))

(deftest cold-first-call-succeeds-within-the-cold-budget
  (testing "load (600ms) exceeds the warm budget (300ms) but not the cold one"
    (let [warmth   (atom #{})
          p        (cold-start "qwen3-embedding:4b" 600)
          embedder (res/resilient-embedder [{:provider p :provider-key :ollama-test}]
                                           {:budget-ms       300
                                            :cold-budget-ms  2000
                                            :total-budget-ms 3000
                                            :warmth          warmth})
          [ms r]   (timed #(proto/embed-text embedder "x"))]
      (is (= [1.0 2.0 3.0] (:ok r)) (str "cold call failed: " (some-> (:ex r) ex-message)))
      (is (< ms 3000))
      (is (contains? @warmth [:ollama-test "qwen3-embedding:4b"])
          "a provider that answered is remembered warm")
      (let [[ms2 r2] (timed #(proto/embed-text embedder "y"))]
        (is (= [1.0 2.0 3.0] (:ok r2)))
        (is (< ms2 300) "the warm call is fast and runs on the warm budget")))))

(deftest the-old-flat-budget-fails-the-same-cold-call
  (testing "control: with cold budget = warm budget the load blows the attempt"
    (let [p        (cold-start "qwen3-embedding:4b" 600)
          embedder (res/resilient-embedder [{:provider p :provider-key :ollama-test}]
                                           {:budget-ms       300
                                            :cold-budget-ms  300
                                            :total-budget-ms 3000
                                            :warmth          (atom #{})})
          [_ r]    (timed #(proto/embed-text embedder "x"))]
      (is (:ex r)))))

(deftest evicted-warm-provider-is-retried-on-the-cold-budget
  (testing "known warm, but the model was evicted: one cold retry, then success"
    (let [warmth    (atom #{[:ollama-test nil]})
          calls     (atom 0)
          ;; First call (warm budget) times out mid-reload; the retry, on the
          ;; cold budget, sees the model loaded.
          slow-once (reify proto/EmbeddingProvider
                      (embed-text [_ _]
                        (if (= 1 (swap! calls inc))
                          (do (Thread/sleep 600) [0.0])
                          (do (Thread/sleep 100) [1.0 2.0 3.0])))
                      (embed-batch [this ts] (mapv #(proto/embed-text this %) ts))
                      (embedding-dimension [_] 3))
          embedder  (res/resilient-embedder [{:provider slow-once :provider-key :ollama-test}]
                                            {:budget-ms       300
                                             :cold-budget-ms  2000
                                             :total-budget-ms 4000
                                             :warmth          warmth})
          [ms r]    (timed #(proto/embed-text embedder "x"))]
      (is (= [1.0 2.0 3.0] (:ok r)) (str "retry failed: " (some-> (:ex r) ex-message)))
      (is (= 2 @calls) "exactly one retry")
      (is (< ms 4000)))))

(deftest past-the-budget-the-error-is-loud
  (let [p        (cold-start "qwen3-embedding:4b" 5000)
        embedder (res/resilient-embedder [{:provider p :provider-key :ollama-test}]
                                         {:budget-ms       200
                                          :cold-budget-ms  500
                                          :total-budget-ms 800
                                          :warmth          (atom #{})})
        [ms r]   (timed #(proto/embed-text embedder "x"))
        ex       (:ex r)
        msg      (some-> ex ex-message)
        d        (some-> ex ex-data)]
    (is ex "a load longer than the whole budget must fail")
    (is (< ms 2000) (str "took " ms "ms against an 800ms budget"))
    (is (= :embedder/chain-exhausted (:error d)))
    (is (str/includes? msg "ollama-test") msg)
    (is (str/includes? msg "qwen3-embedding:4b") msg)
    (is (re-find #"\d+ms" msg) msg)
    (is (nat-int? (:elapsed-ms d)))
    (is (= "qwen3-embedding:4b" (:model (first (:failures d)))))))
