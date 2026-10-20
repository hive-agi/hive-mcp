(ns hive-mcp.server.routes.async-lane-test
  "A short async verb must not wait behind hours-long async work.

   Before lanes, every async tool call shared one FIFO pool (`mcp-async`, 8
   slots). Eight ingests running meant a `git ship land` that needs seconds sat
   :task/queued until one of them finished (ASYNC-POOL-LANE, 2026-10-12
   atask-2225d26a). These tests fill the long lane and require the short one
   to keep running."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.routes.async-tasks :as at]
            [hive-mcp.server.routes.async-task.lane :as lane]
            [hive-mcp.server.routes.middleware :as mw]
            [hive-mcp.channel.async-result :as async-buf]
            [hive-weave.pool :as wpool])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- clean [f]
  (at/reset-registry!)
  (reset! at/runner nil)
  (reset! at/lane-runners {})
  (try (f)
       (finally
         (reset! at/runner nil)
         (reset! at/lane-runners {})
         (at/reset-registry!))))

(use-fixtures :each clean)

(defn- await-latch [^CountDownLatch l ms] (.await l ms TimeUnit/MILLISECONDS))

(defn- await-state [task-id state ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond
        (= state (:state (at/get-task task-id))) true
        (> (System/currentTimeMillis) deadline)  false
        :else (do (Thread/sleep 10) (recur))))))

(defn- hog!
  "Submit `n` tasks on `lane-kw` that each block until `release` opens.
   Returns true once all `n` are running."
  [n lane-kw ^CountDownLatch release]
  (let [started (CountDownLatch. n)]
    (dotimes [i n]
      (at/submit! {:task-id (str "t-hog-" (name lane-kw) "-" i) :tool "ingest" :caller-id "test"
                   :lane lane-kw
                   :f (fn [] (.countDown started) (.await release 60 TimeUnit/SECONDS))}))
    (await-latch started 10000)))

;; =============================================================================
;; Classification is data
;; =============================================================================

(deftest forge-verbs-are-short-and-everything-else-keeps-the-default
  (testing "the git verbs that wait on a forge ride the short lane"
    (is (= :short (lane/classify "git" "ship land")))
    (is (= :short (lane/classify "git" "ship watch")))
    (is (= :short (lane/classify "git" "ship release")))
    (is (= :short (lane/classify "git" "live sync")))
    (is (= :short (lane/classify "git" (keyword "ship land")))
        "a keyword command classifies like its string"))
  (testing "anything not declared keeps the default lane"
    (is (= lane/default-lane (lane/classify "git" "ship status")))
    (is (= lane/default-lane (lane/classify "ingest" "run")))
    (is (= lane/default-lane (lane/classify "git" nil)))
    (is (= lane/default-lane (lane/classify nil nil))))
  (testing "every lane the table names is a lane with a pool"
    (is (every? lane/lanes (mapcat vals (vals lane/declared-lanes))))))

;; =============================================================================
;; The incident, reproduced: one shared lane starves a short task
;; =============================================================================

(deftest on-a-shared-lane-a-short-task-queues-behind-a-long-one
  ;; Acceptance 1 of the card, measured: a one-slot lane held by a long task
  ;; leaves a second task on that lane :task/queued, its wait growing.
  (let [pool    (wpool/make-pool {:name "test-batch-1" :size 1 :queue-capacity 8 :rejection :abort})
        release (CountDownLatch. 1)
        ran     (CountDownLatch. 1)]
    (try
      (swap! at/lane-runners assoc :batch (at/->WeavePoolRunner pool))
      (is (hog! 1 :batch release))
      (at/submit! {:task-id "t-land-shared" :tool "git" :caller-id "test" :lane :batch
                   :f (fn [] (.countDown ran))})
      (let [t1 (at/get-task "t-land-shared")]
        (is (false? (await-latch ran 250)) "it does not run while the long task holds the slot")
        (let [t2 (at/get-task "t-land-shared")]
          (is (= :task/queued (:state t1) (:state t2)))
          (is (< (:elapsed-ms t1) (:elapsed-ms t2)) "and its wait keeps growing")))
      (finally
        (.countDown release)
        (wpool/shutdown! pool)))))

;; =============================================================================
;; The fix: the short lane is not the long lane
;; =============================================================================

(deftest a-short-task-runs-while-the-default-long-lane-is-full
  ;; The real default pools: fill every batch slot, then a short task must
  ;; still run to completion.
  (let [release (CountDownLatch. 1)]
    (try
      (is (hog! at/default-pool-size :batch release) "every batch slot is held")
      (let [done (CountDownLatch. 1)]
        (at/submit! {:task-id "t-land" :tool "git" :caller-id "test" :lane :short
                     :f (fn [] (.countDown done))})
        (is (await-latch done 5000) "the short task ran although the long lane is full")
        (is (await-state "t-land" :task/done 5000))
        (is (= :short (:lane (at/get-task "t-land"))) "the listing names the lane"))
      (finally (.countDown release)))))

(deftest a-task-without-a-lane-keeps-the-default
  (let [seen   (atom nil)
        inline (fn [tag]
                 (reify at/ITaskRunner
                   (-run [_ f] (reset! seen tag) (f) ::h)
                   (-cancel [_ _] false)
                   (-view [_ h] {:present? (some? h) :done? true :cancelled? false})))]
    (reset! at/lane-runners {:batch (inline :batch) :short (inline :short)})
    (at/submit! {:task-id "t-plain" :tool "memory" :caller-id "test" :f (fn [] nil)})
    (is (= :batch @seen))
    (is (= :batch (:lane (at/get-task "t-plain"))))))

(deftest an-injected-runner-still-takes-every-lane
  ;; `runner` predates lanes; a test or caller that rebinds it substitutes
  ;; execution for ALL work, as before.
  (let [seen (atom 0)]
    (reset! at/runner (reify at/ITaskRunner
                        (-run [_ f] (swap! seen inc) (f) ::h)
                        (-cancel [_ _] false)
                        (-view [_ h] {:present? (some? h) :done? true :cancelled? false})))
    (at/submit! {:task-id "t-a" :tool "git" :caller-id "test" :lane :short :f (fn [] nil)})
    (at/submit! {:task-id "t-b" :tool "memory" :caller-id "test" :f (fn [] nil)})
    (is (= 2 @seen))))

(deftest cancel-reaches-the-lane-the-task-runs-on
  (let [pool        (wpool/make-pool {:name "test-short-cancel" :size 1 :queue-capacity 1 :rejection :abort})
        started     (CountDownLatch. 1)
        interrupted (CountDownLatch. 1)]
    (try
      (swap! at/lane-runners assoc :short (at/->WeavePoolRunner pool))
      (at/submit! {:task-id "t-short-cancel" :tool "git" :caller-id "test" :lane :short
                   :f (fn []
                        (.countDown started)
                        (try (Thread/sleep 60000)
                             (catch InterruptedException _ (.countDown interrupted))))})
      (is (await-latch started 5000))
      (is (= :cancel/interrupted (:adt/variant (at/cancel! "t-short-cancel"))))
      (is (await-latch interrupted 5000))
      (finally (wpool/shutdown! pool)))))

;; =============================================================================
;; The middleware hands the classification to submit!
;; =============================================================================

(deftest the-async-wrapper-submits-on-the-classified-lane
  (let [submitted (atom [])]
    (with-redefs [at/submit!                   (fn [spec] (swap! submitted conj spec) ::h)
                  async-buf/record-submission! (fn [& _] true)]
      ((mw/wrap-handler-async (fn [_] :ok) "git") {:async true :command "ship land"})
      ((mw/wrap-handler-async (fn [_] :ok) "ingest") {:async true :command "run"}))
    (is (= [:short lane/default-lane] (mapv :lane @submitted)))))
