(ns hive-mcp.await.burst-source-test
  "Review B3 on PR #760: a source that emits from INSIDE open-source (a
   burst at subscribe: a log tail that already matches, a predicate already
   true at the first poll, the RUN-LEDGER-4 case) and two racing deliveries
   must start a wake exactly once.

   The :burst source here is a real open-source method that delivers N
   matching events before it answers its closer, and the resumer is a
   counting :burst-stub. Both are registered through the ports, not
   redefined."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.await.domain :as domain]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private resumes (atom []))
(defonce ^:private closed (atom #{}))

(defmethod ports/open-resumer :burst-stub
  [_owner]
  (fn [req] (swap! resumes conj (get-in req [:awaiter :id])) {:ok true}))

(defmethod ports/arm-on-create? :burst-stub [_owner] true)

(defmethod ports/open-resumer :burst-parked
  ;; Same counting resumer, but a kind that parks (arm-on-create? false).
  [_owner]
  (fn [req] (swap! resumes conj (get-in req [:awaiter :id])) {:ok true}))

(defmethod ports/open-source :burst
  [subscription awaiter deliver!]
  ;; Emit the whole burst synchronously, before the closer exists.
  (dotimes [i (long (or (get-in subscription [:filter :n]) 3))]
    (deliver! {:type :burst :data {:n (get-in subscription [:filter :n]) :i i}}))
  (fn close [] (swap! closed conj (:id awaiter))))

(defonce ^:private latches
  ;; awaiter id -> the latch its :late-thread source waits on before emitting.
  (atom {}))

(defmethod ports/open-source :late-thread
  ;; Emits from a thread of its OWN, after open-source has returned: the
  ;; shape of a poller or a log tail (RUN-LEDGER-4) or of a concurrent
  ;; `await fire`. It blocks until the test counts the latch down.
  [_subscription awaiter deliver!]
  (let [latch (java.util.concurrent.CountDownLatch. 1)
        id    (:id awaiter)]
    (swap! latches assoc id latch)
    (future (.await latch) (deliver! {:type :late-thread :data {}}))
    ;; Released as the LAST act before returning: the emitting thread then
    ;; races the registry's own code after open-source returns, which is
    ;; where the closer is stored.
    (.countDown latch)
    (fn close [] (swap! closed conj id))))

(defn- temp-path []
  (str (io/file (.toFile (java.nio.file.Files/createTempDirectory
                          "await-burst" (make-array java.nio.file.attribute.FileAttribute 0)))
                "registry.edn")))

(defn- wait-until [ms pred]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (pred)]
        (if (or v (> (System/currentTimeMillis) end)) v (do (Thread/sleep 10) (recur)))))))

(deftest burst-at-subscribe-wakes-an-armed-at-create-awaiter-once
  (reset! resumes []) (reset! closed #{})
  (let [reg (registry/make-registry (temp-path))]
    (registry/init! reg)
    (try
      (let [a (registry/create! reg {:id "burst-1" :owner {:kind :burst-stub}
                                     :subscription {:type :burst :filter {:n 5}}
                                     :timeout-ms 60000})]
        (is (contains? #{:waking :woken} (:status a))
            "the first event of the burst woke the row that was inserted :armed"))
      (is (wait-until 2000 #(some? (registry/resume-result reg "burst-1"))))
      (is (= ["burst-1"] @resumes) "five events at subscribe, one resume")
      (is (= 1 (:attempt (registry/lookup reg "burst-1"))) "begin-wake ran once")
      (is (contains? @closed "burst-1") "the source that fired during open is closed, not leaked")
      (is (nil? (get-in @(:state reg) [:timers "burst-1"])) "no deadline is left for a woken row")
      (finally (registry/shutdown! reg)))))

(deftest n3-a-source-that-emits-from-its-own-thread-never-leaks
  (reset! resumes []) (reset! closed #{}) (reset! latches {})
  (let [reg (registry/make-registry (temp-path))
        ids (mapv #(str "late-" %) (range 200))]
    (registry/init! reg)
    (try
      (doseq [id ids]
        (registry/create! reg {:id id :owner {:kind :burst-stub} :timeout-ms 600000
                               :subscription {:type :late-thread}}))
      (is (wait-until 5000 #(every? (fn [id] (some? (registry/resume-result reg id))) ids)))
      (is (= (set ids) (set @resumes)) "every awaiter woke exactly once")
      (is (= 200 (count @resumes)))
      (is (wait-until 2000 #(= (set ids) @closed))
          "every source was closed: none outlived its awaiter")
      (is (empty? (select-keys (:closers @(:state reg)) ids)) "no closer is left stored")
      (is (empty? (select-keys (:timers @(:state reg)) ids)) "no deadline timer is left stored")
      (finally (registry/shutdown! reg)))))

(deftest racing-deliveries-start-one-wake
  (reset! resumes [])
  (let [reg (registry/make-registry (temp-path))]
    (registry/init! reg)
    (try
      (registry/create! reg {:id "race-1" :owner {:kind :burst-stub}
                             :subscription {:type :burst :filter {:n 0}}})
      (is (= :armed (:status (registry/lookup reg "race-1"))))
      (let [payload {:awaiter-id "race-1" :at 1 :reason :event}
            starts  (->> (range 16)
                         (mapv (fn [_] (future (registry/deliver! reg "race-1" payload))))
                         (mapv deref)
                         (remove nil?))]
        (is (= ["race-1"] starts) "exactly one of 16 concurrent deliveries starts the wake"))
      (is (wait-until 2000 #(some? (registry/resume-result reg "race-1"))))
      (is (= ["race-1"] @resumes))
      (testing "a woken awaiter is never handed over again"
        (is (nil? (registry/deliver! reg "race-1" {:awaiter-id "race-1" :at 2 :reason :event})))
        (is (= ["race-1"] @resumes)))
      (finally (registry/shutdown! reg)))))

(deftest arm-and-a-racing-event-start-one-wake
  (testing "the pending path (hive-agent-run style): an event recorded while pending, then arm, races a second event"
    (reset! resumes [])
    (let [reg (registry/make-registry (temp-path))]
      (registry/init! reg)
      (try
        (registry/create! reg {:id "p-1" :owner {:kind :burst-parked}
                               :subscription {:type :burst :filter {:n 1}}})
        (is (= :pending (:status (registry/lookup reg "p-1"))))
        (is (some? (:pending-wake (registry/lookup reg "p-1"))) "the burst was recorded, not resumed")
        (let [ev  {:type :burst :data {:n 1 :i 9}}
              fs  [(future (registry/arm! reg "p-1" "run-p"))
                   (future (registry/offer! reg "p-1" ev))
                   (future (registry/offer! reg "p-1" ev))]]
          (run! deref fs))
        (is (wait-until 2000 #(some? (registry/resume-result reg "p-1"))))
        (Thread/sleep 50)
        (is (= ["p-1"] @resumes))
        (is (= 1 (:attempt (registry/lookup reg "p-1"))))
        (is (domain/live? {:status :armed}) "sanity: :armed is live")
        (finally (registry/shutdown! reg))))))
