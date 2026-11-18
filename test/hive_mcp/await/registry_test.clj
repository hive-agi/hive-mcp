(ns hive-mcp.await.registry-test
  "Registry acceptance for the two-phase park contract.

   A created awaiter is :pending and RESUMES NOTHING, even when its event or
   its deadline arrives first: the wake is recorded. `arm!` promotes it and
   fires the recorded wake exactly once. A deadline fires with :timeout, a
   re-init from the EDN file in a fresh registry restores and wakes once (the
   restart probe), a deferred spawn is re-driven, and a corrupt registry file
   is moved aside rather than overwritten."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.resumer-conformance :as conformance]
            [hive-mcp.await.sources.manual :as manual]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private resumes
  ;; Every resume request the :test-stub resumer received, in order.
  (atom []))

(defonce ^:private stub-answer
  ;; What the stub answers; reset to a success before each test. A test sets
  ;; it to a deferral or a refusal to drive the re-drive path.
  (atom (fn [_req] {:resumed :ok})))

(defmethod ports/open-resumer :test-stub
  [_owner]
  (fn [req] (swap! resumes conj req) (@stub-answer req)))

(defn- drain-resumes!
  "The recorded resume requests, cleared."
  []
  (let [[old _] (reset-vals! resumes [])] old))

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "await-reg" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- temp-path [] (str (io/file (temp-dir) "registry.edn")))

(use-fixtures :each
  (fn [t]
    (reset! resumes [])
    (reset! stub-answer (fn [_] {:resumed :ok}))
    (t)))

(defn- wait-until
  "Poll `pred` every 10 ms up to `ms`; answers its last value."
  [ms pred]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (pred)]
        (if (or v (> (System/currentTimeMillis) end)) v (do (Thread/sleep 10) (recur)))))))

(defn- registry-on [path & {:as opts}]
  (let [reg (registry/make-registry path (or opts {}))]
    (registry/init! reg)
    reg))

(deftest stub-resumer-conforms
  (conformance/run-cases! {:kind :test-stub} drain-resumes!))

(deftest created-awaiter-is-pending-and-resumes-nothing
  (let [reg (registry-on (temp-path))]
    (try
      (let [a (registry/create! reg {:id "m1" :owner {:kind :test-stub} :prompt "next step"
                                     :subscription {:type :manual :filter {:ci :green}}})]
        (is (= :pending (:status a)) "create leaves the awaiter pending")
        (is (some #{"m1"} (manual/listening)) "its source is open at once")
        (testing "a matching event while pending is RECORDED, never resumed"
          (manual/fire! {:ci :green :sha "abc"} "m1")
          (let [a' (registry/lookup reg "m1")]
            (is (= :pending (:status a')))
            (is (= :event (get-in a' [:pending-wake :reason])))
            (is (empty? @resumes) "no run is resumed before the park is observed")))
        (testing "arming fires the recorded wake exactly once"
          (is (true? (:fired? (registry/arm! reg "m1" "run-1"))))
          (is (wait-until 2000 #(= 1 (count @resumes))))
          (is (= :event (get-in (first @resumes) [:payload :reason])))
          (is (.startsWith ^String (:prompt (first @resumes)) "next step"))
          (is (= "run-1" (get-in (registry/lookup reg "m1") [:owner :run-id]))
              "arm binds the parked run id")
          (is (not (some #{"m1"} (manual/listening))) "source closed on wake"))
        (testing "arming twice delivers nothing twice"
          (registry/arm! reg "m1" "run-1")
          (manual/fire! {:ci :green} "m1")
          (is (= 1 (count @resumes)))))
      (finally (registry/shutdown! reg)))))

(deftest armed-awaiter-wakes-on-a-later-event
  (let [reg (registry-on (temp-path))]
    (try
      (registry/create! reg {:id "a1" :owner {:kind :test-stub}
                             :subscription {:type :manual :filter {:ci :green}}})
      (registry/arm! reg "a1" "run-a1")
      (is (= :armed (:status (registry/lookup reg "a1"))))
      (testing "a non-matching fire does not wake"
        (manual/fire! {:ci :red} "a1")
        (is (= :armed (:status (registry/lookup reg "a1"))))
        (is (empty? @resumes)))
      (testing "a matching fire wakes and resumes exactly once"
        (manual/fire! {:ci :green} "a1")
        (manual/fire! {:ci :green} "a1")
        (is (wait-until 2000 #(= :woken (:status (registry/lookup reg "a1")))))
        (is (= 1 (count @resumes)))
        (is (= {:ok? true :value {:resumed :ok} :attempt 1}
               (dissoc (registry/resume-result reg "a1") :give-up?))))
      (finally (registry/shutdown! reg)))))

(deftest deadline-while-pending-is-recorded-then-fired-on-arm
  (let [reg (registry-on (temp-path))]
    (try
      (registry/create! reg {:id "t1" :owner {:kind :test-stub} :timeout-ms 30
                             :subscription {:type :manual}})
      (is (wait-until 2000 #(= :timeout (get-in (registry/lookup reg "t1")
                                                [:pending-wake :reason])))
          "the deadline is recorded while pending")
      (is (empty? @resumes) "a passed deadline does not resume a run that has not parked")
      (registry/arm! reg "t1" "run-t1")
      (is (wait-until 2000 #(= 1 (count @resumes))))
      (is (= :timeout (get-in (first @resumes) [:payload :reason])))
      (testing "a fire after the timeout does not resume again"
        (manual/fire! {} "t1")
        (is (= 1 (count @resumes))))
      (finally (registry/shutdown! reg)))))

(deftest armed-deadline-wakes-with-timeout
  (let [reg (registry-on (temp-path))]
    (try
      (registry/create! reg {:id "dl-armed" :owner {:kind :test-stub} :timeout-ms 50
                             :subscription {:type :manual}})
      (registry/arm! reg "dl-armed" "run-d1")
      (is (wait-until 3000 #(= 1 (count @resumes))))
      (is (= :timeout (get-in (first @resumes) [:payload :reason])))
      (is (= :woken (:status (registry/lookup reg "dl-armed"))))
      (finally (registry/shutdown! reg)))))

(deftest registered-but-unarmed-wakes-are-not-delivered
  (let [reg (registry-on (temp-path))]
    (try
      (registry/create! reg {:id "u1" :owner {:kind :test-stub}
                             :subscription {:type :manual}})
      (dotimes [_ 3] (manual/fire! {} "u1"))
      (is (empty? @resumes) "an unarmed awaiter records one wake and delivers none")
      (is (= 1 (count (registry/awaiters reg))))
      (finally (registry/shutdown! reg)))))

(deftest restart-probe-rearms-and-wakes-once
  (let [path (temp-path)
        reg1 (registry-on path)]
    (registry/create! reg1 {:id "r1" :owner {:kind :test-stub} :prompt "after restart"
                            :subscription {:type :manual :dedup-key :sha}})
    (registry/create! reg1 {:id "r2" :owner {:kind :test-stub}
                            :subscription {:type :manual}})
    (registry/arm! reg1 "r1" "run-r1")
    (registry/cancel! reg1 "r2")
    ;; The process dies: sources and timers go, the file and the lock stay.
    (registry/shutdown! reg1)
    (manual/fire! {:sha "lost"} "r1")
    (is (empty? @resumes) "nothing listens once the first registry is gone")
    (testing "a second init restores and wakes exactly once"
      (let [reg2 (registry/make-registry path)]
        (let [report (registry/init! reg2)]
          (is (= 2 (:loaded report)))
          (is (= ["r1"] (:armed report)) "only the live awaiter is restored")
          (is (empty? (:failed report)))
          (is (nil? (:corrupt-aside report))))
        (is (= :cancelled (:status (registry/lookup reg2 "r2"))))
        (manual/fire! {:sha "abc"})
        (manual/fire! {:sha "abc"})
        (manual/fire! {:sha "def"})
        (is (wait-until 2000 #(= 1 (count @resumes))) "exactly one wake after re-init")
        (is (= "r1" (get-in (first @resumes) [:awaiter :id])))
        (registry/shutdown! reg2)))
    (testing "the wake is persisted: a third init restores nothing to wake"
      (let [reg3 (registry/make-registry path)]
        (is (= [] (:armed (registry/init! reg3))))
        (is (= :woken (:status (registry/lookup reg3 "r1"))))
        (registry/shutdown! reg3)))))

(deftest restart-probe-deadline-passed-while-down
  (let [path  (temp-path)
        clock (atom 1000)]
    (let [reg1 (registry-on path :now-fn #(deref clock))]
      (registry/create! reg1 {:id "p1" :owner {:kind :test-stub} :deadline 1000000
                              :subscription {:type :manual}})
      (registry/arm! reg1 "p1" "run-p1")
      (registry/shutdown! reg1))
    (reset! clock 2000000)
    (let [reg2 (registry-on path :now-fn #(deref clock))]
      (try
        (is (wait-until 2000 #(= 1 (count @resumes))))
        (is (= :timeout (get-in (first @resumes) [:payload :reason])))
        (manual/fire! {} "p1")
        (is (= 1 (count @resumes)))
        (finally (registry/shutdown! reg2))))))

(deftest stale-pending-awaiter-is-cancelled-at-tick
  (let [clock (atom 1000)
        reg   (registry-on (temp-path) :now-fn #(deref clock) :pending-ttl-ms 100)]
    (try
      (registry/create! reg {:id "s1" :owner {:kind :test-stub}
                             :subscription {:type :manual}})
      (is (= [] (registry/tick! reg)) "not stale yet")
      (reset! clock 5000)
      (registry/tick! reg)
      (is (= :cancelled (:status (registry/lookup reg "s1")))
          "an awaiter whose ling never parked is cancelled, not left to leak")
      (is (not (some #{"s1"} (manual/listening))))
      (is (empty? @resumes))
      (finally (registry/shutdown! reg)))))

(deftest deferred-resume-is-redriven
  (let [calls (atom 0)]
    (reset! stub-answer (fn [_] (if (= 1 (swap! calls inc))
                                  {:success false :deferred true}
                                  {:resumed :second-time})))
    (let [reg (registry-on (temp-path))]
      (try
        (registry/create! reg {:id "x1" :owner {:kind :test-stub}
                               :subscription {:type :manual}})
        (registry/arm! reg "x1" "run-x1")
        (manual/fire! {} "x1")
        (testing "the first attempt deferred: the awaiter stays :waking and is retried"
          (is (wait-until 4000 #(= :woken (:status (registry/lookup reg "x1")))))
          (is (= 2 (count @resumes)) "one deferred attempt, one re-driven success")
          (is (= #{"x1"} (set (map #(get-in % [:awaiter :id]) @resumes)))
              "every attempt resumes the same awaiter"))
        (finally (registry/shutdown! reg))))))

(deftest refused-resume-stays-waking-with-its-reason
  (reset! stub-answer (fn [_] (throw (ex-info "spawn refused" {:error :await/resume-refused}))))
  (let [reg (registry-on (temp-path) :max-attempts 2)]
    (try
      (registry/create! reg {:id "f1" :owner {:kind :test-stub}
                             :subscription {:type :manual}})
      (registry/arm! reg "f1" "run-f1")
      (manual/fire! {} "f1")
      (is (wait-until 5000 #(true? (:give-up? (registry/resume-result reg "f1")))))
      (let [a (registry/lookup reg "f1")]
        (is (= :waking (:status a)) "a failed resume is never marked woken")
        (is (= "spawn refused" (:error (registry/resume-result reg "f1"))))
        (is (= 2 (count @resumes)) "retried up to :max-attempts, then left for a human"))
      (finally (registry/shutdown! reg)))))

(deftest waking-awaiter-is-redriven-on-init
  (let [path (temp-path)]
    ;; A crash mid-resume leaves this on disk: :waking, with its wake, and no
    ;; recorded outcome. Written directly, because that is the state no API
    ;; call produces.
    (registry/write-awaiters!
     path
     {"w1" {:id "w1" :owner {:kind :test-stub} :subscription {:type :manual}
            :deadline nil :prompt nil :status :waking :created 1 :seen #{}
            :pending-wake nil :wake {:reason :event :event {:type :manual :data {}}}
            :attempt 1}})
    (let [reg2 (registry-on path)]
      (try
        (testing "init puts an in-flight wake back in flight instead of losing it"
          (is (wait-until 4000 #(= 1 (count @resumes))))
          (is (= :woken (:status (registry/lookup reg2 "w1")))))
        (finally (registry/shutdown! reg2))))))

(deftest corrupt-registry-file-is-moved-aside
  (let [path (temp-path)]
    (spit (io/file path) "{:version 2 :awaiters {")
    (let [reg (registry/make-registry path)]
      (try
        (let [report (registry/init! reg)]
          (is (= 0 (:loaded report)))
          (is (some? (:corrupt-aside report)) "the damaged file is preserved, not overwritten")
          (is (.exists (io/file (:corrupt-aside report))))
          (is (.exists (io/file path)) "a fresh registry exists after init persists"))
        (finally (registry/shutdown! reg))))))

(deftest one-registry-per-file-is-enforced
  (let [path (temp-path)
        reg1 (registry-on path)]
    (try
      (let [reg2 (registry/make-registry path)]
        (is (= :await/registry-locked
               (:error (ex-data (try (registry/init! reg2)
                                     (catch clojure.lang.ExceptionInfo e e)))))
            "a second registry over one file is refused, so neither erases the other"))
      (finally (registry/shutdown! reg1)))
    (let [reg3 (registry-on path)]
      (try
        (is (map? (registry/init! reg3)) "the lock is released by shutdown!")
        (finally (registry/shutdown! reg3))))))

(deftest duplicate-create-is-refused
  (let [reg (registry-on (temp-path))]
    (try
      (registry/create! reg {:id "dup" :owner {:kind :test-stub}
                             :subscription {:type :manual}})
      (is (= :await/duplicate
             (:error (ex-data (try (registry/create! reg {:id "dup" :owner {:kind :test-stub}
                                                          :subscription {:type :manual}})
                                   (catch clojure.lang.ExceptionInfo e e))))))
      (is (= 1 (count (registry/awaiters reg))))
      (finally (registry/shutdown! reg)))))

(deftest unknown-ports-refused-before-recording
  (let [path (temp-path)
        reg  (registry-on path)]
    (try
      (is (= :await/unknown-resumer
             (:error (ex-data (try (registry/create! reg {:id "u1" :owner {:kind :nobody}
                                                          :subscription {:type :manual}})
                                   (catch clojure.lang.ExceptionInfo e e))))))
      (is (= :await/unknown-source
             (:error (ex-data (try (registry/create! reg {:id "u2" :owner {:kind :test-stub}
                                                          :subscription {:type :nowhere}})
                                   (catch clojure.lang.ExceptionInfo e e))))))
      (is (empty? (registry/awaiters reg)))
      (is (= {} (registry/read-awaiters path)))
      (finally (registry/shutdown! reg)))))

(deftest boot-restores-and-is-idempotent-in-one-jvm
  (let [path (temp-path)
        prop "hive.await.registry"
        old  (System/getProperty prop)]
    (System/setProperty prop path)
    (try
      (let [reg1 (registry-on path)]
        (registry/create! reg1 {:id "b1" :owner {:kind :test-stub}
                                :subscription {:type :manual}})
        (registry/arm! reg1 "b1" "run-b1")
        (registry/shutdown! reg1))
      (testing "boot! restores the parked awaiter from the file"
        (let [r (registry/boot!)]
          (is (= ["b1"] (:armed r)))
          (is (= :armed (:status (registry/lookup-awaiter "b1"))))))
      (testing "a second boot! in the same JVM releases its own lock and restores again"
        (let [r (registry/boot!)]
          (is (= ["b1"] (:armed r)) "not refused on the lock the first boot! holds")))
      (testing "the restored awaiter wakes once"
        (manual/fire! {} "b1")
        (is (wait-until 2000 #(= 1 (count @resumes)))))
      (finally
        (registry/shutdown-process!)
        (if old (System/setProperty prop old) (System/clearProperty prop))))))

(deftest lookup-awaiter-takes-the-id-alone
  (testing "arity 1, no registry argument: the seam hive-agent calls before it parks"
    (let [prop "hive.await.registry"
          old  (System/getProperty prop)]
      ;; never the operator's registry: a live hive-mcp holds its lock
      (System/setProperty prop (temp-path))
      (try
        (registry/shutdown-process!)
        (is (fn? registry/lookup-awaiter))
        (is (nil? (registry/lookup-awaiter (str "no-such-awaiter-" (random-uuid)))))
        (finally
          (registry/shutdown-process!)
          (if old (System/setProperty prop old) (System/clearProperty prop)))))))
