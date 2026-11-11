(ns hive-mcp.await.registry-test
  "Registry acceptance: a manual fire resumes through the resumer port, a
   deadline wakes with :timeout, and a re-init from the EDN file in a fresh
   registry re-arms and wakes exactly once (the restart probe)."
  (:require [clojure.java.io :as io]
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

(defmethod ports/open-resumer :test-stub
  [_owner]
  (fn [req] (swap! resumes conj req) {:resumed (get-in req [:awaiter :id])}))

(defn- drain-resumes!
  "The recorded resume requests, cleared."
  []
  (let [[old _] (reset-vals! resumes [])] old))

(defn- temp-path []
  (let [d (.toFile (java.nio.file.Files/createTempDirectory
                    "await-reg" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (str (io/file d "registry.edn"))))

(use-fixtures :each (fn [t] (reset! resumes []) (t)))

(defn- wait-until
  "Poll `pred` every 10 ms up to `ms`; answers its last value."
  [ms pred]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (pred)]
        (if (or v (> (System/currentTimeMillis) end)) v (do (Thread/sleep 10) (recur)))))))

(deftest stub-resumer-conforms
  (conformance/run-cases! {:kind :test-stub} drain-resumes!))

(deftest manual-fire-resumes-once
  (let [reg (registry/make-registry (temp-path))]
    (try
      (registry/init! reg)
      (let [a (registry/create! reg {:id "m1" :owner {:kind :test-stub} :prompt "next step"
                                     :subscription {:type :manual :filter {:ci :green}}})]
        (is (= :armed (:status a)))
        (is (some #{"m1"} (manual/listening)))
        (testing "a non-matching fire does not wake"
          (manual/fire! {:ci :red} "m1")
          (is (= :armed (:status (registry/lookup reg "m1"))))
          (is (empty? @resumes)))
        (testing "a matching fire wakes and resumes exactly once"
          (manual/fire! {:ci :green} "m1")
          (manual/fire! {:ci :green} "m1")
          (is (= :woken (:status (registry/lookup reg "m1"))))
          (is (= 1 (count @resumes)))
          (is (= :event (get-in (first @resumes) [:payload :reason])))
          (is (.startsWith ^String (:prompt (first @resumes)) "next step"))
          (is (= {:ok? true :value {:resumed "m1"}} (registry/resume-result reg "m1")))
          (is (not (some #{"m1"} (manual/listening))) "source closed on wake")))
      (finally (registry/shutdown! reg)))))

(deftest deadline-wakes-with-timeout
  (let [reg (registry/make-registry (temp-path))]
    (try
      (registry/init! reg)
      (registry/create! reg {:id "t1" :owner {:kind :test-stub} :timeout-ms 50
                             :subscription {:type :manual}})
      (is (wait-until 2000 #(= :woken (:status (registry/lookup reg "t1")))))
      (is (= 1 (count @resumes)))
      (is (= :timeout (get-in (first @resumes) [:payload :reason])))
      (testing "a fire after the timeout does not resume again"
        (manual/fire! {} "t1")
        (is (= 1 (count @resumes))))
      (finally (registry/shutdown! reg)))))

(deftest restart-probe-rearms-and-wakes-once
  (let [path (temp-path)
        reg1 (registry/make-registry path)]
    (registry/init! reg1)
    (registry/create! reg1 {:id "r1" :owner {:kind :test-stub} :prompt "after restart"
                            :subscription {:type :manual :dedup-key :sha}})
    (registry/create! reg1 {:id "r2" :owner {:kind :test-stub}
                            :subscription {:type :manual}})
    (registry/cancel! reg1 "r2")
    ;; The process dies: sources and timers go, the file stays.
    (registry/shutdown! reg1)
    (manual/fire! {:sha "lost"} "r1")
    (is (empty? @resumes) "nothing listens once the first registry is gone")
    (let [reg2 (registry/make-registry path)]
      (try
        (let [report (registry/init! reg2)]
          (is (= 2 (:loaded report)))
          (is (= ["r1"] (:armed report)) "only the armed awaiter is re-armed")
          (is (empty? (:failed report))))
        (is (= :cancelled (:status (registry/lookup reg2 "r2"))))
        (manual/fire! {:sha "abc"})
        (manual/fire! {:sha "abc"})
        (manual/fire! {:sha "def"})
        (is (= 1 (count @resumes)) "exactly one wake after re-init")
        (is (= "r1" (get-in (first @resumes) [:awaiter :id])))
        (testing "the wake is persisted: a third init re-arms nothing"
          (let [reg3 (registry/make-registry path)]
            (try
              (is (= [] (:armed (registry/init! reg3))))
              (is (= :woken (:status (registry/lookup reg3 "r1"))))
              (finally (registry/shutdown! reg3)))))
        (finally (registry/shutdown! reg2))))))

(deftest restart-probe-deadline-passed-while-down
  (let [path  (temp-path)
        clock (atom 1000)
        reg1  (registry/make-registry path {:now-fn #(deref clock)})]
    (registry/init! reg1)
    (registry/create! reg1 {:id "d1" :owner {:kind :test-stub} :deadline 1000000
                            :subscription {:type :manual}})
    (registry/shutdown! reg1)
    (reset! clock 2000000)
    (let [reg2 (registry/make-registry path {:now-fn #(deref clock)})]
      (try
        (registry/init! reg2)
        (is (wait-until 2000 #(= 1 (count @resumes))))
        (is (= :timeout (get-in (first @resumes) [:payload :reason])))
        (manual/fire! {} "d1")
        (is (= 1 (count @resumes)))
        (finally (registry/shutdown! reg2))))))

(deftest unknown-ports-refused-before-recording
  (let [path (temp-path)
        reg  (registry/make-registry path)]
    (try
      (registry/init! reg)
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
