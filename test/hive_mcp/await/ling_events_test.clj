(ns hive-mcp.await.ling-events-test
  "Acceptance for the :parked listener that closes the two-phase park.

   Driven through the ports only: the listener is registered as an
   IDeliveryChannel and every shout reaches it through
   hive-mcp.protocols.delivery-channel/fanout!, the same fanout a hivemind
   shout takes in-process and from NATS. The registry is a real one over a
   temp file; the owner's resumer is a stub kind registered on the
   open-resumer port. No var is redefined.

   The shouts are shaped as hive-agent (#191) emits them: the exit shout's
   :data is hive-agent.loop.exit-signal/exit-data, {:termination :parked
   :awaiter-id :run-id :ling-id} for a park."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.await.ling-events :as le]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.sources.manual :as manual]
            [hive-mcp.protocols.delivery-channel :as dc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private resumes
  ;; Every resume request the :ling-events-stub resumer received.
  (atom []))

(defmethod ports/open-resumer :ling-events-stub
  [_owner]
  (fn [req] (swap! resumes conj req) {:resumed :ok}))

(defn- temp-path []
  (str (io/file (.toFile (java.nio.file.Files/createTempDirectory
                          "await-le" (make-array java.nio.file.attribute.FileAttribute 0)))
                "registry.edn")))

(defn- wait-until
  "Poll `pred` every 10 ms up to `ms`; answers its last value."
  [ms pred]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (pred)]
        (if (or v (> (System/currentTimeMillis) end)) v (do (Thread/sleep 10) (recur)))))))

(def ^:dynamic *reg*
  "The registry of the running test."
  nil)

(use-fixtures :each
  (fn [t]
    (reset! resumes [])
    (let [reg (registry/make-registry (temp-path))]
      (registry/init! reg)
      ;; inline submit: the action runs on the fanout's delivery thread
      (le/install! (le/listener-channel (constantly reg) (fn [f] (f))))
      (try
        (binding [*reg* reg] (t))
        (finally
          (le/uninstall!)
          (registry/shutdown! reg))))))

(defn- create! [id ling-id]
  (registry/create! *reg* {:id id
                           :owner {:kind :ling-events-stub :ling-id ling-id}
                           :prompt "continue"
                           :subscription {:type :manual}}))

(defn- shout!
  "Fan one ling shout out through the delivery-channel port, as
   hivemind.messaging/route-shout! does."
  [agent-id event-type data & {:as extra}]
  (dc/fanout! (merge {:agent-id agent-id :event-type event-type
                      :timestamp (System/currentTimeMillis)
                      :project-id "hive-mcp" :data data :via :local-origin}
                     extra)))

(defn- status [id] (:status (registry/lookup *reg* id)))

(deftest the-listener-is-a-delivery-channel
  (is (some #{le/listener-id} (map dc/channel-id (dc/get-channels)))
      "install! registers it on the IDeliveryChannel port"))

(deftest parked-shout-arms-the-awaiter-with-its-run-id
  (create! "p1" "ling-1")
  (is (= :pending (status "p1")))
  (shout! "ling-1" :parked {:termination :parked :awaiter-id "p1"
                            :run-id "run-77" :ling-id "ling-1"})
  (is (wait-until 2000 #(= :armed (status "p1"))) "the :parked shout arms it")
  (is (= "run-77" (get-in (registry/lookup *reg* "p1") [:owner :run-id]))
      "the parked run id is bound for the resume")
  (testing "the armed awaiter now wakes on its event and resumes once"
    (manual/fire! {} "p1")
    (is (wait-until 2000 #(= :woken (status "p1"))))
    (is (= 1 (count @resumes)))))

(deftest a-wake-recorded-while-pending-fires-on-the-parked-shout
  (create! "p2" "ling-2")
  (manual/fire! {:ci :green} "p2")
  (is (empty? @resumes) "pending: recorded, not resumed")
  (shout! "ling-2" "parked" {"termination" "parked" "awaiter-id" "p2" "run-id" "run-2"})
  (is (wait-until 2000 #(= 1 (count @resumes)))
      "string keys (a JSON hop) arm too, and the recorded wake fires")
  (is (= :event (get-in (first @resumes) [:payload :reason]))))

(deftest a-ling-that-ends-without-parking-cancels-its-pending-awaiters
  (create! "c1" "ling-3")
  (create! "c2" "ling-3")
  (create! "keep" "ling-other")
  (shout! "ling-3" :completed {:termination :completed :run-id "run-3"})
  (is (wait-until 2000 #(and (= :cancelled (status "c1")) (= :cancelled (status "c2")))))
  (is (= :pending (status "keep")) "another ling's awaiter is untouched")
  (is (not (some #{"c1" "c2"} (manual/listening))) "their sources are closed")
  (testing "every other terminal event cancels the same way"
    (doseq [[i et] (map-indexed vector [:error :truncated :context-death :aborted])]
      (let [id (str "e" i) ling (str "ling-e" i)]
        (create! id ling)
        (shout! ling et {:termination et})
        (is (wait-until 2000 #(= :cancelled (status id))) (str et " cancels"))))))

(deftest an-armed-awaiter-survives-its-ling-ending
  (create! "a1" "ling-4")
  (shout! "ling-4" :parked {:termination :parked :awaiter-id "a1" :run-id "run-4"})
  (is (wait-until 2000 #(= :armed (status "a1"))))
  (shout! "ling-4" :completed {:termination :completed})
  (Thread/sleep 100)
  (is (= :armed (status "a1")) "only :pending awaiters are cancelled"))

(deftest shouts-that-are-not-a-runtime-exit-change-nothing
  (create! "n1" "ling-5")
  (testing "a deliberate 'completed' shout from the hivemind tool, mid-run"
    (shout! "ling-5" :completed {:termination :completed} :deliberate? true))
  (testing "a completed shout without :termination (not an exit shout)"
    (shout! "ling-5" :completed {:result "half done"}))
  (testing "progress and started"
    (shout! "ling-5" :progress {:message "working"})
    (shout! "ling-5" :started {:termination :completed}))
  (Thread/sleep 100)
  (is (= :pending (status "n1"))))

(deftest a-parked-shout-cannot-arm-another-lings-awaiter
  (create! "o1" "ling-owner")
  (shout! "ling-intruder" :parked {:termination :parked :awaiter-id "o1" :run-id "run-x"})
  (Thread/sleep 100)
  (is (= :pending (status "o1")))
  (is (= :other-ling (:refused (le/apply-action! *reg* {:op :arm :awaiter-id "o1"
                                                         :run-id "run-x"
                                                         :ling-id "ling-intruder"}))))
  (is (= :unknown-awaiter (:refused (le/apply-action! *reg* {:op :arm :awaiter-id "nope"
                                                              :run-id nil :ling-id nil})))))

(deftest a-resumed-lings-parked-shout-arms-its-earlier-awaiter
  (testing "park -> wake -> the successor parks on an awaiter naming the original id -> wake"
    (create! "g1" "lander")
    (shout! "lander" :parked {:termination :parked :awaiter-id "g1" :run-id "run-1"})
    (is (wait-until 2000 #(= :armed (status "g1"))))
    (manual/fire! {} "g1")
    (is (wait-until 2000 #(= :woken (status "g1"))))
    (create! "g2" "lander")
    (shout! "lander-w-g1-1" :parked {:termination :parked :awaiter-id "g2" :run-id "run-2"})
    (is (wait-until 2000 #(= :armed (status "g2"))) "the successor's park arms it")
    (manual/fire! {} "g2")
    (is (wait-until 2000 #(= :woken (status "g2"))))
    (is (= 2 (count @resumes))))
  (testing "a chained resume name is the same ling; a mere prefix is not"
    (is (le/same-ling? "lander" "lander-w-1234abcd-1-r2-w-9f9f9f9f-3"))
    (is (not (le/same-ling? "land" "lander")))
    (is (not (le/same-ling? "lander-w-1234abcd-1" "lander")))
    (is (not (le/same-ling? nil "lander")))))

(deftest classify-reads-the-exit-shout
  (is (= {:op :arm :awaiter-id "x" :run-id "r" :ling-id "l"}
         (le/classify {:agent-id "l" :event-type :parked
                       :data {:termination :parked :awaiter-id "x" :run-id "r"}})))
  (is (nil? (le/classify {:agent-id "l" :event-type :parked :data {:termination :parked}}))
      "a park with no awaiter id names nothing to arm")
  (is (= {:op :cancel-owned :ling-id "l" :event-type :error}
         (le/classify {:agent-id "l" :event-type "error" :data {:termination :error}})))
  (is (nil? (le/classify nil)))
  (is (nil? (le/classify {:event-type :completed :data {:termination :completed}}))
      "an exit shout naming no ling cancels nothing"))
