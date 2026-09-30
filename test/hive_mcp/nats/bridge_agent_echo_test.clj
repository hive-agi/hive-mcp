(ns hive-mcp.nats.bridge-agent-echo-test
  "Shouts always fan out locally, so the NATS echo of this process's OWN agent
   events must not be auto-shouted a second time. publish-agent-event! stamps
   its payload with the node-id; the agent handlers skip the auto-shout for a
   self-stamped message and keep the lifecycle republish unconditional. A
   failed event reads its error from :error or from :result :error. No NATS
   server is contacted: the backbone is a recording stub."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.nats.bridge :as bridge]
            [hive-mcp.protocols.event-backbone :as eb]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private stamp-self (deref #'bridge/stamp-self))
(def ^:private self-publish? (deref #'bridge/self-publish?))
(def ^:private handle-agent-completed (deref #'bridge/handle-agent-completed))
(def ^:private handle-agent-failed (deref #'bridge/handle-agent-failed))

(defn- recording-backbone
  "An IEventBackbone that records every publish into `sink`."
  [sink]
  (reify eb/IEventBackbone
    (backbone-id [_] :recording)
    (connected? [_] true)
    (publish! [_ subject payload] (swap! sink conj [subject payload]) true)
    (subscribe! [_ _ _] nil)
    (unsubscribe! [_ _] nil)))

(defn- run-handler
  "Run `handler` on `msg`; return {:shouts [...] :lifecycle [...]} it produced."
  [handler msg]
  (let [shouts    (atom [])
        lifecycle (atom [])]
    (with-redefs [bridge/auto-shout-agent-event!
                  (fn [ling-id event-type summary]
                    (swap! shouts conj {:ling-id ling-id :event-type event-type
                                        :message summary}))
                  bridge/republish-lifecycle-locally!
                  (fn [event] (swap! lifecycle conj event))]
      (handler msg))
    {:shouts @shouts :lifecycle @lifecycle}))

(def ^:private foreign-node {:nats/source-node "some-other-coordinator"})

;; =============================================================================
;; Publisher: agent events carry this process's node-id
;; =============================================================================

(deftest publish-agent-event-stamps-self
  (let [sink (atom [])]
    (with-redefs [eb/get-backbone (fn [] (recording-backbone sink))]
      (bridge/publish-agent-event! {:event-type :completed :ling-id "ling-1"}))
    (let [[[subject payload]] @sink]
      (is (= "hive.v1.agent.completed.ling-1" subject))
      (is (self-publish? payload)
          "the NATS echo of our own agent event must be recognisable as ours"))))

;; =============================================================================
;; Subscriber: completed
;; =============================================================================

(deftest self-stamped-completion-is-not-shouted
  (let [{:keys [shouts lifecycle]}
        (run-handler handle-agent-completed
                     (stamp-self {:ling-id "ling-1" :task-id "t-1"
                                  :result {:result "done"}}))]
    (is (empty? shouts) "our own completion is already audible in-JVM")
    (is (= [:task-completed :slave-killed] (mapv :type lifecycle))
        "lifecycle republish stays unconditional")))

(deftest foreign-completion-is-shouted
  (let [{:keys [shouts lifecycle]}
        (run-handler handle-agent-completed
                     (merge foreign-node {:ling-id "ling-2" :task-id "t-2"
                                          :result {:result "done"}}))]
    (is (= 1 (count shouts)))
    (is (= :completed (:event-type (first shouts))))
    (is (= [:task-completed :slave-killed] (mapv :type lifecycle)))))

;; =============================================================================
;; Subscriber: failed
;; =============================================================================

(deftest self-stamped-failure-is-not-shouted
  (let [{:keys [shouts lifecycle]}
        (run-handler handle-agent-failed
                     (stamp-self {:ling-id "ling-3" :task-id "t-3" :error "boom"}))]
    (is (empty? shouts))
    (is (= [:task-failed :slave-killed] (mapv :type lifecycle)))
    (is (= "boom" (:error (first lifecycle))))))

(deftest foreign-failure-is-shouted
  (let [{:keys [shouts]}
        (run-handler handle-agent-failed
                     (merge foreign-node {:ling-id "ling-4" :error "boom"}))]
    (is (= [:error] (mapv :event-type shouts)))
    (is (str/includes? (:message (first shouts)) "boom"))))

(deftest failure-reads-error-nested-in-result
  (testing "an error carried only under :result reaches both shout and :task-failed"
    (let [{:keys [shouts lifecycle]}
          (run-handler handle-agent-failed
                       (merge foreign-node {:ling-id "ling-5" :task-id "t-5"
                                            :result {:error "nested-boom"}}))]
      (is (str/includes? (:message (first shouts)) "nested-boom")
          "shout must not fall back to 'unknown error'")
      (is (= "nested-boom" (:error (first lifecycle))))))
  (testing "a top-level :error wins over the nested one"
    (let [{:keys [lifecycle]}
          (run-handler handle-agent-failed
                       (merge foreign-node {:ling-id "ling-6" :task-id "t-6"
                                            :error "top"
                                            :result {:error "nested"}}))]
      (is (= "top" (:error (first lifecycle)))))))
