(ns hive-mcp.hivemind.messaging.ask-timeout-test
  "An ask that times out is announced, not silently dropped.

   Why: ask! dissoc'ed the pending entry identically on answer and on timeout,
   and the entry carried no project or spawner, so an agent that stalled
   waiting for a human left no trace anyone could hear. The entry now records
   :project-id, :parent-id and :asked-at, and a timeout shouts the asker
   :blocked {:reason :ask-timeout :ask-id ...} before the entry is dropped.

   No swarm addon: pending-asks, the broadcast channel and shout! are stubbed."
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.hivemind.messaging :as msg]
            [hive-mcp.hivemind.state :as state]
            [hive-mcp.swarm.datascript.queries :as queries]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Pure builders
;; =============================================================================

(deftest pending-entry-carries-scope-and-time
  (let [e (msg/pending-ask-entry {:agent-id "ling-1" :question "q?" :options ["a"]
                                  :response-chan ::ch :project-id "p"
                                  :parent-id "coord" :asked-at 42})]
    (is (= {:question "q?" :options ["a"] :agent-id "ling-1" :response-chan ::ch
            :asked-at 42 :project-id "p" :parent-id "coord"}
           e)))
  (testing "unknown project / parent are omitted, not nil"
    (let [e (msg/pending-ask-entry {:agent-id "ling-1" :question "q?" :asked-at 1})]
      (is (not (contains? e :project-id)))
      (is (not (contains? e :parent-id))))))

(deftest timeout-shout-names-the-ask
  (let [d (msg/ask-timeout-shout "ask-9" {:question "deploy?" :project-id "p"
                                          :parent-id "coord" :asked-at 7}
                                 100)]
    (is (= :ask-timeout (:reason d)))
    (is (= "ask-9" (:ask-id d)))
    (is (= 7 (:asked-at d)))
    (is (= "p" (:project-id d)))
    (is (= "coord" (:parent-id d)))
    (is (re-find #"deploy\?" (:message d)))
    (is (not (msg/empty-shout? d)))))

;; =============================================================================
;; ask! round trips
;; =============================================================================

(defn- run-ask
  "Run ask! with stubs. `respond?` answers the ask from another thread.
   -> {:result r :shouts [[agent-id event-type data]...] :entry e :after {...}}"
  [respond? & ask-opts]
  (let [asks (atom {})
        shouts (atom [])
        seen-entry (promise)]
    (with-redefs [state/pending-asks asks
                  channel/broadcast! (fn [{:keys [ask-id]}]
                                       (deliver seen-entry (get @asks ask-id))
                                       (when respond?
                                         (async/put! (:response-chan (get @asks ask-id))
                                                     {:decision "yes" :by "human"
                                                      :ask-id ask-id})))
                  queries/get-slave-by-name-or-id (fn [_] {:slave/parent "coord-lane"})
                  msg/shout! (fn [aid et data] (swap! shouts conj [aid et data]) true)]
      (let [result (apply msg/ask! "ling-1" "deploy?" ["yes" "no"] ask-opts)]
        {:result result :shouts @shouts :entry @seen-entry :after @asks}))))

(deftest timeout-shouts-blocked-before-dropping-the-entry
  (let [{:keys [result shouts entry after]}
        (run-ask false :timeout-ms 20 :project-id "proj")]
    (is (:timeout result))
    (testing "the pending entry carried project, spawner and time"
      (is (= "proj" (:project-id entry)))
      (is (= "coord-lane" (:parent-id entry)))
      (is (integer? (:asked-at entry))))
    (testing "the asker is shouted :blocked with the ask id"
      (is (= 1 (count shouts)))
      (let [[aid et data] (first shouts)]
        (is (= "ling-1" aid))
        (is (= :blocked et))
        (is (= :ask-timeout (:reason data)))
        (is (= (:ask-id result) (:ask-id data)))
        (is (= "proj" (:project-id data)))
        (is (= "coord-lane" (:parent-id data)))))
    (is (empty? after) "the entry is dropped after the announcement")))

(deftest answered-ask-shouts-nothing
  (let [{:keys [result shouts after]} (run-ask true :timeout-ms 5000)]
    (is (= "yes" (:decision result)))
    (is (empty? shouts))
    (is (empty? after))))

;; =============================================================================
;; Status neutrality
;; =============================================================================
;; The asker has already resumed with the timeout answer when this shout is
;; heard, so it must not park the asker's slave :blocked.

(deftest timeout-shout-is-status-neutral
  (let [d (msg/ask-timeout-shout "ask-1" {:question "q?"} 10)]
    (is (true? (:status-neutral? d)))
    (is (nil? (msg/shout-slave-status :blocked d))
        "a status-neutral shout sets no slave status")))

(deftest ordinary-shout-still-sets-slave-status
  ;; Both registry lookups are stubbed: shout-slave-status checks the type is
  ;; registered before mapping it, and this tree has no swarm addon.
  (with-redefs [hive-mcp.hivemind.event-registry/valid-event-type?
                (fn [et] (contains? #{:blocked :completed} et))
                hive-mcp.hivemind.event-registry/slave-status
                (fn [et] (get {:blocked :blocked :completed :idle} et))]
    (is (= :blocked (msg/shout-slave-status :blocked {:message "stuck"})))
    (is (= :idle (msg/shout-slave-status :completed {})))))
