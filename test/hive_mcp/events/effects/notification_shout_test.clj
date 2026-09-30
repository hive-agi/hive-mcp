(ns hive-mcp.events.effects.notification-shout-test
  "The :shout effect forwards top-level :message / :task.

   Why: ShoutEffectData allows :message and :task at the top level of the
   effect, and session_complete and the crystal wrap handler put the
   completion message there. handle-shout forwarded only :data, so those
   shouts reached shout! with no payload and were suppressed as empty — the
   coordinator never heard a ling finish its session."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.events.core :as ev]
            [hive-mcp.events.effects.notification :as notif]
            [hive-mcp.hivemind.core]
            [hive-mcp.hivemind.messaging :as msg]
            [hive-mcp.tools.session-complete :as session-complete]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [t] (ev/with-clean-registry (t))))

;; =============================================================================
;; Pure: shout-effect->shout-data
;; =============================================================================

(deftest payload-keys-derive-from-schema
  (is (= #{:message :task} (set notif/shout-effect-payload-keys))))

(deftest top-level-message-is-promoted
  (is (= {:message "Session complete: x"}
         (notif/shout-effect->shout-data
          {:agent-id "ling-1" :event-type :completed
           :message "Session complete: x"}))))

(deftest top-level-keys-merge-with-data
  (is (= {:message "m" :task "t" :task-id "k"}
         (notif/shout-effect->shout-data
          {:event-type :completed :message "m" :task "t"
           :data {:task-id "k"}}))))

(deftest explicit-data-wins-over-top-level
  (is (= {:message "from-data"}
         (notif/shout-effect->shout-data
          {:event-type :completed :message "top" :data {:message "from-data"}}))))

(deftest data-only-effect-is-unchanged
  (let [data {:task-id "k" :result :ok}]
    (is (identical? data (notif/shout-effect->shout-data
                          {:event-type :completed :data data})))))

;; =============================================================================
;; Integration: the registered :shout effect handler
;; =============================================================================

(defn- run-shout-effect
  "Run the registered :shout fx handler on `effect`; -> the shout! call."
  [effect]
  (notif/register-notification-effects!)
  (let [shouted (atom nil)]
    (with-redefs [hive-mcp.hivemind.core/shout!
                  (fn [aid etype data]
                    (reset! shouted {:agent-id aid :event-type etype :data data})
                    true)]
      ((ev/get-fx-handler :shout) effect))
    @shouted))

(deftest session-complete-shout-is-not-empty
  (testing "the effect built by session_complete reaches shout! with its message"
    (let [build (deref #'session-complete/build-session-effects)
          effect (:shout (build "feat: x" [] "ling-7" "/tmp"))
          {:keys [agent-id event-type data]} (run-shout-effect effect)]
      (is (= "ling-7" agent-id))
      (is (= :completed event-type))
      (is (= "Session complete: feat: x" (:message data)))
      (is (not (msg/empty-shout? data))
          "a completion shout must not be suppressed as empty"))))

(deftest crystal-wrap-shaped-shout-is-not-empty
  (testing "a top-level :message effect (crystal wrap shape) keeps its message"
    (let [{:keys [data]} (run-shout-effect {:agent-id "ling-8"
                                            :event-type :completed
                                            :message "3 notes, 1 decision"})]
      (is (= "3 notes, 1 decision" (:message data)))
      (is (not (msg/empty-shout? data))))))
