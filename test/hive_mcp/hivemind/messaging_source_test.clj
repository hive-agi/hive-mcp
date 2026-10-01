(ns hive-mcp.hivemind.messaging-source-test
  "The core compat hivemind piggyback source.

   The pure projection is pinned on registry SNAPSHOTS (values, no atom). The
   source itself is exercised in the environment this alias provides, the one
   the seam was about: hive-agent is NOT on the :test classpath, so the swarm
   registry resolves to nil per call and the source must yield no rows rather
   than throw."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.piggyback :as pb]
            [hive-mcp.channel.piggyback.sources :as sources]
            [hive-mcp.hivemind.messaging :as messaging]
            [hive-mcp.hivemind.state :as state]
            [hive-mcp.swarm.delegate :as delegate]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private snapshot
  {"ling-a" {:data {:messages [{:event-type :progress :message "turn 1"
                                :timestamp 10 :project-id "p" :shout-id "s1"
                                :parent-id "coordinator"}
                               {:event-type :completed :task "done"
                                :timestamp 11 :to "ling-b" :context-id "c1"
                                :deliberate? true}]}}
   "ling-b" {:data {:messages []}}})

(deftest registry->messages-projects-a-snapshot-test
  (is (= [] (vec (messaging/registry->messages nil))))
  (is (= [] (vec (messaging/registry->messages {}))))
  (is (= [{:agent-id "ling-a" :event-type :progress :message "turn 1"
           :timestamp 10 :project-id "p" :shout-id "s1" :parent-id "coordinator"}
          {:agent-id "ling-a" :event-type :completed :message "done" :task "done"
           :timestamp 11 :project-id "global" :to "ling-b" :context-id "c1"
           :deliberate? true}]
         (vec (messaging/registry->messages snapshot)))
      "routing keys ride along; a missing message falls back to the task"))

(deftest the-hivemind-source-yields-nothing-without-the-swarm-test
  (when-not (delegate/available? "hive-agent.swarm.hivemind.state")
    (testing "resolved per call: nil registry, no rows, no exception"
      (is (nil? (state/current-agent-registry)))
      (is (= [] (sources/messages-from
                 {::hivemind #'hive-mcp.hivemind.messaging/all-hivemind-messages}))))))

(deftest the-hivemind-source-is-registered-by-var-test
  (let [slot @pb/message-source-fn]
    (try
      (require 'hive-mcp.hivemind.messaging :reload)
      (is (= #'hive-mcp.hivemind.messaging/all-hivemind-messages @pb/message-source-fn)
          "the slot holds the VAR, so a reload of the source is observed")
      (finally (pb/register-message-source! slot)))))
