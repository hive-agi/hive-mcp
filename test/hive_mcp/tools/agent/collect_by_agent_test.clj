(ns hive-mcp.tools.agent.collect-by-agent-test
  "agent collect with agent_id alone collects that agent's latest task.
   Task lookup is a stub port; the result comes from the JVM journal."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [hive-mcp.tools.agent.lifecycle :as lifecycle]
            [hive-mcp.tools.swarm.channel :as channel]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(deftest latest-task-id-is-the-newest-started
  (is (nil? (lifecycle/latest-task-id [])))
  (is (= "t2" (lifecycle/latest-task-id
               [{:task/id "t1" :task/started-at (java.util.Date. 10)}
                {:task/id "t2" :task/started-at (java.util.Date. 30)}
                {:task/id "t3" :task/started-at (java.util.Date. 20)}]))))

(deftest agent-id-alone-collects-the-latest-task
  (let [older (str "task-old-" (random-uuid))
        newer (str "task-new-" (random-uuid))
        tasks {"ling-1" [{:task/id older :task/started-at (java.util.Date. 1)}
                         {:task/id newer :task/started-at (java.util.Date. 2)}]}
        tasks-for #(get tasks %)]
    (channel/record-task-result! newer {:status "completed" :result "done"})
    (testing "agent_id alone resolves to the newest task"
      (is (= newer (:task_id (body (lifecycle/handle-collect tasks-for {:agent_id "ling-1"}))))))
    (testing "task_id still wins"
      (channel/record-task-result! older {:status "completed" :result "older"})
      (is (= older (:task_id (body (lifecycle/handle-collect tasks-for {:agent_id "ling-1" :task_id older}))))))
    (testing "an agent with no task, and no ids at all, are MCP errors"
      (is (:isError (lifecycle/handle-collect tasks-for {:agent_id "ghost"})))
      (is (:isError (lifecycle/handle-collect tasks-for {}))))))
