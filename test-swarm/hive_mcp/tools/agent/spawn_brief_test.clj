(ns hive-mcp.tools.agent.spawn-brief-test
  "A spawn's initial brief reaches the backend, and the response says whether
   one was attached."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.test.stub.headless-backend :as hb]
            [hive-test.isolation :as iso]
            [hive-mcp.isolation-methods]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (iso/with-isolations :swarm-ds))

(deftest spawn-brief-picks-task-then-prompt
  (is (= "a" (spawn/spawn-brief {:task "a"})))
  (is (= "b" (spawn/spawn-brief {:prompt "b"})))
  (is (= "b" (spawn/spawn-brief {:task "  " :prompt "b"})))
  (is (= "same" (spawn/spawn-brief {:task "same" :prompt "same"})))
  (is (nil? (spawn/spawn-brief {})))
  (is (nil? (spawn/spawn-brief {:task "" :prompt " "}))))

(deftest spawn-brief-refuses-disagreeing-task-and-prompt
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"disagree"
                        (spawn/spawn-brief {:task "a" :prompt "b"}))))

(defn- spawn-headless
  "Spawn a headless ling through the handler against stub backend B.
   Returns [response spawn-opts-seen-by-backend]."
  [b params]
  (hb/with-backend :hive-agent b
    (let [resp (spawn/handle-spawn (merge {:type "ling"
                                           :cwd "/tmp/spawn-brief"
                                           :spawn_mode "headless"
                                           :model "venice:test-model"}
                                          params))]
      [resp (some-> (hb/calls-of b :spawn!) first second)])))

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(defn- ends-with-brief? [task brief]
  (and (string? task) (.endsWith ^String task brief)))

(deftest prompt-at-spawn-reaches-the-backend
  (testing "prompt is delivered as the initial task, not dropped"
    (let [[resp opts] (spawn-headless (hb/->backend :hive-agent)
                                      {:name "brief-prompt" :prompt "write the report"})]
      (is (not (:isError resp)) (:text resp))
      (is (ends-with-brief? (:task opts) "write the report"))
      (is (true? (:task-attached (body resp))))
      (is (nil? (:warning (body resp)))))))

(deftest task-at-spawn-reaches-the-backend
  (let [[resp opts] (spawn-headless (hb/->backend :hive-agent)
                                    {:name "brief-task" :task "fix the bug"})]
    (is (ends-with-brief? (:task opts) "fix the bug"))
    (is (true? (:task-attached (body resp))))))

(deftest spawn-without-a-brief-says-so
  (let [[resp opts] (spawn-headless (hb/->backend :hive-agent) {:name "brief-none"})]
    (is (not (:isError resp)))
    (is (nil? (:task opts)))
    (is (false? (:task-attached (body resp))))
    (is (re-find #"no task" (:warning (body resp))))))

(deftest disagreeing-brief-is-an-error-and-spawns-nothing
  (let [b (hb/->backend :hive-agent)
        [resp opts] (spawn-headless b {:name "brief-clash" :task "a" :prompt "b"})]
    (is (:isError resp))
    (is (re-find #"disagree" (:text resp)))
    (is (nil? opts))))
