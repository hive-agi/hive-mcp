(ns hive-mcp.tools.agent.spawn-time-budget-test
  "time_budget_ms given to `agent spawn` / `agent batch-spawn` reaches the
   headless backend's ctx as :time-budget-ms, the key hive-agent's
   build-spawn-config reads, through a stub backend (no concrete bb-ling).
   It is recorded on the slave row, so a later `agent dispatch` carries it too."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.tools.agent.dispatch :as dispatch]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.tools.consolidated.agent :as agent]
            [hive-mcp.test.stub.headless-backend :as hb]
            [hive-test.isolation :as iso]
            [hive-mcp.isolation-methods]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (iso/with-isolations :swarm-ds))

(def ^:private base
  {:type "ling" :cwd "/tmp/spawn-time-budget" :spawn_mode "headless"
   :model "venice:test-model" :task "x"})

(defn- backend-calls
  "Run F against a stub headless backend; [response spawn-ctxs dispatch-ctxs]."
  [f]
  (let [b (hb/->backend :test-headless)]
    (hb/with-backend :test-headless b
      (let [resp (f)]
        [resp
         (mapv first (hb/calls-of b :spawn!))
         (mapv first (hb/calls-of b :dispatch!))]))))

(defn- slave-row
  "The registry row of the ling spawned as NAME (the stub backend answers
   the id slave-NAME)."
  [name]
  (or (queries/get-slave (str "slave-" name)) (queries/get-slave name)))

(deftest the-agent-tool-advertises-time-budget-ms
  (is (= "integer"
         (get-in agent/tool-def [:inputSchema :properties "time_budget_ms" :type]))))

(deftest spawn-carries-the-time-budget-to-the-backend-ctx
  (let [[resp [ctx]] (backend-calls #(spawn/handle-spawn
                                      (assoc base :name "tm-one" :time_budget_ms 90000)))]
    (is (not (:isError resp)) (:text resp))
    (is (= 90000 (:time-budget-ms ctx))))
  (testing "a numeric string, as a client without the schema sends it"
    (let [[resp [ctx]] (backend-calls #(spawn/handle-spawn
                                        (assoc base :name "tm-str" :time_budget_ms "90000")))]
      (is (not (:isError resp)) (:text resp))
      (is (= 90000 (:time-budget-ms ctx))))))

(deftest batch-spawn-carries-the-time-budget-per-operation
  (let [[resp ctxs] (backend-calls
                     #(agent/handle-agent
                       {:command "batch-spawn"
                        :operations [(assoc base :name "tm-b1" :time_budget_ms 90000)
                                     (assoc base :name "tm-b2" :time_budget_ms 5000)
                                     (assoc base :name "tm-b3")]}))
        by-id (into {} (map (juxt :id identity)) ctxs)]
    (is (not (:isError resp)) (:text resp))
    (is (= 3 (count ctxs)))
    (is (= 90000 (:time-budget-ms (by-id "tm-b1"))))
    (is (= 5000 (:time-budget-ms (by-id "tm-b2"))))
    (is (not (contains? (by-id "tm-b3") :time-budget-ms)))))

(deftest a-spawn-without-a-time-budget-is-unchanged
  (let [[resp [plain]] (backend-calls #(spawn/handle-spawn (assoc base :name "tm-none")))
        row (slave-row "tm-none")]
    (is (not (:isError resp)) (:text resp))
    (is (some? plain))
    (is (not (contains? plain :time-budget-ms)))
    (is (some? row))
    (is (nil? (:ling/time-budget-ms row)))
    (testing "zero and negative set no deadline: the ctx equals the plain spawn's"
      (doseq [[nm v] [["tm-zero" 0] ["tm-neg" -1]]]
        (let [[resp [ctx]] (backend-calls #(spawn/handle-spawn
                                            (assoc base :name nm :time_budget_ms v)))]
          (is (not (:isError resp)) (:text resp))
          (is (= (dissoc plain :id) (dissoc ctx :id))))))))

(deftest a-malformed-time-budget-spawns-nothing
  (let [[resp ctxs] (backend-calls #(spawn/handle-spawn
                                     (assoc base :name "tm-bad" :time_budget_ms "soon")))]
    (is (:isError resp))
    (is (re-find #"time_budget_ms" (:text resp)))
    (is (empty? ctxs))))

(deftest a-later-dispatch-carries-the-recorded-time-budget
  (let [[resp _ dispatch-ctxs]
        (backend-calls
         (fn []
           (spawn/handle-spawn (assoc base :name "tm-disp" :time_budget_ms 90000))
           (dispatch/handle-dispatch
            {:agent_id (:slave/id (slave-row "tm-disp")) :prompt "again"})))]
    (is (not (:isError resp)) (:text resp))
    (is (= 90000 (:ling/time-budget-ms (slave-row "tm-disp"))))
    (is (= [90000] (mapv :time-budget-ms dispatch-ctxs)))))
