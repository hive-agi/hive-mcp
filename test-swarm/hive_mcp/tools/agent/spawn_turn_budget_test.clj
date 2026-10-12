(ns hive-mcp.tools.agent.spawn-turn-budget-test
  "turn_budget given to `agent spawn` / `agent batch-spawn` reaches the
   headless backend's ctx as the kebab lease spec hive-agent's
   build-spawn-config reads, through a stub backend (no concrete bb-ling)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
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
  {:type "ling" :cwd "/tmp/spawn-turn-budget" :spawn_mode "headless"
   :model "venice:test-model" :task "x"})

(def ^:private lease-json {"judge" "hivemind" "initial" 60 "hard_cap" 400})
(def ^:private lease {:judge :hivemind :initial 60 :hard-cap 400})

(defn- spawn-ctxs
  "Run F against a stub headless backend; the ctx of every spawn! call."
  [f]
  (let [b (hb/->backend :test-headless)]
    (hb/with-backend :test-headless b
      (let [resp (f)]
        [resp (mapv first (hb/calls-of b :spawn!))]))))

(deftest normalize-reads-the-mcp-shapes
  (testing "the port resolves hive-agent's normalizer (hive-agent is on this classpath)"
    (is (fn? (spawn/turn-budget-normalizer))))
  (let [normalize (spawn/turn-budget-normalizer)]
    (is (= lease (spawn/turn-budget-opt normalize lease-json)))
    (is (= lease (spawn/turn-budget-opt normalize {:judge "hivemind" :initial "60" :hard-cap 400})))
    (is (= lease (spawn/turn-budget-opt normalize "{\"judge\":\"hivemind\",\"initial\":60,\"hard_cap\":400}")))
    (is (= {:max-extensions 8 :wrap-up? true}
           (spawn/turn-budget-opt normalize {"max_extensions" 8 "wrap_up" true})))
    (is (nil? (spawn/turn-budget-opt normalize nil)))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/turn-budget-opt normalize 5)))
    (is (thrown? clojure.lang.ExceptionInfo (spawn/turn-budget-opt normalize {"initial" "lots"})))))

(deftest spawn-carries-the-lease-to-the-backend-ctx
  (let [[resp [ctx]] (spawn-ctxs #(spawn/handle-spawn
                                   (assoc base :name "tb-one" :turn_budget lease-json)))]
    (is (not (:isError resp)) (:text resp))
    (is (= lease (:turn-budget ctx)))))

(deftest batch-spawn-carries-the-lease-per-operation
  (let [[resp ctxs] (spawn-ctxs
                     #(agent/handle-agent
                       {:command "batch-spawn"
                        :operations [(assoc base :name "tb-b1" :turn_budget lease-json)
                                     (assoc base :name "tb-b2"
                                            :turn_budget {"judge" "policy" "initial" 70})
                                     (assoc base :name "tb-b3")]}))
        by-id (into {} (map (juxt :id identity)) ctxs)]
    (is (not (:isError resp)) (:text resp))
    (is (= 3 (count ctxs)))
    (is (= lease (:turn-budget (by-id "tb-b1"))))
    (is (= {:judge :policy :initial 70} (:turn-budget (by-id "tb-b2"))))
    (is (not (contains? (by-id "tb-b3") :turn-budget)))))

(deftest no-turn-budget-leaves-the-key-out
  (testing "a plain spawn's ctx has no :turn-budget, so the backend default holds"
    (let [[resp [ctx]] (spawn-ctxs #(spawn/handle-spawn (assoc base :name "tb-none")))]
      (is (not (:isError resp)) (:text resp))
      (is (some? ctx))
      (is (not (contains? ctx :turn-budget))))))

(deftest a-malformed-turn-budget-spawns-nothing
  (let [[resp ctxs] (spawn-ctxs #(spawn/handle-spawn
                                  (assoc base :name "tb-bad" :turn_budget [1 2])))]
    (is (:isError resp))
    (is (re-find #"turn_budget" (:text resp)))
    (is (empty? ctxs))))
