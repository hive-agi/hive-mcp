(ns hive-mcp.swarm.task-dispatch-test
  "The :task-dispatch chain: the open :local-headless default, the registered
   :hive-agent-swarm dispatcher, and the fallback between them."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-dsl.result :as r]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.config.merge :as merge]
            [hive-mcp.swarm.task-dispatch :as td]
            [hive-mcp.transport.registry :as reg]
            [hive-spi.swarm.dispatch :as port]
            [hive-spi.swarm.dispatch.conformance :as conf]
            [hive-spi.swarm.dispatch.stub :as stub]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- fresh-chain [f]
  (port/unregister-dispatcher! :hive-agent-swarm)
  (reg/clear!)
  (reg/rebind! merge/default-transports)
  (try (with-redefs [ling/create-ling! (fn [id _] id)] (f))
       (finally
         (port/unregister-dispatcher! :hive-agent-swarm)
         (reg/clear!))))

(use-fixtures :each fresh-chain)

(def ^:private task {:task-id "card-1" :title "Do the thing" :prompt "do it"})

(defn- spawn-recorder [spawned]
  (fn [id opts] (swap! spawned conj {:id id :opts opts}) id))

;; The open default satisfies the same conformance suite as the stub.
(conf/defconformance local-headless
  (fn [] {:dispatcher td/local-headless}))

(deftest local-headless-serves-when-no-hive-agent-is-loaded
  (let [spawned (atom [])]
    (with-redefs [ling/create-ling! (spawn-recorder spawned)]
      (let [out (td/dispatch! task {:run-id "run-1" :cwd "/tmp/x"})
            v   (:ok out)]
        (is (r/ok? out) (pr-str out))
        (is (= :local-headless (:via v)))
        (is (= :fallback (:resolved-by v)) "the preferred dispatcher was not in use, and that is stated")
        (is (= [{:adapter :hive-agent-swarm :reason :unregistered}] (:skipped v)))
        (is (= "run-1" (get-in v [:value :run-id])))
        (is (= 1 (count @spawned)))
        (is (= "card-1" (get-in (first @spawned) [:opts :kanban-task-id])))
        (is (= :headless (get-in (first @spawned) [:opts :spawn-mode])))))))

(deftest registered-hive-agent-swarm-serves-first
  (let [spawned (atom [])
        agent   (stub/->stub #{:park :budget})]
    (port/register-dispatcher! :hive-agent-swarm agent 10)
    (with-redefs [ling/create-ling! (spawn-recorder spawned)]
      (let [out (td/dispatch! task {:run-id "run-2"})
            v   (:ok out)]
        (is (r/ok? out) (pr-str out))
        (is (= :hive-agent-swarm (:via v)))
        (is (= :primary (:resolved-by v)))
        (is (empty? (:skipped v)))
        (is (empty? @spawned) "the open path is never touched while the swarm serves")
        (is (= 1 (count (stub/calls agent))))))))

(deftest a-failing-swarm-falls_back-to-local-headless-and-says-so
  (let [spawned (atom [])
        broken  (reify port/ITaskDispatcher
                  (dispatch-task! [_ _ _]
                    (r/err :swarm/down {:error/kind :unavailable :message "swarm down"}))
                  (capabilities [_] #{}))]
    (port/register-dispatcher! :hive-agent-swarm broken 10)
    (with-redefs [ling/create-ling! (spawn-recorder spawned)]
      (let [out (td/dispatch! task {} {:sleep-fn (fn [_] nil)})
            v   (:ok out)]
        (is (r/ok? out) (pr-str out))
        (is (= :local-headless (:via v)))
        (is (= :fallback (:resolved-by v)))
        (is (= [:hive-agent-swarm] (mapv :adapter (:skipped v))))
        (is (= :failed (:reason (first (:skipped v)))))
        (is (= 1 (count @spawned)))))))

(deftest unregistering-the-swarm-returns-dispatch-to-local-headless
  (let [spawned (atom [])]
    (port/register-dispatcher! :hive-agent-swarm (stub/->stub) 10)
    (with-redefs [ling/create-ling! (spawn-recorder spawned)]
      (is (= :hive-agent-swarm (get-in (td/dispatch! task {}) [:ok :via])))
      (port/unregister-dispatcher! :hive-agent-swarm)
      (reg/probe-port! :task-dispatch)
      (is (= :local-headless (get-in (td/dispatch! task {}) [:ok :via]))))))

(deftest a-refused-spawn-is-an-error-and-keeps-the-dispatcher-healthy
  (with-redefs [ling/create-ling! (fn [_ _] (throw (ex-info "no capacity" {})))]
    (let [out (td/dispatch! task {})]
      (is (r/err? out))
      (is (= :dag/spawn-failed (:error out)))
      (is (= :local-headless (:via out))))
    (is (not= :degraded
              (get-in (reg/status) [:ports :task-dispatch :adapters :local-headless :status])))))

(deftest an-invalid-task-is-addressing-and-never-falls-back
  (let [out (td/dispatch! {:title "no id"} {})]
    (is (r/err? out))
    (is (= :addressing (:error/kind out)))))

(deftest create-ling-opts-carry-budget-role-and-run
  (let [o (td/create-ling-opts {:task-id "c" :role "architect" :prompt "p"}
                               {:run-id "r" :max-budget-usd 2.0 :cwd "/w"
                                :spawned-by "coord" :parent "par"})]
    (is (= "c" (:kanban-task-id o)))
    (is (= "r" (:run-id o)))
    (is (= 2.0 (:max-budget-usd o)))
    (is (= {:role "architect"} (:spawn/request o)))
    (is (= "coord" (:spawned-by o)))
    (is (= "par" (:parent o)))
    (is (= :headless (:spawn-mode o))))
  (testing "a blank role adds no request and zero budget adds no cap"
    (let [o (td/create-ling-opts {:task-id "c" :role "  " :prompt "p"} {:max-budget-usd 0})]
      (is (not (contains? o :spawn/request)))
      (is (not (contains? o :max-budget-usd))))))

(deftest ling-id-is-a-slug-of-the-title
  (is (= "swarm-dag-do-the-thing--1" (td/ling-id "Do the thing!" 1)))
  (is (= "swarm-dag-task-2" (td/ling-id nil 2))))
