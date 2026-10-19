(ns hive-mcp.agent.ling.dispatch-status-restore-test
  "Ling.dispatch! writes :slave/status :working before it dispatches. A
   failed dispatch must not leave that :working behind: the status the ling
   had before comes back, unless another writer changed it meanwhile.

   Backends are injected through the headless registry (a reified
   IHeadlessBackend), the way strategy_test does; no with-redefs."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.ling :as ling]
            [hive-mcp.agent.ling.spawn :as spawn]
            [hive-mcp.agent.ling.headless-registry :as headless-reg]
            [hive-mcp.addons.headless :as headless-proto]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.swarm.datascript.lings :as ds-lings]
            [hive-mcp.swarm.datascript.queries :as ds-queries]
            [hive-test.isolation :as iso]
            hive-mcp.isolation-methods))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (iso/with-isolations :swarm-ds :terminal-registry :headless-registry))

(defn- backend
  "A headless backend whose dispatch is DISPATCH-FN (ctx opts) -> answer."
  [dispatch-fn]
  (reify
    headless-proto/IHeadlessBackend
    (headless-id [_] :claude-sdk)
    (headless-spawn! [_ ctx _opts] (:id ctx))
    (headless-dispatch! [_ ctx opts] (dispatch-fn ctx opts))
    (headless-status [_ _ctx ds-data] ds-data)
    (headless-kill! [_ ctx] {:killed? true :id (:id ctx)})
    (headless-interrupt! [_ _ctx] {:success? false})

    headless-proto/IHeadlessCapabilities
    (declared-capabilities [_] #{:cap/streaming})))

(defn- ling-with-status
  "Register ling ID with STATUS on a backend dispatching through DISPATCH-FN."
  [id status dispatch-fn]
  (headless-reg/register-headless! :claude-sdk (backend dispatch-fn))
  (ds-lings/add-slave! id {:status status :cwd "/tmp"})
  (ds-lings/update-slave! id {:ling/spawn-mode :claude-sdk})
  (ling/->ling id {:cwd "/tmp" :spawn-mode :headless}))

(defn- status-of [id] (:slave/status (ds-queries/get-slave id)))

(defn- dispatch-outcome
  "Dispatch a task to L: :thrown when it threw, else the task id."
  [l]
  (try (proto/dispatch! l {:task "do the thing"})
       (catch clojure.lang.ExceptionInfo _ :thrown)))

(deftest refused-dispatch-restores-prior-status
  (testing "a backend answering {:dispatched false} leaves the ling as it was"
    (let [l (ling-with-status "refused-ling" :idle
                              (fn [_ _] {:dispatched false :reason :busy}))]
      (is (= :thrown (dispatch-outcome l)))
      (is (= :idle (status-of "refused-ling")))))
  (testing "a backend answering false leaves a :blocked ling :blocked"
    (let [l (ling-with-status "refused-false" :blocked (fn [_ _] false))]
      (is (= :thrown (dispatch-outcome l)))
      (is (= :blocked (status-of "refused-false"))))))

(deftest thrown-dispatch-restores-prior-status
  (testing "a backend that throws leaves the ling with its prior status"
    (let [l (ling-with-status "thrown-ling" :idle
                              (fn [_ _] (throw (ex-info "backend down" {}))))]
      (is (= :thrown (dispatch-outcome l)))
      (is (= :idle (status-of "thrown-ling")))))
  (testing "a non-ExceptionInfo Throwable is handled the same way"
    (let [l (ling-with-status "assert-ling" :idle
                              (fn [_ _] (throw (AssertionError. "pre failed"))))]
      (is (= :thrown (dispatch-outcome l)))
      (is (= :idle (status-of "assert-ling"))))))

(deftest backend-named-status-wins
  (testing "a failure whose ex-data names :slave-status sets that status"
    (let [l (ling-with-status "broken-ling" :idle
                              (fn [_ _] (throw (ex-info "session gone"
                                                        {:slave-status :error}))))]
      (is (= :thrown (dispatch-outcome l)))
      (is (= :error (status-of "broken-ling"))))))

(deftest concurrent-status-change-is-not-clobbered
  (testing "a status written while the dispatch was failing is kept"
    (let [l (ling-with-status "raced-ling" :idle
                              (fn [ctx _]
                                ;; another writer moves the ling on first
                                (ds-lings/update-slave! (:id ctx) {:slave/status :terminated})
                                (throw (ex-info "backend down" {}))))]
      (is (= :thrown (dispatch-outcome l)))
      (is (= :terminated (status-of "raced-ling"))))))

(deftest successful-dispatch-leaves-working
  (testing "a dispatch that succeeds leaves :working, unchanged"
    (doseq [[id answer] [["ok-true" true]
                         ["ok-map" {:dispatched true :task-id "t"}]
                         ["ok-nil" nil]]]
      (let [l (ling-with-status id :idle (fn [_ _] answer))]
        (is (string? (dispatch-outcome l)) id)
        (is (= :working (status-of id)) id)))))

(deftest dispatch-refusal-classifies-answers
  (is (nil? (spawn/dispatch-refusal true)))
  (is (nil? (spawn/dispatch-refusal nil)))
  (is (nil? (spawn/dispatch-refusal {:dispatched true})))
  (is (some? (spawn/dispatch-refusal false)))
  (is (some? (spawn/dispatch-refusal {:dispatched false})))
  (is (some? (spawn/dispatch-refusal {:error :backend/down}))))
