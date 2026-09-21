;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.adapters.soft-test
  "The swarm host adapters bind extraction-bound host namespaces late.

   Two classpaths are exercised through the one seam the adapters expose,
   `soft/*resolve*`, and no host var is redefined:

   - the host namespace has LEFT (resolver answers nil): every soft method
     must answer exactly what the port's own Noop answers;
   - the host namespace is PRESENT (resolver hands back a recording stub):
     arguments and the return value pass through unchanged, and each
     adapter keeps its own throw/no-throw contract."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.swarm.adapters.agent-context :as agent-context]
            [hive-mcp.swarm.adapters.events :as events]
            [hive-mcp.swarm.adapters.ling-host :as ling-host]
            [hive-mcp.swarm.adapters.memory-scope :as memory-scope]
            [hive-mcp.swarm.adapters.soft :as soft]
            [hive-spi.swarm.ports.agent-context :as ctx-spi]
            [hive-spi.swarm.ports.events :as events-spi]
            [hive-spi.swarm.ports.ling-host :as ling-spi]
            [hive-spi.swarm.ports.memory-scope :as scope-spi]))

;; =============================================================================
;; Resolvers standing in for the two classpaths
;; =============================================================================

(defn- host-gone
  "A classpath every soft host namespace has left."
  [_sym]
  nil)

(defn- host-stub
  "A resolver over ANSWERS {sym answer-or-fn}. Each resolved fn records
   [sym args] into CALLS, then returns the answer (calling it when it is a
   fn). A symbol outside ANSWERS resolves to nil."
  [calls answers]
  (fn [sym]
    (when (contains? answers sym)
      (let [answer (get answers sym)]
        (fn [& args]
          (swap! calls conj [sym (vec args)])
          (if (fn? answer) (apply answer args) answer))))))

(defn- boom [& _] (throw (ex-info "host failure" {:host :down})))

;; =============================================================================
;; The helper itself
;; =============================================================================

(deftest resolve-soft-answers-nil-for-an-absent-namespace
  (is (nil? (soft/resolve-soft 'hive-mcp.no-such-namespace-anywhere/f)))
  (is (var? (soft/resolve-soft 'clojure.string/upper-case)))
  (testing "a resolver that throws is an absent namespace, not a crash"
    (binding [soft/*resolve* boom]
      (is (nil? (soft/resolve-soft 'any.ns/f))))))

(deftest host-or-guards-the-resolution-only
  (testing "absent: the fallback answers"
    (is (= :fallback
           (soft/host-or 'hive-mcp.no-such-namespace-anywhere/f (constantly :fallback) 1 2))))
  (testing "present: the host fn answers, with the args"
    (is (= "AB" (soft/host-or 'clojure.string/upper-case (constantly :fallback) "ab"))))
  (testing "a throw from the host fn is the caller's to see"
    (binding [soft/*resolve* (constantly boom)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (soft/host-or 'any.ns/f (constantly :fallback)))))))

;; =============================================================================
;; memory-scope: project identity is kernel, the three disc methods are soft
;; =============================================================================

(deftest memory-scope-answers-the-noop-once-hive-memory-has-left
  (binding [soft/*resolve* host-gone]
    (let [adapter (memory-scope/make-adapter)
          noop scope-spi/noop]
      (testing "project identity is kernel code: it answers with every soft host gone"
        (is (= "no-such-project-dir"
               (scope-spi/project-id-for-path adapter "/no-such-root/no-such-project-dir")))
        (is (= "global"
               (scope-spi/infer-scope-from-path adapter "/no-such-root/no-such-project-dir"))))
      (is (= (scope-spi/staleness-warnings noop ["/p/a.clj"])
             (scope-spi/staleness-warnings adapter ["/p/a.clj"])))
      (is (= (scope-spi/format-staleness-warnings noop [{:path "a"}])
             (scope-spi/format-staleness-warnings adapter [{:path "a"}])))
      (is (= (scope-spi/kg-first-context noop ["/p/a.clj"])
             (scope-spi/kg-first-context adapter ["/p/a.clj"]))))))

(deftest memory-scope-passes-host-answers-through
  (let [calls (atom [])
        answers {'hive-mcp.knowledge-graph.disc/staleness-warnings [{:path "a" :stale true}]
                 'hive-mcp.knowledge-graph.disc/format-staleness-warnings "1 stale"
                 'hive-mcp.knowledge-graph.disc/kg-first-context {:kg-known ["a"]}}]
    (binding [soft/*resolve* (host-stub calls answers)]
      (let [adapter (memory-scope/make-adapter)]
        (is (= [{:path "a" :stale true}] (scope-spi/staleness-warnings adapter ["a"])))
        (is (= "1 stale" (scope-spi/format-staleness-warnings adapter [:w])))
        (is (= {:kg-known ["a"]} (scope-spi/kg-first-context adapter ["a"])))))
    (is (= [['hive-mcp.knowledge-graph.disc/staleness-warnings [["a"]]]
            ['hive-mcp.knowledge-graph.disc/format-staleness-warnings [[:w]]]
            ['hive-mcp.knowledge-graph.disc/kg-first-context [["a"]]]]
           @calls))))

(deftest memory-scope-does-not-swallow-a-host-throw
  ;; The adapter never caught before it went soft; going soft must not add a
  ;; catch, or a broken disc read would start reading as "nothing is stale".
  (binding [soft/*resolve* (constantly boom)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (scope-spi/staleness-warnings (memory-scope/make-adapter) ["/p/a.clj"])))))

;; =============================================================================
;; ling-host: catchup is soft, and never throws
;; =============================================================================

(deftest ling-catchup-degrades-and-delegates
  (testing "hive-workflows has left: the Noop's nil"
    (binding [soft/*resolve* host-gone]
      (is (= (ling-spi/ling-catchup ling-spi/noop {:directory "/p"})
             (ling-spi/ling-catchup (ling-host/make-adapter) {:directory "/p"})))))
  (testing "present: opts in, context out"
    (let [calls (atom [])]
      (binding [soft/*resolve* (host-stub calls {'hive-mcp.workflows.catchup-ling/ling-catchup "## context"})]
        (is (= "## context" (ling-spi/ling-catchup (ling-host/make-adapter) {:directory "/p"}))))
      (is (= [['hive-mcp.workflows.catchup-ling/ling-catchup [{:directory "/p"}]]] @calls))))
  (testing "a host throw stays inside the adapter"
    (binding [soft/*resolve* (constantly boom)]
      (is (nil? (ling-spi/ling-catchup (ling-host/make-adapter) {}))))))

;; =============================================================================
;; agent-context: the budget guardrail is soft, and never throws
;; =============================================================================

(deftest budget-guardrail-degrades-and-delegates
  (testing "the budget hook has left: budgets unenforced, like the Noop"
    (binding [soft/*resolve* host-gone]
      (let [adapter (agent-context/make-adapter)]
        (is (= (ctx-spi/register-budget! ctx-spi/noop "ling-1" 2.0 {:model "m"})
               (ctx-spi/register-budget! adapter "ling-1" 2.0 {:model "m"})))
        (is (= (ctx-spi/deregister-budget! ctx-spi/noop "ling-1")
               (ctx-spi/deregister-budget! adapter "ling-1"))))))
  (testing "present: the entry map passes through"
    (let [calls (atom [])
          entry {:agent-id "ling-1" :max-budget-usd 2.0}]
      (binding [soft/*resolve* (host-stub calls {'hive-mcp.agent.hooks.budget/register-budget! entry
                                                 'hive-mcp.agent.hooks.budget/deregister-budget! entry})]
        (let [adapter (agent-context/make-adapter)]
          (is (= entry (ctx-spi/register-budget! adapter "ling-1" 2.0 {:model "m"})))
          (is (= entry (ctx-spi/deregister-budget! adapter "ling-1")))))
      (is (= [['hive-mcp.agent.hooks.budget/register-budget! ["ling-1" 2.0 {:model "m"}]]
              ['hive-mcp.agent.hooks.budget/deregister-budget! ["ling-1"]]]
             @calls))))
  (testing "a host throw stays inside the adapter"
    (binding [soft/*resolve* (constantly boom)]
      (is (nil? (ctx-spi/register-budget! (agent-context/make-adapter) "ling-1" 2.0 {}))))))

;; =============================================================================
;; events: telemetry and the DAG scheduler are soft
;; =============================================================================

(deftest telemetry-and-dag-answer-the-noop-once-their-hosts-have-left
  (binding [soft/*resolve* host-gone]
    (let [adapter (events/make-adapter)
          noop events-spi/noop]
      (is (= (events-spi/set-lings-active! noop 3)
             (events-spi/set-lings-active! adapter 3)))
      (is (= (events-spi/start-dag! noop "plan-1" {:max-slots 2})
             (events-spi/start-dag! adapter "plan-1" {:max-slots 2}))
          "no scheduler is :no-scheduler, never :active true")
      (is (= (events-spi/stop-dag! noop) (events-spi/stop-dag! adapter)))
      (is (= (events-spi/dag-status noop) (events-spi/dag-status adapter))))))

(deftest dag-scheduler-keeps-the-port-contract-when-present
  (testing "a started plan is reported active, with the scheduler's own keys"
    (let [calls (atom [])]
      (binding [soft/*resolve* (host-stub calls {'hive-mcp.scheduler.dag-waves/start-dag! {:started true :plan-id "plan-1"}
                                                 'hive-mcp.scheduler.dag-waves/stop-dag! {:stopped true :plan-id "plan-1"}
                                                 'hive-mcp.scheduler.dag-waves/dag-status {:active true :plan-id "plan-1"}
                                                 'hive-mcp.telemetry.prometheus/set-lings-active! :gauge-set})]
        (let [adapter (events/make-adapter)]
          (is (= {:started true :plan-id "plan-1" :active true}
                 (events-spi/start-dag! adapter "plan-1" {:max-slots 2})))
          (is (= {:stopped true :plan-id "plan-1"} (events-spi/stop-dag! adapter)))
          (is (= {:active true :plan-id "plan-1"} (events-spi/dag-status adapter)))
          (is (= :gauge-set (events-spi/set-lings-active! adapter 3)))))
      (is (= [['hive-mcp.scheduler.dag-waves/start-dag! ["plan-1" {:max-slots 2}]]
              ['hive-mcp.scheduler.dag-waves/stop-dag! []]
              ['hive-mcp.scheduler.dag-waves/dag-status []]
              ['hive-mcp.telemetry.prometheus/set-lings-active! [3]]]
             @calls))))
  (testing "the host throws; the port does not"
    (binding [soft/*resolve* (constantly boom)]
      (let [adapter (events/make-adapter)]
        (is (= {:started false :active false :plan-id "plan-1"
                :reason :already-active :error "host failure"}
               (events-spi/start-dag! adapter "plan-1" {})))
        (is (= {:stopped false :error "host failure"} (events-spi/stop-dag! adapter)))
        (is (= {:active false} (events-spi/dag-status adapter)))
        (is (nil? (events-spi/set-lings-active! adapter 3)))))))
