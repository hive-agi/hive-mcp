(ns hive-mcp.server.lifecycle-test
  "Facets for hive-mcp.server.lifecycle (cov-facets f07).

   run-shutdown-sequence! is driven over stub IShutdownHook records registered
   in the system registry, isolated with the registry's own test API
   (capture-all / reset-all! / restore-all!): one hook succeeds, one throws,
   one overruns its own IShutdownBudget. trigger-session-end! and
   get-hooks-registry run over a real hooks registry with a recording handler.

   Not covered here: register-shutdown-hook! and init-hooks! install a JVM
   shutdown hook through Runtime/addShutdownHook (no port), and init-hooks!
   also wires swarm.sync and the storage heal sweep. Only their idempotent
   already-initialised arm is pinned."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.hooks.core :as hooks]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.server.lifecycle :as sl]
            [hive-mcp.system.registry :as reg]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord StubHook [hname priority behaviour seen]
  lifecycle/IShutdownHook
  (shutdown-priority [_] priority)
  (shutdown-name [_] hname)
  (shutdown! [_ ctx]
    (swap! seen conj [hname (:timeout-ms ctx) (:reason ctx)])
    (case behaviour
      :ok    nil
      :throw (throw (ex-info "hook failed" {:hook hname}))
      :hang  (Thread/sleep 2000))))

(defrecord BudgetedHook [hname priority budget-ms seen]
  lifecycle/IShutdownHook
  (shutdown-priority [_] priority)
  (shutdown-name [_] hname)
  (shutdown! [_ ctx]
    (swap! seen conj [hname (:timeout-ms ctx) (:reason ctx)])
    (Thread/sleep 2000))
  lifecycle/IShutdownBudget
  (shutdown-timeout-ms [_] budget-ms))

(defn- with-isolated-registry [f]
  (let [saved (reg/capture-all)]
    (reg/reset-all!)
    (try (f) (finally (reg/reset-all!) (reg/restore-all! saved)))))

(defn- project-report
  "Errors projected to [name kind] so the golden holds no Throwable."
  [{:keys [ran errors]}]
  {:ran ran
   :errors (vec (sort (map (fn [{:keys [name error]}]
                             [name (if (= :timeout error) :timeout (ex-message error))])
                           errors)))})

(deftest-golden shutdown-sequence-golden
  "test/golden/hive-mcp/f07-lifecycle-shutdown-sequence.edn"
  (with-isolated-registry
    (fn []
      (let [seen (atom [])]
        (reg/register-shutdown! (->StubHook "a/ok" 10 :ok seen))
        (reg/register-shutdown! (->StubHook "b/throws" 20 :throw seen))
        (reg/register-shutdown! (->BudgetedHook "c/budgeted" 30 50 seen))
        (let [report (sl/run-shutdown-sequence! {:reason :test :timeout-ms 500})]
          {:report (project-report report)
           :seen   (vec (sort @seen))
           :empty  (do (reg/reset-all!)
                       (sl/run-shutdown-sequence! {:reason :empty}))})))))

(deftest-golden session-end-golden
  "test/golden/hive-mcp/f07-lifecycle-session-end.edn"
  (let [registry (hooks/create-registry)
        got      (atom [])
        _        (hooks/register-hook registry :session-end
                                      (fn [ctx] (swap! got conj (:reason ctx)) :wrapped))
        reg-atom (atom registry)]
    {:with-registry    (sl/trigger-session-end! reg-atom :test-end)
     :handler-saw      @got
     :without-registry (sl/trigger-session-end! (atom nil) :test-end)
     :get-registry     (identical? registry (sl/get-hooks-registry reg-atom))
     :get-empty        (sl/get-hooks-registry (atom nil))
     :init-when-set    (sl/init-hooks! reg-atom (atom true) (atom "hive"))
     :register-when-set (sl/register-shutdown-hook! (atom true) (atom nil) reg-atom)}))

(deftest project-config-shape
  (let [cfg (sl/read-project-config)]
    (is (or (nil? cfg) (contains? cfg :hot-reload)))))
