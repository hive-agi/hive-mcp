(ns hive-mcp.tools.agent.cleanup-orphaned-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.agent.lifecycle :as lifecycle]
            [hive-mcp.tools.agent.reconcile :as reconcile]))

(def ^:private reap! @#'lifecycle/reap-orphaned-elisp-lings!)

(def ^:private zombie {:slave/id "ghost" :slave/status :zombie :slave/alive? false})

(deftest cleanup-reaps-only-orphaned-elisp-lings
  (let [killed (atom [])
        probe  (reconcile/->known-agents ["live"] [] {"ghost" zombie})
        rows   [{:slave/id "ghost" :slave/status :working :slave/depth 1}
                {:slave/id "live" :slave/status :working :slave/depth 1}]
        reaped (reap! rows probe #(do (swap! killed conj %) true))]
    (is (= ["ghost"] reaped))
    (is (= ["ghost"] @killed))))

(deftest cleanup-reports-only-what-emacs-forgot
  (testing "a refused or throwing kill is not reported as reaped"
    (let [probe (reconcile/->known-agents [] [] {"a" zombie "b" zombie})
          rows  [{:slave/id "a" :slave/status :idle} {:slave/id "b" :slave/status :idle}]]
      (is (= [] (reap! rows probe (fn [id] (if (= id "a") false (throw (ex-info "boom" {}))))))))))
