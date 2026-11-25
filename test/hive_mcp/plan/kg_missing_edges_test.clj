(ns hive-mcp.plan.kg-missing-edges-test
  "RUN-LEDGER-6: a degraded KG call names exactly which edges did not land,
   as {:from :to :relation :label :reason}, and a call that landed names
   none. No with-redefs: the thunks ARE the injected KG behaviour, and the
   timeout comes from the config DI env/override path being unset (5000 ms
   default is never reached; the hanging thunk is cut by an explicit
   :timeout-ms on the call)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.plan.kg-degraded :as sut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private plan->tasks
  [{:from "plan" :to "t1" :relation :depends-on}
   {:from "plan" :to "t2" :relation :depends-on}])

(def ^:private task->task
  [{:from "t2" :to "t1" :relation :depends-on}])

(defn- hang [] (try (Thread/sleep 60000) (catch InterruptedException _ ::interrupted)))

(deftest missing-edges-name-every-spec-of-a-degraded-call
  (let [result (sut/apply-kg-calls
                [{:label "plan-task-edges" :thunk hang :specs plan->tasks :timeout-ms 100}
                 {:label "task-dep-edges" :thunk (constantly ["e3"]) :specs task->task}])]
    (is (true? (:degraded? result)))
    (is (= ["e3"] (:edges result)))
    (is (= [{:from "plan" :to "t1" :relation "depends-on" :label "plan-task-edges"
             :reason "kg-timeout:plan-task-edges:100ms"}
            {:from "plan" :to "t2" :relation "depends-on" :label "plan-task-edges"
             :reason "kg-timeout:plan-task-edges:100ms"}]
           (:missing-edges result)))))

(deftest a-throwing-call-names-its-edges-with-the-error
  (let [result (sut/apply-kg-calls
                [["decision-edge" #(throw (ex-info "store closed" {}))
                  [{:from "plan" :to "dec" :relation :derived-from}]]])]
    (is (= [{:from "plan" :to "dec" :relation "derived-from" :label "decision-edge"
             :reason "kg-error:decision-edge:store closed"}]
           (:missing-edges result)))))

(deftest nothing-missing-when-every-call-lands
  (let [result (sut/apply-kg-calls
                [{:label "a" :thunk (constantly ["e1" "e2"]) :specs plan->tasks}])]
    (is (= [] (:missing-edges result)))
    (is (false? (:degraded? result)))))

(deftest a-call-without-specs-still-degrades-but-names-no-edge
  (testing "back-compat: [label thunk] pairs keep working"
    (let [result (sut/apply-kg-calls [["x" #(throw (ex-info "boom" {}))]])]
      (is (true? (:degraded? result)))
      (is (= [] (:missing-edges result))))))

(deftest pending-edges-are-listed-when-not-confirmed
  (testing "queued but unconfirmed edges are named as pending, not missing"
    (let [kg (sut/apply-kg-calls [{:label "a" :thunk (constantly ["e1" "e2"]) :specs plan->tasks}])
          r  (sut/with-durability kg (constantly false) {:timeout-ms 30})]
      (is (true? (:pending? r)))
      (is (= [{:from "plan" :to "t1" :relation "depends-on" :label "a"}
              {:from "plan" :to "t2" :relation "depends-on" :label "a"}]
             (:pending-edges r)))
      (is (= [] (:missing-edges r))))))
