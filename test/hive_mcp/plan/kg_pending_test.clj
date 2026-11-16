(ns hive-mcp.plan.kg-pending-test
  "A KG call that times out may still have enqueued its writes: that is
   pending, never 'zero edges, not pending'."
  (:require [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-mcp.plan.kg-degraded :as kg]
            [hive-mcp.plan.tool :as tool]
            [hive-mcp.scheduler.dag-waves :as dag]
            [hive-spi.kanban.registry]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private timed-out-only
  {:edges [] :warnings ["kg-timeout:plan-task-edges:5000ms"]
   :timed-out ["plan-task-edges"] :degraded? true})

(deftest all-calls-timed-out-reports-pending
  (let [out (kg/with-durability timed-out-only (fn [_] false))]
    (is (true? (:pending? out)))
    (is (some #(= "kg-pending:calls-timed-out:plan-task-edges" %) (:warnings out)))))

(deftest a-timed-out-call-beside-landed-edges-still-reports-pending
  (let [out (kg/with-durability (assoc timed-out-only :edges ["e1"]) (fn [_] true))]
    (is (true? (:pending? out)))))

(deftest clean-edges-are-not-pending
  (let [out (kg/with-durability {:edges ["e1"] :warnings [] :timed-out [] :degraded? false}
                                (fn [_] true))]
    (is (false? (:pending? out)))))

(deftest no-edges-and-no-timeouts-is-not-pending
  (is (false? (:pending? (kg/with-durability {:edges [] :warnings [] :timed-out [] :degraded? false}
                                             (fn [_] true))))))

(deftest apply-kg-calls-names-the-timed-out-calls
  (with-redefs [kg/call-with-timeout
                (fn [label _] (if (= label "slow")
                                (r/err :weave/timeout {:timeout-ms 5})
                                (r/ok "edge-1")))]
    (let [out (kg/apply-kg-calls [["slow" (fn [])] ["fast" (fn [])]])]
      (is (= ["slow"] (:timed-out out)))
      (is (= ["edge-1"] (:edges out)))
      (is (true? (:degraded? out))))))

(deftest cards-carry-their-dependencies-as-dep-tags
  (let [created (atom [])
        steps   [{:id "s1" :title "one" :depends-on []}
                 {:id "s2" :title "two" :depends-on ["s1"]}
                 {:id "s3" :title "three" :depends-on ["s1" "s2"]}]
        waves   {"s1" 0 "s2" 1 "s3" 2}]
    (with-redefs [hive-spi.kanban.registry/create-task!
                  (fn [req] (swap! created conj req)
                    {:ok {:id (str "card-" (count @created))}})]
      (let [res ((deref #'tool/create-step-cards!) (reverse steps) waves "/tmp")]
        (is (= ["s3" "s2" "s1"] (mapv :step-id res)) "answers in step order")
        (is (= {"s1" "card-1" "s2" "card-2" "s3" "card-3"}
               (into {} (map (juxt :step-id :task-id)) res)))
        (let [tags-of (fn [title] (->> @created (some #(when (= title (:title %)) (:tags %)))))]
          (is (= #{} (set (filter #(and (string? %) (.startsWith ^String % "dep:")) (tags-of "one")))))
          (is (= #{"dep:card-1"} (set (filter #(.startsWith ^String % "dep:") (tags-of "two")))))
          (is (= #{"dep:card-1" "dep:card-2"}
                 (set (filter #(.startsWith ^String % "dep:") (tags-of "three"))))))
        (is (= #{"card-1"} (dag/card-dependencies {:tags (->> @created
                                                              (some #(when (= "two" (:title %)) (:tags %))))})))))))
