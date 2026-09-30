(ns hive-mcp.scheduler.kg-gc-test
  "The KG GC lane must not read an invisible universe as a clean graph."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.scheduler.kg-gc :as lane]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (lane/reset-state!) (try (f) (finally (lane/reset-state!)))))

(def ^:private clean
  {:universe {:sources 10 :orphans 2 :registered 8 :projection-edges 40}
   :blind [] :scanned 10 :orphaned 2 :pruned 1 :demoted 0 :preserved 7
   :errors 0 :partial [] :next-cursor "synth-9"
   :details [{:outcome :orphaned :effect-count 5 :complete? true}
             {:outcome :pruned :effect-count 3 :complete? true}
             {:outcome :orphaned :effect-count 0 :complete? false}]})

(deftest verdict-clean-test
  (let [v (lane/verdict clean)]
    (is (:ok? v))
    (is (false? (:vacuous? v)))
    (is (= 2 (get-in v [:collected :total])) "the partial one is not collected")
    (is (= 2 (get-in v [:selected :orphaned])))
    (is (= 8 (get-in v [:collected :edges-removed])))
    (is (= "synth-9" (get-in v [:coverage :cursor])))))

(deftest verdict-empty-universe-fails-test
  (testing "0 collected over an empty universe is not ok"
    (let [v (lane/verdict {:universe {:sources 0 :orphans 0 :registered 0 :projection-edges 0}
                           :blind [] :scanned 0 :orphaned 0 :pruned 0 :demoted 0 :errors 0})]
      (is (false? (:ok? v)))
      (is (:vacuous? v))
      (is (some #(= :universe (:class %)) (:blind v))))))

(deftest verdict-blind-class-fails-test
  (let [v (lane/verdict (assoc clean :blind [{:class :orphaned-record :reason "x"}]))]
    (is (false? (:ok? v)))))

(deftest lane-interval-gate-test
  (let [calls (atom 0)
        f     (fn [_] (swap! calls inc) clean)
        opts  {:cleanup-fn f :heap-fn (constantly 0.1) :enabled true :interval-minutes 60}]
    (is (:ok? (lane/run-lane! opts)))
    (is (:skipped (lane/run-lane! opts)) "second tick inside the interval skips")
    (is (:ok? (lane/run-lane! (assoc opts :force? true))))
    (is (= 2 @calls))))

(deftest lane-passes-cursor-test
  (let [seen (atom [])
        f    (fn [o] (swap! seen conj (:after o)) clean)
        opts {:cleanup-fn f :heap-fn (constantly 0.1) :enabled true :force? true}]
    (lane/run-lane! opts)
    (lane/run-lane! opts)
    (is (= [nil "synth-9"] @seen))))

(deftest lane-refuses-under-heap-pressure-test
  (let [called (atom false)
        r (lane/run-lane! {:cleanup-fn (fn [_] (reset! called true) clean)
                           :heap-fn (constantly 0.95) :enabled true :force? true})]
    (is (= "heap-pressure" (:reason r)))
    (is (false? @called))))

(deftest continue-guard-test
  (let [c (lane/make-continue? {:max-heap-ratio 0.8 :deadline-ms 100} 0
                               {:heap-fn (constantly 0.5) :now-fn (constantly 50)})]
    (is (nil? (c))))
  (is (= :heap-pressure ((lane/make-continue? {:max-heap-ratio 0.8 :deadline-ms 100} 0
                                              {:heap-fn (constantly 0.9) :now-fn (constantly 0)}))))
  (is (= :deadline ((lane/make-continue? {:max-heap-ratio 0.8 :deadline-ms 100} 0
                                         {:heap-fn (constantly 0.1) :now-fn (constantly 500)})))))

(deftest lane-releases-on-throw-test
  (is (thrown? Exception (lane/run-lane! {:cleanup-fn (fn [_] (throw (Exception. "boom")))
                                          :heap-fn (constantly 0.1) :enabled true :force? true})))
  (is (false? (:running? (lane/status)))))
