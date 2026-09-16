(ns hive-mcp.session.wrap-adopt-test
  "The ::adopt state: a coordinator absorbs the wraps it owns, a ling absorbs
   nothing, and a failure degrades instead of taking the wrap down."
  (:require [clojure.test :refer [deftest testing is]]
            [hive-mcp.session.identity :as sid]
            [hive-mcp.workflows.wrap-session :as wrap]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ref-coord
  (sid/session-ref {:id "s-a" :kind :coordinator :project-id "hive" :agent-id "coord-a"}))

(def ref-ling
  (sid/session-ref {:id "s-a1" :kind :ling :parent-id "s-a" :depth 1
                    :project-id "hive" :agent-id "ling-a1"}))

(deftest start-resolves-a-session-ref-test
  (testing "the injected session-ref-fn is what gives the wrap its identity"
    (let [out (wrap/handle-start {:session-ref-fn (constantly ref-coord)
                                  :scope-fn (constantly "hive")
                                  :directory "/tmp" :agent-id "coord-a"}
                                 {})]
      (is (= ref-coord (:session-ref out)))
      (is (= "hive" (:project-id out)))))
  (testing "with no session-ref-fn the wrap runs unscoped, as it did before"
    (let [out (wrap/handle-start {:scope-fn (constantly "hive")
                                  :directory "/tmp" :agent-id "coord-a"}
                                 {})]
      (is (nil? (:session-ref out))))))

(deftest coordinator-adopts-test
  (let [seen (atom nil)
        out  (wrap/handle-adopt {:adopt-fn (fn [ref] (reset! seen ref) [{:wrap-queue/id "w-a1"}])}
                                {:session-ref ref-coord})]
    (testing "adopt-fn is called with the coordinator's own ref"
      (is (= ref-coord @seen)))
    (is (= ["w-a1"] (mapv :wrap-queue/id (:adopted out))))))

(deftest ling-adopts-nothing-test
  (testing "a ling's wrap permeates UP; it never absorbs siblings"
    (let [called (atom false)
          out (wrap/handle-adopt {:adopt-fn (fn [_] (reset! called true) [{:wrap-queue/id "w-x"}])}
                                 {:session-ref ref-ling})]
      (is (= [] (:adopted out)))
      (is (false? @called)))))

(deftest unscoped-wrap-adopts-nothing-test
  (testing "no session-ref means no ownership claim, so nothing is absorbed"
    (let [out (wrap/handle-adopt {:adopt-fn (fn [_] [{:wrap-queue/id "w-x"}])}
                                 {:session-ref nil})]
      (is (= [] (:adopted out))))))

(deftest adopt-failure-degrades-test
  (testing "adoption is additive context: a throw leaves the wrap running"
    (let [out (wrap/handle-adopt {:adopt-fn (fn [_] (throw (ex-info "boom" {})))}
                                 {:session-ref ref-coord})]
      (is (= [] (:adopted out)))
      (is (true? (:adopt-degraded out)))
      (is (= "boom" (:adopt-error out)))
      (is (nil? (:error out))))))

(deftest adopt-sits-between-gather-and-crystallize-test
  (let [spec (:fsm wrap/wrap-session-spec)]
    (is (contains? spec :hive-mcp.workflows.wrap-session/adopt))
    (testing "gather now dispatches into adopt, and adopt into crystallize"
      (is (= :hive-mcp.workflows.wrap-session/adopt
             (ffirst (:dispatches (get spec :hive-mcp.workflows.wrap-session/gather)))))
      (is (= :hive-mcp.workflows.wrap-session/crystallize
             (ffirst (:dispatches (get spec :hive-mcp.workflows.wrap-session/adopt))))))
    (testing "the EDN handler-map carries it too, or the EDN spec would not compile"
      ;; The map holds the VAR, not the value it had at load time — that is what
      ;; lets a reload rewire this handler. Asserting equality against the fn
      ;; would pin the frozen spelling this table was converted away from.
      (is (identical? #'wrap/handle-adopt (:adopt wrap/handler-map))))))
