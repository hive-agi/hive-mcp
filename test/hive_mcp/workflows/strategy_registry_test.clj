(ns hive-mcp.workflows.strategy-registry-test
  "The method strategy registry is owner-scoped: an owner registers, replaces
   and removes only its own methods, and the defaulting resolver never answers
   nil. Every test uses keys of its own and removes them, so the shared registry
   (and its boot-seeded :wf/default) is left as it was found."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as result]
            [hive-mcp.workflows.strategy :as strategy]
            [hive-mcp.workflows.strategy-registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private owner-a :strategy-registry-test/owner-a)
(def ^:private owner-b :strategy-registry-test/owner-b)
(def ^:private m1 :strategy-registry-test-one)
(def ^:private m2 :strategy-registry-test-two)

(defn- strategy-answering [v]
  (reify strategy/IDispatchStrategy
    (dispatch [_ _ _] (result/ok v))))

(defn- others [] (vec (remove #{m1 m2} (registry/all-methods))))

(defn- with-own-keys [f]
  (try (f)
       (finally
         (registry/deregister-by-owner! owner-a)
         (registry/deregister-by-owner! owner-b))))

(deftest registration-is-owner-scoped
  (with-own-keys
    (fn []
      (let [s1 (strategy-answering 1)
            s2 (strategy-answering 2)
            before (others)]
        (testing "an unregistered method has no entry"
          (is (nil? (registry/lookup m1))))
        (testing "a first registration is :ok and carries its owner and strategy"
          (is (= :ok (registry/register! owner-a m1 {:strategy s1})))
          (is (= owner-a (:owner (registry/lookup m1))))
          (is (identical? s1 (:strategy (registry/lookup m1))))
          (is (some #{m1} (registry/all-methods))))
        (testing "the same owner replaces its own entry"
          (is (= :replaced (registry/register! owner-a m1 {:strategy s2})))
          (is (identical? s2 (:strategy (registry/lookup m1)))))
        (testing "another owner cannot take a method that is held"
          (is (= :conflict (registry/register! owner-b m1 {:strategy s1})))
          (is (= owner-a (:owner (registry/lookup m1))))
          (is (identical? s2 (:strategy (registry/lookup m1)))))
        (testing "removal is scoped to the owner"
          (is (= :ok (registry/register! owner-b m2 {:strategy s1})))
          (is (= #{m1} (registry/deregister-by-owner! owner-a)))
          (is (nil? (registry/lookup m1)))
          (is (= owner-b (:owner (registry/lookup m2))))
          (is (not-any? #{m1} (registry/all-methods))))
        (testing "removing an owner that holds nothing removes nothing"
          (is (= #{} (registry/deregister-by-owner! owner-a))))
        (testing "entries of other owners are left as they were"
          (is (= before (others))))))))

(deftest the-defaulting-resolver-never-answers-nil
  (with-own-keys
    (fn []
      (let [s (strategy-answering :mine)]
        (testing "a registered method resolves to its own strategy"
          (registry/register! owner-a m1 {:strategy s})
          (is (identical? s (registry/lookup-strategy-or-default m1))))
        (testing "an unregistered method resolves to a strategy, not nil"
          (registry/deregister-by-owner! owner-a)
          (is (strategy/dispatch-strategy? (registry/lookup-strategy-or-default m1))))
        (testing "while `lookup` still says nobody registered it"
          (is (nil? (registry/lookup m1))))))))

(deftest hook-entries-register-under-the-addon-that-owns-them
  (with-own-keys
    (fn []
      (let [s     (strategy-answering :hooked)
            entry (strategy/workflow-strategy-entry
                   :wf/strategy {:method m1 :strategy s :owner owner-a})]
        (is (= [:ok] (registry/register-by-key! owner-a :wf/strategy entry)))
        (is (= owner-a (:owner (registry/lookup m1))))
        (is (identical? s (:strategy (registry/lookup m1))))
        (testing "a value that is not a strategy entry is ignored, not registered"
          (is (= [:ignored] (registry/register-by-key! owner-a :wf/strategy [{:method m2}])))
          (is (nil? (registry/lookup m2))))))))
