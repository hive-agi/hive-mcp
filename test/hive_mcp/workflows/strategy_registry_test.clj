(ns hive-mcp.workflows.strategy-registry-test
  "The method strategy registry is owner-scoped: an owner registers, replaces
   and removes only its own methods, and the defaulting resolver never answers
   nil. Every test uses keys of its own and removes them, so the shared registry
   (and its boot-seeded :wf/default) is left as it was found."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-dsl.result :as result]
            [hive-mcp.workflows.strategy :as strategy]
            [hive-mcp.workflows.strategy-registry :as registry]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

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

;; -----------------------------------------------------------------------------
;; Facets. The registry has no restore!, so these run inside a fixture that
;; snapshots its private state atom and puts the value back afterwards.
;; -----------------------------------------------------------------------------

(defn- with-restored-registry [f]
  (let [saved (:data (registry/snapshot))]
    (try (f) (finally (reset! @#'registry/state saved)))))

;; Every test of this namespace now runs inside it; the deftests above also
;; clean up their own keys, so for them it is a no-op.
(use-fixtures :each with-restored-registry)

(def ^:private FacetOwner [:enum :facet/a :facet/b :wf/core])

;; Repeating methods (one of them the seeded :wf/default) reach :replaced and
;; :conflict; a uuid reaches :ok. Mirrors the SAA registry suites.
(def ^:private FacetMethod
  [:or {:gen/schema [:tuple [:enum :facet/m1 :facet/m2 :wf/default] :uuid [:enum 0 1 2 3]]
        :gen/fmap (fn [[k u arm]] (if (zero? arm) u k))}
   [:enum :facet/m1 :facet/m2 :wf/default] :uuid])

(deftrifecta-from-schema register-follows-the-conflict-policy
  #'registry/register!
  {:in [:cat FacetOwner FacetMethod [:map [:strategy registry/Strategy]]]
   :out registry/Outcome
   :rel (fn [[owner method _] out]
          (let [data (:data (registry/snapshot))
                owned (get-in data [:by-owner owner] #{})]
            (and (= (not= :conflict out) (contains? owned method))
                 (= (not= :conflict out) (= owner (:owner (registry/lookup method)))))))
   :classify (fn [_ out] out)
   :classify-domain registry/Outcome
   :num-tests 200})

(deftrifecta-from-schema register-by-key-routes-entries
  #'registry/register-by-key!
  {:in [:cat FacetOwner [:= :wf/strategy]
        [:or {:gen/schema [:tuple [:enum :facet/m1 :facet/m2] [:enum :entry :entries :junk]]
              :gen/fmap (fn [[method shape]]
                          (let [e (strategy/workflow-strategy-entry
                                   :wf/strategy {:method method
                                                 :strategy (strategy/->NoopDispatchStrategy)
                                                 :owner :facet/a})]
                            (case shape :entry e :entries [e {:method method}] :junk 42)))}
         :map [:vector :any] :int]]
   :out [:vector [:enum :ok :replaced :conflict :ignored]]
   :rel (fn [[_ _ entries] out]
          (= (count out) (cond (sequential? entries) (count entries) (map? entries) 1 :else 0)))
   :classify (fn [[_ _ entries] _] (cond (sequential? entries) :many (map? entries) :one :else :none))
   :classify-domain #{:many :one :none}})

(golden/deftest-golden strategy-registry-lifecycle-golden
  "test/golden/hive-mcp/workflows/strategy-registry-lifecycle.edn"
  (let [noop (strategy/->NoopDispatchStrategy)
        seeded (registry/all-methods)
        _ (registry/reset-for-test!)
        empty-default (strategy/dispatch-strategy? (registry/lookup-strategy-or-default))
        outcomes [(registry/register! :facet/a :facet/m1 {:strategy noop})
                  (registry/register! :facet/a :facet/m1 {:strategy noop})
                  (registry/register! :facet/b :facet/m1 {:strategy noop})
                  (registry/register! :facet/b :facet/m2 {:strategy noop})]
        snap (registry/snapshot)
        by-owner (into (sorted-map) (update-vals (get-in snap [:data :by-owner]) (comp vec sort)))
        removed (vec (sort (registry/deregister-by-owner! :facet/a)))]
    {:seeded seeded :empty-default empty-default :outcomes outcomes :by-owner by-owner
     :removed removed :after (registry/all-methods)
     :owner-of-m2 (:owner (registry/lookup :facet/m2))
     :reset (registry/reset-for-test!)}))
