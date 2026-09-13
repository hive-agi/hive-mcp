(ns hive-mcp.saa.act-ports-test
  "Contract suite for the two Act port kinds of the SAA registry.

   P1 :saa/dispatch-mode registers by :mode, resolves, and deregisters by owner
      (façade + by-key) without touching the :saa/core seed.
   P2 dispatch modes are an open set: a second owner cannot take a mode that is
      already held (first-write-wins).
   P3 :saa/plan-store registers, resolves and deregisters by owner; the kernel
      seeds no plan store.
   P4 core-seed contributes :dag-wave as an ordinary :saa/core dispatch mode."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.saa.registry :as registry]
            [hive-mcp.saa.registry.dispatch-modes :as r-dispatch]
            [hive-mcp.saa.registry.plan-stores :as r-stores]
            [hive-mcp.saa.core-seed :as core-seed]
            [hive-mcp.saa.types :as types]
            [hive-mcp.saa.support :as support]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each support/with-fresh-registry)

(defn- stub-dispatch
  [tag]
  (fn [_plan _agent-id _ctx]
    {:wave-id (name tag) :result {:status :dispatched :tag tag}}))

(defn- stub-store
  [_plan _agent-id _directory]
  {:memory-id "plan-mem-1" :kanban-ids ["k1"] :kg-edges 1})

(defn- dispatch-entry
  [owner mode]
  (types/saa-registry-entry :saa/dispatch-mode
                            {:mode mode :dispatch (stub-dispatch mode) :owner owner}))

(defn- store-entry
  [owner]
  (types/saa-registry-entry :saa/plan-store {:store stub-store :owner owner}))

;; =============================================================================
;; P1 — dispatch-mode round-trip + owner-scoped deregister
;; =============================================================================

(deftest p1-dispatch-mode-register-then-resolve
  (testing "a contributed mode resolves to the contributed fn"
    (is (= [:ok] (registry/register-by-key! :addon-x :saa/dispatch-mode
                                            [(dispatch-entry :addon-x :forge-like)])))
    (let [f (registry/lookup-dispatch-mode :forge-like)]
      (is (ifn? f))
      (is (= "forge-like" (:wave-id (f {:id "p"} "agent" {})))))
    (is (= :addon-x (:owner (r-dispatch/lookup :forge-like))))
    (is (nil? (registry/lookup-dispatch-mode :never-registered)))))

(deftest p1-dispatch-mode-deregister-by-owner-keeps-seed
  (testing "façade deregister-by-owner! removes only the owner's modes"
    (registry/register-by-key! :addon-x :saa/dispatch-mode
                               [(dispatch-entry :addon-x :forge-like)
                                (dispatch-entry :addon-x :other-like)])
    (is (= #{:forge-like :other-like}
           (:dispatch-modes (registry/deregister-by-owner! :addon-x))))
    (is (nil? (registry/lookup-dispatch-mode :forge-like)))
    (is (ifn? (registry/lookup-dispatch-mode :dag-wave)) ":saa/core seed intact")))

(deftest p1-dispatch-mode-deregister-by-key
  (testing "deregister-by-key! routes :saa/dispatch-mode to its child registry"
    (registry/register-by-key! :addon-x :saa/dispatch-mode
                               [(dispatch-entry :addon-x :forge-like)])
    (is (= #{:forge-like} (registry/deregister-by-key! :addon-x :saa/dispatch-mode)))
    (is (nil? (registry/lookup-dispatch-mode :forge-like)))))

;; =============================================================================
;; P2 — open set, first-write-wins across owners
;; =============================================================================

(deftest p2-dispatch-mode-cross-owner-conflict
  (testing "an addon cannot take over a mode another owner holds"
    (is (= [:conflict] (registry/register-by-key! :addon-y :saa/dispatch-mode
                                                  [(dispatch-entry :addon-y :dag-wave)])))
    (is (= :saa/core (:owner (r-dispatch/lookup :dag-wave))))))

;; =============================================================================
;; P3 — plan-store round-trip, no default
;; =============================================================================

(deftest p3-no-default-plan-store
  (testing "the :saa/core seed contributes no plan store"
    (is (nil? (registry/lookup-plan-store)))
    (is (= [] (r-stores/all-ids)))))

(deftest p3-plan-store-register-resolve-deregister
  (testing "a contributed store resolves and deregisters by owner"
    (is (= [:ok] (registry/register-by-key! :addon-s :saa/plan-store [(store-entry :addon-s)])))
    (is (= stub-store (registry/lookup-plan-store)))
    (is (= :addon-s (:owner (r-stores/lookup :saa/default))))
    (is (= #{:saa/default} (:plan-stores (registry/deregister-by-owner! :addon-s))))
    (is (nil? (registry/lookup-plan-store)))))

(deftest p3-plan-store-deregister-by-key
  (testing "deregister-by-key! routes :saa/plan-store to its child registry"
    (registry/register-by-key! :addon-s :saa/plan-store [(store-entry :addon-s)])
    (is (= #{:saa/default} (registry/deregister-by-key! :addon-s :saa/plan-store)))
    (is (nil? (registry/lookup-plan-store)))))

;; =============================================================================
;; P4 — :dag-wave is a seeded contribution, not a kernel branch
;; =============================================================================

(deftest p4-core-seed-contributes-dag-wave
  (testing "install! seeds :dag-wave under :saa/core"
    (let [{:keys [dispatch-modes]} (core-seed/install!)]
      (is (= 1 dispatch-modes))
      (is (= :saa/core (:owner (r-dispatch/lookup :dag-wave))))
      (is (= [:dag-wave] (r-dispatch/all-ids))))))

(deftest p4-dag-wave-dispatch-threads-ctx-into-scheduler
  (testing "the seeded builder calls the injected scheduler port with cwd + run-id"
    (let [calls (atom [])
          f     (core-seed/dag-wave-dispatch-fn
                 (fn [plan-id opts]
                   (swap! calls conj [plan-id opts])
                   {:plan-id plan-id}))]
      (is (= {:wave-id "run-7" :result {:plan-id "p1" :status :dispatched}}
             (f {:id "p1"} "agent" {:run-id "run-7" :directory "/proj"})))
      (is (= [["p1" {:cwd "/proj" :run-id "run-7"}]] @calls))))

  (testing "without a run-id the wave id is the scheduler's plan id"
    (let [f (core-seed/dag-wave-dispatch-fn (fn [plan-id _] {:plan-id plan-id}))]
      (is (= "p2" (:wave-id (f {:id "p2"} "agent" {:directory "/proj"})))))))
