(ns hive-mcp.saa.registry.contract-test
  "Contract facets for the six SAA child registries and the façade over them.
   Every test owns the registries through support/with-fresh-registry, which
   snapshots them first and restores the snapshot on teardown."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.protocols.saa :as psaa]
            [hive-mcp.saa.registry :as registry]
            [hive-mcp.saa.registry.dispatch-modes :as r-dispatch]
            [hive-mcp.saa.registry.phase-providers :as r-providers]
            [hive-mcp.saa.registry.plan-stores :as r-stores]
            [hive-mcp.saa.registry.planners :as r-planners]
            [hive-mcp.saa.registry.scorers :as r-scorers]
            [hive-mcp.saa.registry.tool-intents :as r-intents]
            [hive-mcp.saa.support :as support]
            [hive-mcp.saa.types :as types]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each support/with-fresh-registry)

(def ^:private Owner [:enum :owner/a :owner/b :saa/core])
(def ^:private Id
  ;; a few repeating ids reach :replaced / :conflict; a fresh uuid reaches :ok.
  ;; A repeating id is drawn three times in four: an even :or split left
  ;; :replaced (a same-owner rewrite) near 20 of 200 against a floor of 16.
  [:or {:gen/schema [:tuple [:enum :id/x :id/y :saa/default] :uuid [:enum 0 1 2 3]]
        :gen/fmap (fn [[k u arm]] (if (zero? arm) u k))}
   [:enum :id/x :id/y :saa/default] :uuid])

;;; ---------------------------------------------------------------------------
;;; Singleton registries — the conflict policy, as a property over all five
;;; ---------------------------------------------------------------------------

(defn- conflict-rel
  "Relation for a singleton register!, per the ns docstring's conflict
   policy. The registry persists across iterations, so judge against the
   record it now holds: a winning write (:ok absent, :replaced same owner)
   stores this owner and entry; a :conflict leaves another owner's record."
  [lookup]
  (fn [[owner id entry] out]
    (let [rec (lookup id)]
      (case out
        (:ok :replaced) (and (= owner (:owner rec)) (= (:v entry) (:v rec)))
        :conflict       (not= owner (:owner rec))))))

(deftrifecta-from-schema scorers-conflict-policy
  #'r-scorers/register!
  {:in [:cat Owner Id [:map [:v :int]]]
   :out types/RegisterOutcome
   :rel (conflict-rel r-scorers/lookup)
   :classify (fn [_ out] out)
   :classify-domain types/RegisterOutcome
   :num-tests 200})

(deftrifecta-from-schema providers-conflict-policy
  #'r-providers/register!
  {:in [:cat Owner Id [:map [:v :int]]]
   :out types/RegisterOutcome
   :rel (conflict-rel r-providers/lookup)
   :classify (fn [_ out] out)
   :classify-domain types/RegisterOutcome
   :num-tests 200})

(deftrifecta-from-schema planners-conflict-policy
  #'r-planners/register!
  {:in [:cat Owner Id [:map [:v :int]]]
   :out types/RegisterOutcome
   :rel (conflict-rel r-planners/lookup)
   :classify (fn [_ out] out)
   :classify-domain types/RegisterOutcome
   :num-tests 200})

(deftrifecta-from-schema plan-stores-conflict-policy
  #'r-stores/register!
  {:in [:cat Owner Id [:map [:v :int]]]
   :out types/RegisterOutcome
   :rel (conflict-rel r-stores/lookup)
   :classify (fn [_ out] out)
   :classify-domain types/RegisterOutcome
   :num-tests 200})

(deftrifecta-from-schema dispatch-modes-conflict-policy
  #'r-dispatch/register!
  {:in [:cat Owner Id [:map [:v :int]]]
   :out types/RegisterOutcome
   :rel (conflict-rel r-dispatch/lookup)
   :classify (fn [_ out] out)
   :classify-domain types/RegisterOutcome
   :num-tests 200})

(def ^:private singletons
  {:scorers        {:register! r-scorers/register! :lookup r-scorers/lookup
                    :dereg r-scorers/deregister-by-owner! :ids r-scorers/all-ids
                    :snapshot r-scorers/snapshot :restore! r-scorers/restore!
                    :reset! r-scorers/reset-for-test!}
   :providers      {:register! r-providers/register! :lookup r-providers/lookup
                    :dereg r-providers/deregister-by-owner! :ids r-providers/all-ids
                    :snapshot r-providers/snapshot :restore! r-providers/restore!
                    :reset! r-providers/reset-for-test!}
   :planners       {:register! r-planners/register! :lookup r-planners/lookup
                    :dereg r-planners/deregister-by-owner! :ids r-planners/all-ids
                    :snapshot r-planners/snapshot :restore! r-planners/restore!
                    :reset! r-planners/reset-for-test!}
   :plan-stores    {:register! r-stores/register! :lookup r-stores/lookup
                    :dereg r-stores/deregister-by-owner! :ids r-stores/all-ids
                    :snapshot r-stores/snapshot :restore! r-stores/restore!
                    :reset! r-stores/reset-for-test!}
   :dispatch-modes {:register! r-dispatch/register! :lookup r-dispatch/lookup
                    :dereg r-dispatch/deregister-by-owner! :ids r-dispatch/all-ids
                    :snapshot r-dispatch/snapshot :restore! r-dispatch/restore!
                    :reset! r-dispatch/reset-for-test!}})

(defn- singleton-lifecycle
  "Drive one singleton registry through its whole surface from empty."
  [{:keys [register! lookup dereg ids snapshot restore! reset!]}]
  (reset!)
  (let [r1 (register! :owner/a :id/x {:v 1})
        r2 (register! :owner/a :id/x {:v 2})
        r3 (register! :owner/b :id/x {:v 3})
        r4 (register! :owner/b :id/y {:v 4})
        snap (snapshot)
        held (mapv (fn [id] (some-> (lookup id) (select-keys [:owner :v]))) [:id/x :id/y :id/z])
        removed (dereg :owner/a)
        after-dereg (ids)
        _ (restore! snap)
        restored (ids)
        _ (restore! nil)]
    {:outcomes [r1 r2 r3 r4] :held held :removed removed :after-dereg after-dereg
     :restored restored :after-nil-restore (ids)
     :snapshot-shape (m/validate types/RegistrySnapshot snap)}))

(golden/deftest-golden singleton-registries-lifecycle-golden
  "test/golden/hive-mcp/saa/registry/singleton-lifecycle.edn"
  (into (sorted-map) (map (fn [[k reg]] [k (singleton-lifecycle reg)])) singletons))

(deftest lookup-misses-answer-nil
  (doseq [[k {:keys [lookup]}] singletons]
    (is (nil? (lookup :id/never)) (str k))))

;;; ---------------------------------------------------------------------------
;;; tool-intents — accumulative per-owner slices
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema tool-intent-slice-is-the-set-registered
  #'r-intents/register!
  {:in [:cat Owner [:enum :read :search :custom] [:map [:tools [:vector [:enum "a" "b" "c"]]]]]
   :out [:= :ok]
   :rel (fn [[owner intent {:keys [tools]}] _]
          (and (= (set tools) (r-intents/lookup-owner-slice owner intent))
               (every? (set (:tools (r-intents/lookup intent))) tools)
               (contains? (set (r-intents/all-intents)) intent)))
   :num-tests 100})

(golden/deftest-golden tool-intents-lifecycle-golden
  "test/golden/hive-mcp/saa/registry/tool-intents-lifecycle.edn"
  (let [_ (r-intents/reset-for-test!)
        ok (r-intents/register! :owner/a :read {:tools ["x" "y"]})
        _ (r-intents/register! :owner/b :read {:tools #{"z"}})
        _ (r-intents/register! :owner/b :custom {})
        snap (r-intents/snapshot)
        read-before (r-intents/lookup :read)
        removed (r-intents/deregister-by-owner! :owner/b)
        _ (r-intents/restore! snap)
        restored (r-intents/all-intents)]
    {:ok ok :read-before read-before :custom-slice (r-intents/lookup-owner-slice :owner/b :custom)
     :removed removed :restored restored
     :snapshot-shape (m/validate r-intents/ToolIntentSnapshot snap)
     :reset (r-intents/reset-for-test!)}))

;;; ---------------------------------------------------------------------------
;;; façade
;;; ---------------------------------------------------------------------------

(golden/deftest-golden facade-routing-golden
  "test/golden/hive-mcp/saa/registry/facade-routing.edn"
  (let [entries [(types/saa-registry-entry :saa/tool-intent {:intent :read :tools ["fx"] :owner :owner/a})
                 (types/saa-registry-entry :saa/dispatch-mode {:mode :m/x :dispatch (fn [_ _ _] {}) :owner :owner/a})
                 (types/saa-registry-entry :saa/plan-store {:store (fn [_ _ _] {}) :owner :owner/a})
                 {:not "an entry"}]
        out (registry/register-by-key! :owner/a :saa/tool-intent entries)
        single (registry/register-by-key! :owner/a :saa/scorer
                                          (types/saa-registry-entry :saa/scorer {:scorer :not-a-scorer :owner :owner/a}))
        skipped (registry/register-by-key! :owner/a :saa/scorer 42)]
    {:out out :single single :skipped skipped
     :tools (registry/resolve-tools :owner/a :read)
     :core-tools (registry/resolve-tools :nobody :read)
     :store? (fn? (registry/lookup-plan-store))
     :mode? (fn? (registry/lookup-dispatch-mode :m/x))
     :unknown-mode (registry/lookup-dispatch-mode :m/none)
     :dereg-key-unknown (registry/deregister-by-key! :owner/a :saa/bogus)
     :dereg-key-store (registry/deregister-by-key! :owner/a :saa/plan-store)
     :dereg-owner (registry/deregister-by-owner! :owner/a)
     :snapshot-keys (sort (keys (registry/snapshot)))}))

(deftest resolvers-satisfy-their-ports-after-a-full-reset
  (registry/reset-for-test!)
  (is (satisfies? psaa/IPhaseProvider (registry/lookup-phase-provider-or-default)))
  (is (satisfies? psaa/IObservationScorer (registry/lookup-scorer-or-default :id/x)))
  (is (satisfies? psaa/IPlanSynthesizer (registry/lookup-planner-or-default)))
  (is (nil? (registry/lookup-plan-store :id/x)))
  (is (m/validate registry/RegistrySnapshot (registry/snapshot)))
  (testing "restore! of a partial snapshot empties the missing children"
    (registry/restore! {})
    (is (= [] (r-scorers/all-ids)))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate — src/hive_mcp/saa/registry
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage saa-registry-contracts
  {:paths ["src/hive_mcp/saa/registry"]})

(deftest saa-registry-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/saa/registry"]})]
    (is (= 43 (count (:universe report))) "every public defn of the tree")
    (is (= 43 (count (:covered report))))))

(deftest every-saa-registry-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.saa.registry.dispatch-modes hive-mcp.saa.registry.phase-providers
                   hive-mcp.saa.registry.plan-stores hive-mcp.saa.registry.planners
                   hive-mcp.saa.registry.scorers hive-mcp.saa.registry.tool-intents]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
