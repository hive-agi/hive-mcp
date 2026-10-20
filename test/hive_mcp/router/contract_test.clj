(ns hive-mcp.router.contract-test
  "Contract facets for the embedder router: the pure type→spec resolver,
   the over-budget escalation and the DefaultRouter constructor. Inputs are
   drawn from a fixed provider table so every resolution arm is reached."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as r]
            [hive-mcp.router.default :as default]
            [hive-mcp.router.escalate :as esc]
            [hive-mcp.router.protocol :as proto]
            [hive-mcp.router.resolve :as resolve]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Test-domain schemas — configs over a small provider-key alphabet so routes,
;;; the default and the provider table collide often enough to reach every arm
;;; ---------------------------------------------------------------------------

(def ^:private ProviderKey [:enum :p/small :p/heavy :p/broken :p/missing])

(def ^:private providers
  {:p/small  {:impl :ollama :model "small" :dimension 1024 :max-tokens 8192}
   :p/heavy  {:impl :venice :model "heavy" :dimension 4096 :max-tokens 28000}
   ;; :impl outside the known set — embedder.spec/make throws
   :p/broken {:impl :bogus :model "broken" :dimension 1 :max-tokens 1}})

(def ^:private Config
  [:map
   [:default [:maybe ProviderKey]]
   [:routes [:map-of [:enum :type/note :type/plan :note :plan] ProviderKey]]
   [:providers [:= {} providers]]
   [:escalation [:map [:heavy-tier-key [:enum :p/heavy :p/missing]]]]])

(def ^:private MemoryType
  [:enum :type/note :note "note" "type/plan" :plan :axiom "axiom" nil 42])

(defn- expected-key
  "Oracle: the provider key per the resolve ns docstring's precedence."
  [config memory-type]
  (let [bare (cond (keyword? memory-type) (name memory-type)
                   (string? memory-type) (last (clojure.string/split memory-type #"/"))
                   :else nil)
        routes (:routes config)]
    (or (when bare (get routes (keyword "type" bare)))
        (when bare (get routes (keyword bare)))
        (:default config))))

;;; ---------------------------------------------------------------------------
;;; resolve
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema provider-key-follows-precedence
  #'resolve/resolve-provider-key
  {:in [:cat Config MemoryType]
   :out [:maybe :keyword]
   :rel (fn [[config mt] out] (= out (expected-key config mt)))
   :num-tests 200})

(deftrifecta-from-schema resolve-spec-names-every-failure
  #'resolve/resolve-spec
  {:in [:cat Config MemoryType]
   :out resolve/SpecResult
   :rel (fn [[config mt] out]
          (let [k (expected-key config mt)]
            (cond
              (nil? k)                (= :router/no-default (:error out))
              (= :p/missing k)        (= :router/unknown-provider (:error out))
              (= :p/broken k)         (= :router/invalid-provider-spec (:error out))
              :else                   (and (= k (get-in out [:ok :provider/key]))
                                           (= (get-in providers [k :dimension])
                                              (get-in out [:ok :provider/dim]))))))
   :classify (fn [_ out] (if (r/ok? out) :ok (:error out)))
   :classify-domain #{:ok :router/no-default :router/unknown-provider
                      :router/invalid-provider-spec}
   :golden-path "test/golden/hive-mcp/router/resolve-spec.edn"
   :num-tests 200})

;;; ---------------------------------------------------------------------------
;;; escalate
;;; ---------------------------------------------------------------------------

(def ^:private small-spec (:ok (resolve/resolve-spec {:default :p/small :providers providers} nil)))
(def ^:private heavy-spec (:ok (resolve/resolve-spec {:default :p/heavy :providers providers} nil)))

(def ^:private DocSize
  ;; nil / non-number read as fitting; the sizes straddle both ceilings
  ;; (8192 small, 28000 heavy) and sit mostly over budget so the escalated
  ;; arm is reached as often as the kept one
  [:enum nil "big" 100 8192 8193 12000 20000 28001 39000])

(deftrifecta-from-schema over-budget-is-strict-and-numeric
  #'esc/over-budget?
  {:in [:cat [:enum {} small-spec heavy-spec] DocSize]
   :out :boolean
   :rel (fn [[spec size] out]
          (= out (and (number? size) (> size (:provider/max-tokens spec)))))
   :num-tests 200})

(deftrifecta-from-schema escalation-reaches-the-heavy-tier
  #'esc/maybe-escalate
  {:in [:cat Config [:enum {} small-spec heavy-spec] DocSize]
   :out [:or [:map [:ok esc/SpecWithCeiling]] [:map [:error resolve/RouterError]]]
   :rel (fn [[config spec size] out]
          (let [heavy (get-in config [:escalation :heavy-tier-key])]
            (cond
              (not (esc/over-budget? spec size))      (= {:ok spec} out)
              (= heavy (:provider/key spec))          (= {:ok spec} out)
              (= :p/missing heavy)                    (= :router/unknown-provider (:error out))
              :else                                   (= :p/heavy (get-in out [:ok :provider/key])))))
   :classify (fn [[_ spec _] out]
               (cond (r/err? out) :err
                     (= spec (:ok out)) :kept
                     :else :escalated))
   :classify-domain #{:err :kept :escalated}
   :num-tests 200})

(deftest escalate-contract-names-its-own-ceiling-shape
  (testing "SpecWithCeiling is the part of ProviderSpec escalation reads"
    (is (m/validate esc/SpecWithCeiling small-spec))
    (is (not (m/validate esc/SpecWithCeiling {:provider/key :x})))))

;;; ---------------------------------------------------------------------------
;;; default router — constructed over a pinned block fn
;;; ---------------------------------------------------------------------------

(golden/deftest-golden default-router-delegates-golden
  "test/golden/hive-mcp/router/default-router.edn"
  (let [config {:default :p/small :routes {:type/plan :p/heavy}
                :providers providers :escalation {:heavy-tier-key :p/heavy}}
        router (default/make-router (constantly config))
        before @(:invalidated? router)]
    (proto/invalidate! router)
    {:note       (proto/resolve-for-type router :type/note)
     :plan       (proto/resolve-for-type router "plan")
     :fits       (proto/escalate-if-large router small-spec 100)
     :escalated  (proto/escalate-if-large router small-spec 9000)
     :invalidated [before @(:invalidated? router)]}))

(deftest make-router-satisfies-both-ports
  (is (m/validate default/Router (default/make-router)))
  (is (m/validate default/Router (default/make-router (constantly nil))))
  (is (= :router/no-default
         (:error (proto/resolve-for-type (default/make-router (constantly nil)) :note)))))

;;; ---------------------------------------------------------------------------
;;; Coverage gate
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage router-contracts
  {:paths ["src/hive_mcp/router"]})

(deftest router-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/router"]})]
    (is (= 5 (count (:universe report))) "every public defn of the tree")
    (is (= 5 (count (:covered report))))))

(deftest every-router-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.router.resolve hive-mcp.router.escalate hive-mcp.router.default]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))
