(ns hive-mcp.workflows.engine-registry-contract-test
  "Contracts and hive-test facets for the workflow registries, the FSM engine
   factory, ling catchup and the addon smoke-check.

   Registries are boundary code over atoms: each is driven under
   instrumentation with keys of its own, and the strategy registry's conflict
   policy is a property over generated registration histories. ling-catchup is
   pinned on its absent-extension arm (the cold test classpath)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as result]
            [hive-mcp.protocols.workflow :as proto]
            [hive-mcp.workflows.addon-smoke :as smoke]
            [hive-mcp.workflows.catchup-ling :as ling]
            [hive-mcp.workflows.fsm-engine :as engine]
            [hive-mcp.workflows.registry :as reg]
            [hive-mcp.workflows.strategy :as strategy]
            [hive-mcp.workflows.strategy-registry :as sr]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  (set (concat
        (for [s '[scan-fsm-specs register-handlers! compile-registry! get-workflow get-spec
                  list-workflows reload! reset-registry! register-forge-belt!
                  register-wrap-session! register-complete-session! register-catchup!
                  register-saa-workflow! init!]]
          (ns-resolve 'hive-mcp.workflows.registry s))
        (for [s '[register! deregister-by-owner! register-by-key! lookup
                  lookup-strategy-or-default all-methods snapshot]]
          (ns-resolve 'hive-mcp.workflows.strategy-registry s))
        [#'engine/create-engine #'ling/ling-catchup
         #'smoke/addons-on-classpath? #'smoke/addons-initialized? #'smoke/discovered-addons
         #'smoke/check])))

(use-fixtures :each
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument!)))))

;; =============================================================================
;; Strategy registry: owner-scoped, first write wins
;; =============================================================================

(def ^:private owners [::a ::b])
(def ^:private methods [::m1 ::m2 ::m3])

(defn- answering [v]
  (reify strategy/IDispatchStrategy (dispatch [_ _ _] (result/ok v))))

(defspec first-owner-keeps-the-method 50
  (prop/for-all [ops (gen/vector (gen/tuple (gen/elements owners) (gen/elements methods)) 0 10)]
    (try
      (let [outcomes (mapv (fn [[o mth]] (sr/register! o mth {:strategy (answering o)})) ops)
            first-owner (reduce (fn [acc [o mth]] (if (contains? acc mth) acc (assoc acc mth o))) {} ops)]
        (and (every? #{:ok :replaced :conflict} outcomes)
             (every? (fn [[mth o]] (= o (:owner (sr/lookup mth)))) first-owner)
             (= (set (keys first-owner))
                (set (filter (set methods) (sr/all-methods))))))
      (finally (doseq [o owners] (sr/deregister-by-owner! o))))))

(deftest strategy-registry-surface
  (try
    (is (= :ok (sr/register! ::a ::m1 {:strategy (answering 1)})))
    (is (= #{::m1} (sr/deregister-by-owner! ::a)))
    (is (= #{} (sr/deregister-by-owner! ::a)))
    (is (strategy/dispatch-strategy? (sr/lookup-strategy-or-default)))
    (is (strategy/dispatch-strategy? (sr/lookup-strategy-or-default ::nobody)))
    (let [{:keys [version data]} (sr/snapshot)]
      (is (int? version))
      (is (contains? (:by-id data) :wf/default) "the boot-seeded Noop"))
    (testing "hook routing: a lone entry, a vector, and a non-entry"
      (let [e (strategy/workflow-strategy-entry :wf/strategy {:method ::m2 :strategy (answering 2) :owner ::a})]
        (is (= [:ok] (sr/register-by-key! ::a :wf/strategy e)))
        (is (= [:replaced :ignored] (sr/register-by-key! ::a :wf/strategy [e {:method ::m3}])))
        (is (= [] (sr/register-by-key! ::a :wf/strategy "nope")))))
    (finally (sr/deregister-by-owner! ::a))))

;; =============================================================================
;; Workflow registry
;; =============================================================================

(deftest registry-lifecycle
  (try
    (let [statuses (reg/init!)]
      (is (every? #(m/validate reg/WorkflowStatus %) (vals statuses)))
      (is (every? (comp :compiled? statuses) [:wrap-session :complete-session :catchup :saa-workflow])
          "every built-in spec compiles against its registered handlers")
      (is (some? (reg/get-workflow :catchup)))
      (is (map? (reg/get-spec :catchup)))
      (is (nil? (reg/get-workflow :absent)))
      (is (nil? (reg/register-handlers! :absent {:start identity})) "unknown workflow: warn, no-op")
      (is (nil? (reg/reload!)))
      (is (:compiled? (:catchup (reg/list-workflows))) "reload keeps the handler registrations")
      (is (= :ok (reg/reset-registry!)))
      (is (= {} (reg/list-workflows))))
    (finally (reg/init!))))

(deftest-golden registry-built-ins
  "test/golden/workflows/registry-built-ins.edn"
  (into (sorted-map) (reg/init!)))

;; =============================================================================
;; Engine factory, ling catchup, smoke-check
;; =============================================================================

(deftest engine-factory
  (let [e (engine/create-engine)]
    (is (proto/workflow-engine? e))
    (is (false? (:loaded? (proto/load-workflow e :absent {}))))
    (is (nil? (engine/reset-statuses!)))))

(deftest ling-catchup-without-extension
  (when-not (try (requiring-resolve 'hive-mcp.extensions.context/ling-catchup)
                 (catch java.io.FileNotFoundException _extension-absent-on-cold-classpath nil))
    (is (nil? (ling/ling-catchup {:directory "/tmp"}))
        "absent extension: nil, the ling runs /catchup itself")
    (is (nil? (ling/ling-catchup nil)))))

(deftest smoke-check-shape
  (is (boolean? (smoke/addons-on-classpath?)))
  (is (boolean? (smoke/addons-initialized? #{})))
  (is (true? (smoke/addons-initialized? #{})) "vacuously initialized")
  (let [r (smoke/check)]
    (is (= (:ok? r) (and (empty? (:missing-addons r)) (empty? (:unresolved r))
                         (empty? (:missing-methods r)) (empty? (:scan-errors r)))))
    (is (string? (:report r)))))
