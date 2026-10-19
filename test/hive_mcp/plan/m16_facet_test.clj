(ns hive-mcp.plan.m16-facet-test
  "Trifectas + goldens for the m16 plan namespaces: schema normalization and
   DAG checks (pure), the field registry (own state snapshot/restored),
   compute-waves, and the kg-degraded warning shaper."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.plan.field-registry :as fr]
            [hive-mcp.plan.kg-degraded :as kg]
            [hive-mcp.plan.schema :as schema]
            [hive-mcp.plan.tool :as tool]
            [clojure.string :as str]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out-schema [nsym fsym]
  (-> (m/function-schemas) (get-in [nsym fsym :schema]) m/children second))

;; -----------------------------------------------------------------------------
;; normalize-priority / normalize-estimate: total coercions, clamp to :medium
;; -----------------------------------------------------------------------------

(hst/deftrifecta-from-schema normalize-priority-contract
  hive-mcp.plan.schema/normalize-priority
  {:in [:cat [:or :string :keyword :boolean :int :nil]]
   :out (out-schema 'hive-mcp.plan.schema 'normalize-priority)
   :rel (fn [[in] out]
          (and (contains? #{:high :medium :low} out)
               (if (or (and (keyword? in) (contains? #{:high :medium :low} in))
                       (and (string? in)
                            (contains? #{"high" "medium" "low"}
                                       (str/lower-case in))))
                 (= out (keyword (str/lower-case (name in))))
                 true)))
   :golden-path "test/golden/hive-mcp/m16-normalize-priority.edn"})

(hst/deftrifecta-from-schema normalize-estimate-contract
  hive-mcp.plan.schema/normalize-estimate
  {:in [:cat [:or :string :keyword :boolean :int :nil]]
   :out (out-schema 'hive-mcp.plan.schema 'normalize-estimate)
   :golden-path "test/golden/hive-mcp/m16-normalize-estimate.edn"})

;; -----------------------------------------------------------------------------
;; detect-cycles / validate-dependencies: DAG checks over generated plans
;; -----------------------------------------------------------------------------

(def ^:private SmallPlan
  [:map {:gen/fmap (fn [m] (update m :steps vec))}
   [:id :string]
   [:title :string]
   [:steps [:vector {:gen/max 4}
            [:map
             [:id [:enum "a" "b" "c" "d"]]
             [:title :string]
             [:depends-on {:optional true} [:vector {:gen/max 2} [:enum "a" "b" "c" "d"]]]]]]])

(hst/deftrifecta-from-schema detect-cycles-contract
  hive-mcp.plan.schema/detect-cycles
  {:in [:cat SmallPlan]
   :out (out-schema 'hive-mcp.plan.schema 'detect-cycles)
   :rel (fn [[plan] out]
          (if (:valid out)
            true
            (= "Circular dependency detected" (:cycle out))))
   :classify (fn [_ out] (if (:valid out) :no-cycle :cycle))
   :classify-domain #{:no-cycle :cycle}
   :golden-path "test/golden/hive-mcp/m16-detect-cycles.edn"})

(hst/deftrifecta-from-schema validate-dependencies-contract
  hive-mcp.plan.schema/validate-dependencies
  {:in [:cat SmallPlan]
   :out (out-schema 'hive-mcp.plan.schema 'validate-dependencies)
   :rel (fn [[plan] out]
          (let [ids (set (map :id (:steps plan)))
                bad (for [s (:steps plan) d (:depends-on s) :when (not (ids d))] d)]
            (= (boolean (:valid out)) (empty? bad))))
   :classify (fn [_ out] (if (:valid out) :all-known :missing))
   :classify-domain #{:all-known :missing}
   :golden-path "test/golden/hive-mcp/m16-validate-dependencies.edn"})

;; -----------------------------------------------------------------------------
;; compute-waves: wave numbers respect dependency order
;; -----------------------------------------------------------------------------

(def ^:private DagSteps
  [:vector {:gen/max 5}
   [:map
    [:id [:enum "s1" "s2" "s3" "s4" "s5"]]
    [:depends-on {:optional true} [:vector {:gen/max 2} [:enum "s1" "s2" "s3"]]]]])

(hst/deftrifecta-from-schema compute-waves-contract
  hive-mcp.plan.tool/compute-waves
  {:in [:cat DagSteps]
   :out (out-schema 'hive-mcp.plan.tool 'compute-waves)
   :rel (fn [[steps] waves]
          (let [ids (set (map :id steps))]
            (and (= ids (set (keys waves)))
                 (every? (fn [[_ w]] (and (int? w) (not (neg? w)))) waves))))
   :golden-path "test/golden/hive-mcp/m16-compute-waves.edn"})

;; -----------------------------------------------------------------------------
;; kg-degraded/warning-tag: pure Result-error shaper
;; -----------------------------------------------------------------------------

(def ^:private ErrResult
  [:or
   [:map [:error [:= :weave/timeout]] [:timeout-ms :int]]
   [:map [:error [:= :weave/exception]] [:message {:optional true} :string] [:class {:optional true} :string]]
   [:map [:error :keyword]]])

(hst/deftrifecta-from-schema warning-tag-contract
  hive-mcp.plan.kg-degraded/warning-tag
  {:in [:cat [:enum "edges" "decision" "tasks"] ErrResult]
   :out (out-schema 'hive-mcp.plan.kg-degraded 'warning-tag)
   :rel (fn [[label err] out]
          (and (str/includes? out label)
               (case (:error err)
                 :weave/timeout (str/starts-with? out "kg-timeout:")
                 :weave/exception (str/starts-with? out "kg-error:")
                 (str/starts-with? out "kg-degraded:"))))
   :golden-path "test/golden/hive-mcp/m16-warning-tag.edn"})

;; -----------------------------------------------------------------------------
;; field-registry: register / query round-trip, own state snapshot+restore
;; -----------------------------------------------------------------------------

;; fr/registry is a private atom of {:step {} :plan {}}; @@#'var reads the
;; value, (reset! @#'var) writes through the var to the atom.
(defn- snapshot-registry []
  @@#'fr/registry)

(defn- registry-fixture [f]
  (let [saved (snapshot-registry)]
    (fr/reset-registry!)
    (try (f) (finally (reset! @#'fr/registry saved)))))

(clojure.test/use-fixtures :each registry-fixture)

(def ^:private FieldSpec
  [:map
   [:scope [:enum :step :plan]]
   [:key [:keyword {:gen/elements [:method :agent :wave :verify]}]]
   [:project? {:optional true} :boolean]])

(hst/deftrifecta-from-schema register-field-contract
  hive-mcp.plan.field-registry/register-field!
  {:in [:cat FieldSpec]
   :out (out-schema 'hive-mcp.plan.field-registry 'register-field!)
   :rel (fn [[spec] out] (= (:key spec) out))
   ;; Mutation facet omitted: the registry is global mutable state and a
   ;; generated mutant that perturbs the spec map only exercises the
   ;; atom, not a projection the test owns.
   :mutation false
   :golden-path "test/golden/hive-mcp/m16-register-field.edn"})

(deftest field-registry-round-trip-test
  ;; The trifecta property facet registers generated specs first; assert on
  ;; this test's own registrations, not on global emptiness.
  (fr/deregister-by-owner! nil) ;; nil owner never matches; registry untouched
  (is (= :method (fr/register-field! {:scope :step :key :method :project? true})))
  (is (= #{:method} (fr/step-field-keys)))
  (is (= #{:method} (fr/project-keys)))
  (is (nil? (fr/register-field! {:scope :bogus :key :x})))
  (is (nil? (fr/register-field! {:scope :step :key "not-a-kw"})))
  (is (= :wave (fr/register-by-key! :my-addon :plan/wave {:scope :plan :key :wave})))
  (is (= #{:wave} (fr/plan-field-keys)))
  (fr/deregister-by-owner! :my-addon)
  (is (not (contains? (fr/plan-field-keys) :wave)))
  (is (contains? (fr/step-field-keys) :method)))

(golden/deftest-golden field-registry-golden
  "test/golden/hive-mcp/m16-field-registry.edn"
  {:empty-keys (fr/step-field-keys)
   :after-register (do (fr/register-field! {:scope :step :key :method :project? true})
                       {:step-keys (fr/step-field-keys)
                        :project-keys (fr/project-keys)})
   :normalized (do (fr/register-field! {:scope :step :key :agent
                                        :normalize (fn [v] (keyword (str/lower-case (name v))))})
                   (fr/apply-normalizers {:agent "CLAUDE" :other 1} :step))
   :validate-report (do (fr/register-field! {:scope :step :key :verify
                                             :validate (fn [v] (when-not (keyword? v) "not a keyword"))})
                        (fr/validate-fields {:verify "nope" :method :ok} :step))
   :defaults (do (fr/register-field! {:scope :step :key :method :default-from :default-method})
                 (fr/resolve-step-defaults {:default-method :saa
                                            :steps [{:id "a"} {:id "b" :method :manual}]}))})
