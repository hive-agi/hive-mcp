(ns hive-mcp.plan.field-registry-test
  "Facets for the plan field registry's pure consult helpers
   (apply-normalizers, validate-fields, resolve-step-defaults), driven by
   their m/=> contracts against a registry this namespace owns: the fixture
   snapshots the private atom, installs a known field set, and restores the
   snapshot afterwards."
  (:require [clojure.string :as str]
            [clojure.test :refer [use-fixtures]]
            [hive-mcp.plan.field-registry :as fr]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out-schema [fsym]
  (-> (m/function-schemas) (get-in ['hive-mcp.plan.field-registry fsym :schema]) m/children second))

(defn- lower-kw [v] (keyword (str/lower-case (name v))))

(defn- install-fields! []
  (fr/reset-registry!)
  (fr/register-field! {:scope :step :key :method :normalize lower-kw
                       :default-from :default-method :project? true})
  (fr/register-field! {:scope :step :key :verify
                       :validate (fn [v] (when-not (keyword? v) "not a keyword"))})
  (fr/register-field! {:scope :plan :key :mode :normalize lower-kw
                       :validate (fn [v] (when (= :bad (lower-kw v)) "bad mode"))}))

;; fr/registry is a private atom; @@#'var reads it, (reset! @#'var) restores.
(defn- registry-fixture [f]
  (let [saved @@#'fr/registry]
    (install-fields!)
    (try (f) (finally (reset! @#'fr/registry saved)))))

(use-fixtures :each registry-fixture)

;; Values drawn so that each registered field's normalizer / validator branch
;; is reached: keywords and strings of mixed case, plus absent keys.
(def ^:private FieldVal [:or [:enum :SAA :Manual :bad :Bad] [:enum "Wave" "bad"]])

(def ^:private FieldMap
  [:map
   [:method {:optional true} FieldVal]
   [:verify {:optional true} FieldVal]
   [:mode {:optional true} FieldVal]
   [:other {:optional true} :int]])

(hst/deftrifecta-from-schema apply-normalizers-contract
  hive-mcp.plan.field-registry/apply-normalizers
  {:in [:cat FieldMap [:enum :step :plan]]
   :out (out-schema 'apply-normalizers)
   :rel (fn [[m scope] out]
          (let [normalized (if (= scope :step) #{:method} #{:mode})]
            (and (= (set (keys m)) (set (keys out)))
                 (every? (fn [[k v]]
                           (= (get out k) (if (normalized k) (lower-kw v) v)))
                         m))))
   :classify (fn [[m scope] _]
               (if (contains? m (if (= scope :step) :method :mode)) :normalized :untouched))
   :classify-domain #{:normalized :untouched}
   :golden-path "test/golden/hive-mcp/plan/field-registry-apply-normalizers.edn"})

(hst/deftrifecta-from-schema validate-fields-contract
  hive-mcp.plan.field-registry/validate-fields
  {:in [:cat FieldMap [:enum :step :plan]]
   :out (out-schema 'validate-fields)
   :rel (fn [[m scope] out]
          (let [expected (if (= scope :step)
                           (when (and (contains? m :verify) (not (keyword? (:verify m))))
                             [{:scope :step :key :verify :value (:verify m) :error "not a keyword"}])
                           (when (and (contains? m :mode) (= :bad (lower-kw (:mode m))))
                             [{:scope :plan :key :mode :value (:mode m) :error "bad mode"}]))]
            (= (vec expected) out)))
   :classify (fn [_ out] (if (seq out) :errors :clean))
   :classify-domain #{:errors :clean}
   :golden-path "test/golden/hive-mcp/plan/field-registry-validate-fields.edn" :classify-floor 12 :num-tests 150})

(def ^:private StepsPlan
  [:map
   [:default-method {:optional true} FieldVal]
   [:steps [:vector {:gen/max 3}
            [:map [:id [:enum "a" "b" "c"]] [:method {:optional true} [:enum :manual :saa]]]]]])

(hst/deftrifecta-from-schema resolve-step-defaults-contract
  hive-mcp.plan.field-registry/resolve-step-defaults
  {:in [:cat StepsPlan]
   :out (out-schema 'resolve-step-defaults)
   :rel (fn [[plan] out]
          (let [dv (some-> (:default-method plan) lower-kw)]
            (if (nil? dv)
              (= plan out)
              (and (= (count (:steps plan)) (count (:steps out)))
                   (every? true?
                           (map (fn [s o] (= o (if (some? (:method s)) s (assoc s :method dv))))
                                (:steps plan) (:steps out)))))))
   :classify (fn [[plan] _] (if (contains? plan :default-method) :defaulted :unchanged))
   :classify-domain #{:defaulted :unchanged}
   :golden-path "test/golden/hive-mcp/plan/field-registry-resolve-step-defaults.edn"})

(golden/deftest-golden field-registry-queries-golden
  "test/golden/hive-mcp/plan/field-registry-queries.edn"
  {:step-keys (fr/step-field-keys)
   :plan-keys (fr/plan-field-keys)
   :project-keys (fr/project-keys)
   :by-key (fr/register-by-key! :my-addon :plan/wave {:scope :plan :key :wave})
   :invalid (fr/register-field! {:scope :bogus :key :x})
   :after-dereg (do (fr/deregister-by-owner! :my-addon) (fr/plan-field-keys))})
