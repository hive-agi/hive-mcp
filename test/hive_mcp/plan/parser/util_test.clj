(ns hive-mcp.plan.parser.util-test
  "Facets for the plan parser utilities: plan->task-specs (with a projected
   addon field registered by the fixture) and validate-dependencies, both
   driven by their m/=> contracts."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.plan.field-registry :as fr]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [hive-mcp.plan.parser.util]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- registry-fixture [f]
  (let [saved @@#'fr/registry]
    (fr/reset-registry!)
    (fr/register-field! {:scope :step :key :method :project? true})
    (fr/register-field! {:scope :step :key :hidden})
    (try (f) (finally (reset! @#'fr/registry saved)))))

(use-fixtures :each registry-fixture)

(defn- out-schema [fsym]
  (-> (m/function-schemas) (get-in ['hive-mcp.plan.parser.util fsym :schema]) m/children second))

(def ^:private Id [:enum "a" "b" "c" "d"])

(def ^:private NormalizedPlan
  [:map
   [:steps [:vector {:max 4}
            [:map
             [:id Id]
             [:title [:enum "A" "B"]]
             [:description {:optional true} [:maybe [:enum "desc"]]]
             [:priority [:enum :high :medium :low]]
             [:tags [:vector {:max 2} [:enum "x" "y"]]]
             [:depends-on [:vector {:max 2} Id]]
             [:method {:optional true} [:enum :saa :manual]]
             [:hidden {:optional true} :int]]]]])

(hst/deftrifecta-from-schema plan->task-specs-contract
  hive-mcp.plan.parser.util/plan->task-specs
  {:in [:cat NormalizedPlan]
   :out (out-schema 'plan->task-specs)
   :rel (fn [[plan] specs]
          (= specs
             (mapv (fn [s]
                     (cond-> {:title (:title s) :description (:description s)
                              :priority (name (:priority s)) :tags (:tags s)
                              :depends-on (:depends-on s) :plan-step-id (:id s)}
                       (contains? s :method) (assoc :method (:method s))))
                   (:steps plan))))
   :golden-path "test/golden/hive-mcp/plan/parser/util-plan-task-specs.edn"})

(hst/deftrifecta-from-schema validate-dependencies-contract
  hive-mcp.plan.parser.util/validate-dependencies
  {:in [:cat NormalizedPlan]
   :out (out-schema 'validate-dependencies)
   :rel (fn [[plan] out]
          (let [ids (set (map :id (:steps plan)))
                bad (first (filter #(some (complement ids) (:depends-on %)) (:steps plan)))]
            (if bad
              (= out {:valid false :step (:id bad) :missing (vec (remove ids (:depends-on bad)))})
              (= out {:valid true}))))
   :classify (fn [_ out] (if (:valid out) :resolved :missing))
   :classify-domain #{:resolved :missing}
   :golden-path "test/golden/hive-mcp/plan/parser/util-validate-dependencies.edn"})
