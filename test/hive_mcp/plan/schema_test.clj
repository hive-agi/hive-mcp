(ns hive-mcp.plan.schema-test
  "Facets for the plan schema namespace: the validators against the Step and
   Plan value objects (drawn from those very schemas, or from a looser raw
   step so both outcomes occur), normalize-step / normalize-plan (their output
   always conforms), and build-step-index. The plan field registry is emptied
   around each test so only core normalization runs."
  (:require [clojure.test :refer [use-fixtures]]
            [hive-mcp.plan.field-registry :as fr]
            [hive-mcp.plan.schema :as schema]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.util :as mu]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- registry-fixture [f]
  (let [saved @@#'fr/registry]
    (fr/reset-registry!)
    (try (f) (finally (reset! @#'fr/registry saved)))))

(use-fixtures :each registry-fixture)

(defn- out-schema [fsym]
  (-> (m/function-schemas) (get-in ['hive-mcp.plan.schema fsym :schema]) m/children second))

;; A raw step as a parser hands it to normalize-step: lenient enum spellings,
;; optional collections, sometimes a wrong-typed :id.
(def ^:private RawStep
  [:map
   [:id [:or [:enum "s1" "s2"] :int]]
   [:title [:enum "A" "B"]]
   [:priority {:optional true} [:or [:enum :HIGH :normal :urgent :low] [:enum "Medium" "trivial"] :nil]]
   [:estimate {:optional true} [:or [:enum :SMALL :huge :large] [:enum "medium"] :nil]]
   [:depends-on {:optional true} [:maybe [:vector {:max 2} [:enum "s1" "s2"]]]]
   [:files {:optional true} [:maybe [:vector {:max 1} [:enum "a.clj"]]]]])

(hst/deftrifecta-from-schema validate-step-contract
  hive-mcp.plan.schema/validate-step
  {:in [:cat [:or schema/Step RawStep]]
   :out (out-schema 'validate-step)
   :rel (fn [[step] out]
          (if (m/validate schema/Step step)
            (= out {:valid true :data step})
            (and (false? (:valid out)) (map? (:errors out)))))
   :classify (fn [_ out] (if (:valid out) :valid :invalid))
   :classify-domain #{:valid :invalid}
   :golden-path "test/golden/hive-mcp/plan/schema-validate-step.edn"})

(hst/deftrifecta-from-schema normalize-step-contract
  hive-mcp.plan.schema/normalize-step
  {:in [:cat [:and RawStep [:map [:id [:enum "s1" "s2"]]]]]
   :out schema/Step
   :rel (fn [[step] out]
          (and (= (:id step) (:id out))
               (= (schema/normalize-priority (:priority step)) (:priority out))
               (= (schema/normalize-estimate (:estimate step)) (:estimate out))
               (= (or (:depends-on step) []) (:depends-on out))))
   :golden-path "test/golden/hive-mcp/plan/schema-normalize-step.edn"})

(def ^:private RawPlan
  [:map
   [:id [:enum "p1"]]
   [:title [:enum "Plan"]]
   [:steps [:vector {:max 3} [:and RawStep [:map [:id [:enum "s1" "s2"]]]]]]
   [:source-format {:optional true} [:enum :edn :markdown]]])

(hst/deftrifecta-from-schema normalize-plan-contract
  hive-mcp.plan.schema/normalize-plan
  {:in [:cat RawPlan]
   :out schema/Plan
   :rel (fn [[plan] out]
          (and (= (count (:steps plan)) (count (:steps out)))
               (= (or (:source-format plan) :edn) (:source-format out))
               (= (mapv schema/normalize-step (:steps plan)) (:steps out))))
   :golden-path "test/golden/hive-mcp/plan/schema-normalize-plan.edn"})

;; schema/Plan itself, generated small: an unbounded [:vector Step] of full
;; Steps (each with an :execution map) takes about 20s per 100 draws.
(def ^:private SmallPlan
  (mu/assoc schema/Plan :steps [:vector {:gen/max 2} schema/Step]))

(hst/deftrifecta-from-schema valid-plan-contract
  hive-mcp.plan.schema/valid-plan?
  {:in [:cat [:or SmallPlan RawPlan]]
   :out (out-schema 'valid-plan?)
   :rel (fn [[plan] out] (= out (nil? (schema/explain-plan plan))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/plan/schema-valid-plan.edn"})

(hst/deftrifecta-from-schema build-step-index-contract
  hive-mcp.plan.schema/build-step-index
  {:in [:cat [:vector {:max 4} [:map [:id [:enum "a" "b" "c"]] [:n :int]]]]
   :out (out-schema 'build-step-index)
   :rel (fn [[steps] out]
          (and (= (set (map :id steps)) (set (keys out)))
               ;; last write wins for a duplicated id
               (every? (fn [[id s]] (= s (last (filter #(= id (:id %)) steps)))) out)))
   :golden-path "test/golden/hive-mcp/plan/schema-build-step-index.edn"})

(golden/deftest-golden plan-to-kanban-result-golden
  "test/golden/hive-mcp/plan/schema-plan-to-kanban-result.edn"
  {:valid (schema/valid-plan-to-kanban-result? {:plan-id "p" :task-ids ["t1"] :kg-edges []})
   :invalid (schema/valid-plan-to-kanban-result? {:plan-id 1})
   :explain-valid (schema/explain-plan-to-kanban-result {:plan-id "p" :task-ids [] :kg-edges []})
   :explain-invalid (schema/explain-plan-to-kanban-result {:plan-id 1 :task-ids ["t"]})
   :explain-step (schema/explain-step {:id 1})
   :valid-step (schema/valid-step? {:id "s" :title "t"})
   :example-valid (schema/valid-plan? schema/example-plan)
   :step-keys (vec (sort schema/step-keys))})
