(ns hive-mcp.plan.plan-schema-contract-test
  "The plan-schema verb's :required / :step-required / :step-enums are derived
   from the projected JSON-Schema, never restated. These tests read the truth
   from the malli schema itself (hive-mcp.plan.schema) and fail when the verb's
   lists drift from it."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.plan.schema :as schema]
            [hive-mcp.plan.tool :as tool]
            [hive-spi.schema.derive :as derive]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]
            [cheshire.core :as json]))

(defn- malli-required
  "Required (non-optional) entry keys of a malli :map schema, as strings."
  [s]
  (into [] (keep (fn [[k props _]] (when-not (:optional props) (name k))))
        (m/children (m/schema s))))

(defn- malli-entry
  [s k]
  (some (fn [[ek _ child]] (when (= k ek) child)) (m/children (m/schema s))))

(defn- malli-enum
  "Values of a malli :enum (possibly wrapped in :maybe), as strings."
  [s]
  (let [s (m/schema s)
        s (if (= :maybe (m/type s)) (first (m/children s)) s)]
    (mapv name (m/children s))))

(def projected (:input-schema (derive/compile-op schema/Plan)))

(defn contract-of
  "Subject: derive the contract from a projected input-schema."
  [input-schema]
  (tool/plan-contract input-schema))

(deftrifecta plan-contract-derivation
  hive-mcp.plan.plan-schema-contract-test/contract-of
  {:cases {:plan projected}
   :xf    identity
   :gen   (gen/return projected)
   :pred  #(and (vector? (:required %)) (vector? (:step-required %))
                (map? (:step-enums %)))
   :num-tests 5})

(deftest contract-matches-the-malli-schema
  (let [{:keys [required step-required step-enums]} (tool/plan-contract projected)]
    (testing ":required is exactly the non-optional keys of schema/Plan"
      (is (= (set (malli-required schema/Plan)) (set required)))
      (is (= #{"id" "title" "steps"} (set required)) "historical value preserved"))
    (testing ":step-required is exactly the non-optional keys of schema/Step"
      (is (= (set (malli-required schema/Step)) (set step-required)))
      (is (= #{"id" "title"} (set step-required)) "historical value preserved"))
    (testing ":step-enums come from the Step :priority / :estimate enums"
      (is (= (set (malli-enum (malli-entry schema/Step :priority)))
             (set (:priority step-enums))))
      (is (= (set (malli-enum (malli-entry schema/Step :estimate)))
             (set (:estimate step-enums))))
      (is (= #{"high" "medium" "low"} (set (:priority step-enums))))
      (is (= #{"small" "medium" "large"} (set (:estimate step-enums)))))))

(deftest handler-response-shape-is-unchanged
  (let [body (json/parse-string (:text (tool/handle-plan-schema {})) true)]
    (is (true? (:success body)))
    (is (every? string? (:required body)))
    (is (every? string? (:step-required body)))
    (is (= #{:priority :estimate} (set (keys (:step-enums body)))))
    (is (= (tool/plan-contract projected)
           (-> body (select-keys [:required :step-required :step-enums]))))))

(deftest drift-is-detected
  (testing "removing a required key from the projection changes the derived list"
    (let [mutated (update projected :required (fn [r] (vec (remove #{"title" :title} r))))]
      (is (not= (:required (tool/plan-contract projected))
                (:required (tool/plan-contract mutated)))))))
