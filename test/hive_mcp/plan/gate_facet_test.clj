(ns hive-mcp.plan.gate-facet-test
  "Facets for the plan storage gate: trifectas on the plan-content? detector
   and the format-gate-error renderer, and a golden over validate-for-storage
   across every failure phase. The plan field registry is emptied around each
   test so only core fields are validated."
  (:require [clojure.string :as str]
            [clojure.test :refer [use-fixtures]]
            [hive-mcp.plan.field-registry :as fr]
            [hive-mcp.plan.gate :as gate]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- registry-fixture [f]
  (let [saved @@#'fr/registry]
    (fr/reset-registry!)
    (try (f) (finally (reset! @#'fr/registry saved)))))

(use-fixtures :each registry-fixture)

;; Content fragments: EDN plan shapes, markdown H2 headers, and prose that
;; looks plan-ish but is not. Two fragments are joined so mixed content occurs.
(def ^:private plan-fragments
  ["{:steps [{:id \"s1\" :title \"T\"}]}" "{:plan/steps [{:step/id \"s1\"}]}"
   "```edn\n{:steps []}\n```" "## Step 1\nDo it"])
(def ^:private prose-fragments
  ["Just a note" "FRICTION: x" "# Title only" "{:x 1}" "  " "### deep header"])

(def ^:private Content
  [:or {:gen/schema [:tuple [:enum :plan :prose :none] [:vector {:min 2 :max 2} [:enum 0 1 2 3]]]
        :gen/fmap (fn [[kind [a b]]]
                    (case kind
                      :plan (str (nth prose-fragments a) "\n" (nth plan-fragments b))
                      :prose (str (nth prose-fragments a) "\n" (nth prose-fragments (+ 2 b)))
                      :none nil))}
   :string :nil])

(defn- expected-plan-content? [content]
  (boolean (and (string? content)
                (some #(str/includes? content %) plan-fragments))))

(hst/deftrifecta-from-schema plan-content-contract
  hive-mcp.plan.gate/plan-content?
  {:in [:cat Content]
   ;; the contract's output is :any; the body's (and (string? ..) ..) is a boolean
   :out :boolean
   :rel (fn [[content] out] (= out (expected-plan-content? content)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/plan/gate-plan-content.edn"})

(def ^:private GateFailure
  [:map
   [:errors [:vector {:max 3} [:enum "Schema: missing :title" "Missing dep: step-9" "cycle a->b"]]]
   [:hint [:enum "Fix the schema" "Break the cycle"]]
   [:phase {:optional true} [:enum :parse :schema :dependencies :cycles]]])

(hst/deftrifecta-from-schema format-gate-error-contract
  hive-mcp.plan.gate/format-gate-error
  {:in [:cat GateFailure]
   :out (-> (m/function-schemas) (get-in ['hive-mcp.plan.gate 'format-gate-error :schema]) m/children second)
   :rel (fn [[{:keys [errors hint phase]}] out]
          (and (str/starts-with? out (str "Plan validation failed (phase: " (name (or phase :unknown)) ")"))
               (every? #(str/includes? out (str "  - " %)) errors)
               (str/includes? out hint)))
   :golden-path "test/golden/hive-mcp/plan/gate-format-gate-error.edn"})

(defn- gate-view [content]
  (let [r (gate/validate-for-storage content)]
    (select-keys r [:valid? :phase :errors :metadata])))

(golden/deftest-golden validate-for-storage-golden
  "test/golden/hive-mcp/plan/gate-validate-for-storage.edn"
  {:valid-edn (gate-view "{:id \"p\" :title \"P\" :steps [{:id \"s1\" :title \"A\"} {:id \"s2\" :title \"B\" :depends-on [\"s1\"]}]}")
   :valid-markdown (gate-view "# Plan\n\n## Step 1: Setup\nA\n\n## Step 2: Build\nB")
   :empty-steps (gate-view "{:id \"p\" :title \"P\" :steps []}")
   :missing-dep (gate-view "{:id \"p\" :title \"P\" :steps [{:id \"s1\" :title \"A\" :depends-on [\"s9\"]}]}")
   :cycle (gate-view "{:id \"p\" :title \"P\" :steps [{:id \"a\" :title \"A\" :depends-on [\"b\"]} {:id \"b\" :title \"B\" :depends-on [\"a\"]}]}")
   :unparseable (gate-view "random text {broken")
   :nil (gate-view nil)})
