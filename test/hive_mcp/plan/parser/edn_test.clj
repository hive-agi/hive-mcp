(ns hive-mcp.plan.parser.edn-test
  "Facets for the EDN plan parser: trifectas on parse-edn-plan over plans
   generated as data and rendered in each supported carrier (raw EDN, embedded
   in prose, a fenced ```edn block, namespaced keys), and on the two
   detectors. The plan field registry is emptied around each test so
   normalization uses core fields only."
  (:require [clojure.string :as str]
            [clojure.test :refer [use-fixtures]]
            [hive-mcp.plan.field-registry :as fr]
            [hive-mcp.plan.parser.edn :as pedn]
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

(defn- out-schema [fsym arity]
  (let [s (get-in (m/function-schemas) ['hive-mcp.plan.parser.edn fsym :schema])
        s (if (= :function (m/type s)) (nth (m/children s) arity) s)]
    (second (m/children s))))

(defn- namespaced [plan]
  (-> plan
      (update :steps (fn [steps]
                       (mapv #(update-keys % (fn [k] (keyword "step" (name k)))) steps)))
      (update-keys #(keyword "plan" (name %)))))

(defn- render [carrier plan]
  (case carrier
    :raw (pr-str plan)
    :embedded (str "# Plan Output\n\nHere is the plan:\n\n" (pr-str plan) "\n\nPlease review.")
    :fenced (str "Intro text\n\n```edn\n" (pr-str plan) "\n```\n")
    :namespaced (pr-str (namespaced plan))))

;; A plan drawn as data (keyword or string step ids, :id always present so the
;; parser never mints a timestamp id), plus the carrier it is rendered in.
(def ^:private PlanData
  [:map
   [:id [:enum "plan-a" "plan-b"]]
   [:title [:enum "Alpha" "Beta"]]
   [:steps [:vector {:min 1 :max 3}
            [:map
             [:id [:enum :s1 :s2 "s3"]]
             [:title [:enum "Do A" "Do B"]]
             [:priority {:optional true} [:enum :high :LOW "urgent"]]]]]])

(def ^:private Rendered
  [:string {:gen/schema [:tuple [:enum :raw :embedded :fenced :namespaced] PlanData]
            :gen/fmap (fn [[carrier plan]] (render carrier plan))}])

(defn- step-ids [content]
  ;; read the step ids back out of the rendered data, either key spelling
  (let [form (read-string (subs content (str/index-of content "{")))
        steps (or (:steps form) (:plan/steps form))]
    (mapv #(name (or (:id %) (:step/id %))) steps)))

(hst/deftrifecta-from-schema parse-edn-plan-contract
  hive-mcp.plan.parser.edn/parse-edn-plan
  {:in [:cat Rendered]
   :out (out-schema 'parse-edn-plan 0)
   :rel (fn [[content] out]
          (and (:success out)
               (= (step-ids content) (mapv :id (get-in out [:plan :steps])))
               (= :edn (get-in out [:plan :source-format]))
               (every? #{:high :medium :low} (map :priority (get-in out [:plan :steps])))))
   :golden-path "test/golden/hive-mcp/plan/parser/edn-parse-edn-plan.edn"})

(def ^:private Mixed
  [:string {:gen/schema [:tuple [:enum :plan :phase :prose :fence-no-plan] PlanData]
            :gen/fmap (fn [[kind plan]]
                        (case kind
                          :plan (render :fenced plan)
                          :phase (str "```edn\n{:phase 1 :tasks " (pr-str (:steps plan)) "}\n```")
                          :prose (str "Notes about " (:title plan))
                          :fence-no-plan "```edn\n{:x 1}\n```"))}])

(hst/deftrifecta-from-schema contains-edn-plan-contract
  hive-mcp.plan.parser.edn/contains-edn-plan?
  ;; Mixed is half plan-shaped (fenced plan or phase block); the :int arm
  ;; drives the documented "false on non-strings" branch.
  {:in [:cat [:or {:gen/schema [:tuple Mixed :int [:enum 0 1 2 3 4 5]]
                   :gen/fmap (fn [[s i arm]] (if (zero? arm) i s))}
              Mixed :int]]
   :out (out-schema 'contains-edn-plan? 0)
   :rel (fn [[content] out]
          (= out (boolean (and (string? content)
                               (or (str/includes? content ":steps")
                                   (str/includes? content ":tasks"))))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/plan/parser/edn-contains-edn-plan.edn"})

(hst/deftrifecta-from-schema contains-edn-block-contract
  hive-mcp.plan.parser.edn/contains-edn-block?
  ;; contract input is :any, but the body re-finds on it: strings only
  {:in [:cat Mixed]
   :out (out-schema 'contains-edn-block? 0)
   :rel (fn [[content] out] (= out (str/includes? content "```edn")))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :golden-path "test/golden/hive-mcp/plan/parser/edn-contains-edn-block.edn" :classify-floor 11 :num-tests 150})

(golden/deftest-golden parse-edn-plan-strategies-golden
  "test/golden/hive-mcp/plan/parser/edn-strategies.edn"
  (let [strip (fn [r] (cond-> r (:success r) (update :plan dissoc :id)))]
    {:phase-blocks (strip (pedn/parse-edn-plan
                           (str "```edn\n{:phase 1 :tasks [{:id :a :title \"A\"}]}\n```\n"
                                "```edn\n{:phase 2 :tasks [{:id :b :title \"B\" :depends-on [:a]}]}\n```")))
     :empty-steps (strip (pedn/parse-edn-plan "{:steps []}"))
     :no-plan (pedn/parse-edn-plan "nothing here")
     :broken (pedn/parse-edn-plan "{:steps [{:id")}))
