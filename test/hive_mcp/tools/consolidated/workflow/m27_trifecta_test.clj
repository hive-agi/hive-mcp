(ns hive-mcp.tools.consolidated.workflow.m27-trifecta-test
  "Schema-derived trifectas over the pure workflow helpers contracted in batch
   m27 (forge-ops sorting/parsing/view, forge-cycle resource building), plus
   golden + mutation facets over the IR and plan-goal refusal arms, which
   answer without hive-workflows on the classpath."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.consolidated.workflow.forge-cycle :as fc]
            [hive-mcp.tools.consolidated.workflow.forge-ops :as fo]
            [hive-mcp.tools.consolidated.workflow.ir :as ir]
            [hive-mcp.tools.core :as tc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Task
  [:map
   [:id [:enum "t1" "t2" "t3" "t4"]]
   [:priority {:optional true} [:maybe [:enum "high" "medium" "low" "priority-high" "priority-low" "bogus"]]]])

(def rank {"high" 0 "priority-high" 0 "medium" 1 "low" 2 "priority-low" 2})

(deftrifecta-from-schema sort-by-priority-then-created-contract
  hive-mcp.tools.consolidated.workflow.forge-ops/sort-by-priority-then-created
  {:in          [:vector {:max 6} Task]
   :out         [:vector :map]
   :rel         (fn [in out]
                  (let [k (fn [t] [(get rank (or (:priority t) "medium") 1) (str (:id t))])]
                    (and (= (frequencies in) (frequencies out))
                         (= (map k out) (sort (map k out))))))
   :golden-path "test/golden/tools/consolidated/workflow-sort-by-priority.edn"
   :num-tests   200})

(def KanbanListText
  "parse-kanban-tasks' inputs: a vector JSON body, a {:tasks} JSON body, an
   MCP response wrapping either, or text that is not JSON."
  [:or
   [:enum "[]" "[{\"id\":\"a\"}]" "{\"tasks\":[{\"id\":\"b\"}]}" "{\"other\":1}" "not json" ""]
   [:map [:text [:enum "[{\"id\":\"c\"}]" "{\"tasks\":[]}" "garbage"]]]
   :nil])

(defn- expected-tasks [in]
  (let [text (if (map? in) (:text in) in)
        parsed (try (json/read-str text :key-fn keyword) (catch Throwable _ nil))]
    (cond (sequential? parsed) parsed
          (map? parsed) (or (:tasks parsed) [])
          :else [])))

(deftrifecta-from-schema parse-kanban-tasks-contract
  hive-mcp.tools.consolidated.workflow.forge-ops/parse-kanban-tasks
  {:in          KanbanListText
   :out         [:sequential :map]
   :rel         (fn [in out] (= (expected-tasks in) out))
   :golden-path "test/golden/tools/consolidated/workflow-parse-kanban-tasks.edn"
   :num-tests   200})

(def SurveyTask
  [:map
   [:id [:enum "a" "b"]]
   [:title :string]
   [:description {:optional true} :string]
   [:context {:optional true}
    [:map [:execution {:optional true}
           [:map [:model {:optional true} [:enum "m1"]]
            [:persona {:optional true} :boolean]
            [:secret {:optional true} :string]]]]]])

(deftrifecta-from-schema survey-view-contract
  hive-mcp.tools.consolidated.workflow.forge-ops/survey-view
  {:in          [:map [:tasks [:vector {:max 3} SurveyTask]] [:count {:optional true} :int]]
   :out         :map
   :rel         (fn [in out]
                  (and (= (dissoc in :tasks) (dissoc out :tasks))
                       (every? #(not (contains? % :description)) (:tasks out))
                       (every? #(not (contains? % :context)) (:tasks out))
                       (= (map :id (:tasks in)) (map :id (:tasks out)))))
   :golden-path "test/golden/tools/consolidated/workflow-survey-view.edn"
   :num-tests   200})

(defn fsm-config-of
  "Unary adapter: the :config build-fsm-resources derives from PARAMS."
  [params]
  (:config (fc/build-fsm-resources params)))

(deftrifecta-from-schema build-fsm-resources-contract
  hive-mcp.tools.consolidated.workflow.m27-trifecta-test/fsm-config-of
  {:in          [:map
                 [:max_slots {:optional true} [:maybe [:int {:min 1 :max 20}]]]
                 [:spawn_mode {:optional true} [:maybe [:enum "vterm" "headless" "claude"]]]
                 [:presets {:optional true} [:maybe [:vector {:max 2} [:enum "ling" "saa"]]]]]
   :out         [:map [:max-slots :int] [:presets [:sequential :string]] [:spawn-mode [:maybe :keyword]]]
   :rel         (fn [{:keys [max_slots spawn_mode presets]} out]
                  (and (= (or max_slots 10) (:max-slots out))
                       (= (some-> spawn_mode keyword) (:spawn-mode out))
                       (= (or presets ["ling" "mcp-first" "saa"]) (:presets out))))
   :golden-path "test/golden/tools/consolidated/workflow-build-fsm-config.edn"
   :num-tests   200})

(deftrifecta ir-refusals
  hive-mcp.tools.consolidated.workflow.ir/handle-author
  {:golden-path "test/golden/tools/consolidated/workflow-ir-author-refusal.edn"
   :cases       {:no-ast {} :nil nil :number-ast {:ast 42}}
   :gen         (gen/elements [{} {:ast nil} {:ast 1} {:ast [:not-a-map]}])
   :pred        #(and (true? (:isError %)) (m/validate tc/McpResponse %))
   :num-tests   20
   :mutations   [["accepts-anything" (fn [_] (tc/mcp-json {:ok true}))]
                 ["wrong-refusal" (fn [_] (tc/mcp-error "unavailable"))]]})

(deftest every-ir-verb-answers-an-mcp-response
  (doseq [h [ir/handle-list ir/handle-get ir/handle-describe ir/handle-register ir/handle-run
             ir/handle-status ir/handle-cancel ir/handle-describe-method ir/handle-describe-vocabulary]]
    (let [r (h {:workflow_id "m27-wf"})]
      (is (m/validate tc/McpResponse r) (str h)))))

(deftest plan-goal-refusals-name-the-missing-param
  (let [plan-goal @(requiring-resolve 'hive-mcp.tools.consolidated.workflow.goal/handle-plan-goal)]
    (is (str/includes? (:text (plan-goal {})) ":goal_spec"))
    (is (str/includes? (:text (plan-goal {:goal_spec "{:goal #{} :init #{}}" :verb_index "[1]"}))
                       ":verb_index"))))
