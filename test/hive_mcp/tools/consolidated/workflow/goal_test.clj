(ns hive-mcp.tools.consolidated.workflow.goal-test
  "G8 tool boundary: goal-schema projection, its example planning verbatim, the
   plan-goal dry-run sound plan over demo vocab, and fail-loud on unsatisfiable /
   missing input — dispatched through the consolidated workflow handler.

   plan-goal exercises the live hive-workflows facade; its round-trip assertions
   are guarded on the facade + hive-spi workflow schemas being on the classpath
   (absent under cold driver-free CI, present under the local :local/root tree).
   goal-schema + the missing-input guard run unconditionally."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.tools.consolidated.workflow :as wf]
            [hive-mcp.tools.consolidated.workflow.goal :as goal]
            [hive-mcp.tools.core :as core]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- ok-json
  "Parse the JSON body of a successful MCP response (nil on an error response)."
  [resp]
  (when-not (:isError resp)
    (json/read-str (:text resp) :key-fn keyword)))

(defn- planner-available? []
  (some? (try (requiring-resolve 'hive-spi.workflow.planner/GoalSpec)
              (catch Throwable _ nil))))

(defn- facade-available? []
  (some? (try (requiring-resolve 'hive-workflows.mcp/plan-goal)
              (catch Throwable _ nil))))

(def ^:private demo-vi-edn
  (pr-str {:demo/scan   {:provides #{[:carto/scanned]} :cost 1}
           :demo/test   {:requires #{[:carto/scanned]} :provides #{[:tests/green]} :cost 1}
           :demo/commit {:requires #{[:tests/green]} :provides #{[:work/committed]} :cost 1}
           :demo/notify {:provides #{[:human/notified]} :cost 1}}))

;;; ── wiring ───────────────────────────────────────────────────────────────────

(deftest workflow-tool-advertises-goal-commands
  (let [enum (get-in wf/tool-def [:inputSchema :properties "command" :enum])
        props (get-in wf/tool-def [:inputSchema :properties])]
    (is (some #{"plan-goal"} enum))
    (is (some #{"goal-schema"} enum))
    (is (contains? props "goal_spec"))
    (is (contains? props "verb_index"))))

;;; ── goal-schema (always available: pure hive-spi projection) ────────────────

(deftest goal-schema-projects-the-goalspec-contract
  (when (planner-available?)
    (testing "goal-schema returns JSON-Schema + required keys + example + how-to"
      (let [body (ok-json (goal/handle-goal-schema {}))]
        (is (true? (:success body)))
        (is (= "object" (get-in body [:json-schema :type])))
        (is (= ["goal" "init"] (:required body)))
        (is (contains? (:example body) :goal))
        (is (contains? body :example-verb-index))
        (is (string? (:how-to body)))))))

;;; ── plan-goal round-trip through the tool boundary ──────────────────────────

(deftest plan-goal-round-trip-returns-sound-plan
  (when (facade-available?)
    (testing "command=plan-goal with a GoalSpec + demo verb_index -> sound plan"
      (let [resp (wf/handle-workflow
                  {:command    "plan-goal"
                   :goal_spec  (pr-str {:goal #{[:work/committed] [:human/notified]} :init #{}})
                   :verb_index demo-vi-edn})
            body (ok-json resp)]
        (is (not (:isError resp)) "dry-run succeeds")
        (is (true? (get-in body [:ok :sound?])))
        (is (= 4 (count (get-in body [:ok :plan :steps]))))
        (is (map? (get-in body [:ok :proof])))))))

(deftest goal-schema-example-plans-verbatim
  (when (facade-available?)
    (testing "the authored goal-schema example + verb-index plan to a sound plan"
      (let [example (deref #'goal/example-goal-spec)
            vi      (deref #'goal/example-verb-index)
            resp    (goal/handle-plan-goal {:goal_spec  (pr-str example)
                                            :verb_index (pr-str vi)})
            body    (ok-json resp)]
        (is (not (:isError resp)))
        (is (true? (get-in body [:ok :sound?])))))))

;;; ── fail-loud through the boundary ──────────────────────────────────────────

(deftest plan-goal-missing-goal-spec-is-loud
  (testing "no :goal_spec -> loud error (no facade needed)"
    (let [resp (goal/handle-plan-goal {})]
      (is (:isError resp))
      (is (re-find #"goal_spec" (:text resp))))))

(deftest plan-goal-unsatisfiable-fails-loud-through-boundary
  (when (facade-available?)
    (testing "a goal fact nobody provides surfaces :goal/no-plan at the boundary"
      (let [resp (goal/handle-plan-goal
                  {:goal_spec  (pr-str {:goal #{[:nope/x]} :init #{}})
                   :verb_index demo-vi-edn})]
        (is (:isError resp))
        (is (re-find #"no-plan" (:text resp)))))))

(deftest plan-goal-unknown-verb-index-shape-is-loud
  (testing "a non-map :verb_index EDN is rejected at the boundary"
    (let [resp (goal/handle-plan-goal
                {:goal_spec  (pr-str {:goal #{[:work/committed]} :init #{}})
                 :verb_index "[:not :a :map]"})]
      (is (:isError resp))
      (is (re-find #"verb_index" (:text resp))))))

;;; ── facets over the input guards (no facade needed) ────────────────────────

(def ^:private BadGoalParams
  "plan-goal params whose :goal_spec is absent, not EDN, or neither map nor
   string: the guard answers before the facade is ever resolved."
  [:map
   [:goal_spec {:optional true} [:or :nil :int :boolean [:enum "{:goal" "#{" "(" "{:a"]]]
   [:author {:optional true} :boolean]
   [:directory {:optional true} [:enum "/tmp/x"]]])

(deftrifecta-from-schema plan-goal-without-goal-spec-is-loud
  hive-mcp.tools.consolidated.workflow.goal/handle-plan-goal
  {:in        BadGoalParams
   :out       core/McpResponse
   :rel       (fn [_ out]
                (and (true? (:isError out))
                     (str/includes? (:text out) "requires :goal_spec")))
   :num-tests 200})

(deftest-golden plan-goal-guard-answers
  "test/golden/tools/consolidated/workflow/goal-guards.edn"
  {:missing-goal-spec (goal/handle-plan-goal {})
   :unparseable       (goal/handle-plan-goal {:goal_spec "{:goal"})
   :verb-index-vector (goal/handle-plan-goal {:goal_spec  {:goal #{[:a/b]} :init #{}}
                                              :verb_index "[:not :a :map]"})
   :verb-index-number (goal/handle-plan-goal {:goal_spec  "{:goal #{} :init #{}}"
                                              :verb_index 42})})
