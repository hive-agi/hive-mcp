(ns hive-mcp.tools.consolidated.workflow.forge-survey-verb-test
  "forge-ops/survey computes plan membership, per-card states and dependency
   readiness, and for a long time NOTHING public called it: its two non-test
   callers are both on the strike path. So the only way to ask 'what would this
   plan select' was to run a strike, and `forge survey` silently fell through
   the :forge :_handler to the belt dashboard, answering with global kanban
   totals. Every attempt to verify plan scoping read-only therefore looked like
   a deployment gap when it was a missing verb.

   These tests pin the verb, and pin that adding it did not move `forge status`."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.workflow :as wf]
            [clojure.data.json :as json]
            [hive-mcp.tools.consolidated.kanban :as c-kanban]
            [hive-mcp.scheduler.vulcan :as vulcan]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest survey-is-its-own-verb-not-the-belt-dashboard
  (let [forge (:forge wf/canonical-handlers)]
    (is (contains? forge :survey)
        "without this key the dispatcher falls through :_handler to status, and
         a plan-scoped question comes back answered with global totals")
    (is (not= (:survey forge) (:status forge))
        "status reports the BELT, survey reports the SELECTION")
    (is (= (:status forge) (:_handler forge))
        "the fallback stays what it was: adding a verb must not move the
         meaning of a bare `forge` call")))

(deftest the-verb-is-advertised
  (let [enum (get-in wf/tool-def [:inputSchema :properties "command" :enum])]
    (is (some #{"forge survey"} enum)
        "a verb absent from the schema is unreachable through the tool surface
         even when the handler exists")
    (testing "the verbs that were already there are still there"
      (is (some #{"forge status"} enum))
      (is (some #{"forge strike"} enum))
      (is (some #{"forge quench"} enum)))))

(deftest survey-reports-a-failure-as-a-result-rather-than-throwing
  (testing "forge-ops/survey THROWS when a plan_id cannot be resolved, by design
            (a plan survey that silently selects nothing is worse than an
            error). The handler must turn that into an MCP error result, not
            let it escape into the tool loop."
    (let [handler (get-in wf/canonical-handlers [:forge :survey])
          res     (handler {:plan_id "no-such-plan-id-20260910"
                            :directory "/test/proj"
                            :plan-entry-fn (constantly nil)})]
      (is (map? res))
      (is (true? (:isError res))
          "an unresolvable plan is reported, and reported as an error")
      (is (and (string? (:text res)) (seq (:text res)))
          "the error names what failed; an empty error text is indistinguishable
           from a success payload lost on the way out"))))

(deftest a-successful-survey-reaches-mcp-as-its-payload
  (testing "survey returns a plain selection map; the handler must deliver it as
            a non-error JSON payload carrying the plan evidence"
    (let [plan    {:id "p" :title "Diamond"
                   :steps [{:id "a" :title "A"} {:id "b" :title "B"}
                           {:id "c" :title "C" :depends-on ["a" "b"]}]}
          graph   {"p" #{"ka" "kb" "kc"} "ka" #{} "kb" #{} "kc" #{"ka" "kb"}}
          card    (fn [id title]
                    {:id id :content (json/write-str {:task-type "kanban" :title title
                                                      :status "todo"})})
          cards   {"ka" (card "ka" "A") "kb" (card "kb" "B") "kc" (card "kc" "C")}
          handler (get-in wf/canonical-handlers [:forge :survey])
          params  {:plan_id "p" :directory "/test/proj"
                   :plan-entry-fn (constantly {:type "plan" :content (pr-str plan)})
                   :task-entry-fn cards
                   :deps-fn graph
                   :state-fn #(vulcan/entry-state (get cards %))}
          res     (with-redefs [c-kanban/handle-kanban
                                (constantly {:text (json/write-str
                                                    [{:id "ka" :title "A"}
                                                     {:id "kb" :title "B"}
                                                     {:id "kc" :title "C"}])})]
                    (handler params))
          body    (some-> (:text res) not-empty (json/read-str :key-fn keyword))]
      (is (not (:isError res)) (str "success reported as error: " (pr-str res)))
      (is (= "p" (:plan-id body)))
      (is (= 3 (:plan-task-count body)))
      (is (= {:ka "open" :kb "open" :kc "open"} (:plan-states body)))
      (is (= "ready" (:selection-status body)))
      (is (= #{"ka" "kb"} (set (map :id (:tasks body))))))))
