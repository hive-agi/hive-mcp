(ns hive-mcp.workflows.complete-session-contract-test
  "Contracts and hive-test facets for the session-complete workflow.

   The dispatch predicates get schema trifectas (they are pure); each handler
   is driven through stub resources under instrumentation; handle-start's two
   [:or] arms are each produced and shown to fail the other; and the whole FSM
   gets a golden over its terminal summary."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.workflows.complete-session :as cs]
            [hive-mcp.workflows.schema :as wfs]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  (set (for [s '[valid? invalid? has-tasks? no-tasks? handle-start handle-commit handle-kanban
                 handle-crystallize handle-shout handle-plan-check handle-evict handle-end
                 compile-complete run-complete run-session-complete]]
         (ns-resolve 'hive-mcp.workflows.complete-session s))))

(use-fixtures :each
  (fn [f]
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f) (finally (mi/unstrument!)))))

(defn- boom [& _] (throw (ex-info "boom" {})))

(def ^:private State
  [:map
   [:commit-msg {:optional true} [:maybe [:enum "" "feat: x"]]]
   [:agent-id {:optional true} [:maybe [:enum "a"]]]
   [:error {:optional true} [:maybe [:enum "bad"]]]
   [:task-ids {:optional true} [:maybe [:vector {:gen/max 3} [:enum "t1" "t2"]]]]])

(deftrifecta-from-schema invalid-iff-error
  hive-mcp.workflows.complete-session/invalid?
  {:in State :out :boolean
   :rel (fn [d out] (= out (some? (:error d))))
   :num-tests 100})

(deftrifecta-from-schema no-tasks-iff-empty
  hive-mcp.workflows.complete-session/no-tasks?
  {:in State :out :boolean
   :rel (fn [d out] (and (= out (empty? (:task-ids d)))
                         (= out (not (cs/has-tasks? d)))))
   :num-tests 100})

(deftest valid-is-a-generalized-boolean
  (is (true? (cs/valid? {:commit-msg "m" :agent-id "a"})))
  (is (nil? (cs/valid? {:commit-msg "m"})) "a missing operand answers nil, not false")
  (is (false? (cs/valid? {:commit-msg "m" :agent-id "a" :error "e"}))))

(deftest start-arms
  (let [[ok-arm err-arm] (-> (m/function-schemas)
                             (get-in ['hive-mcp.workflows.complete-session 'handle-start :schema])
                             m/schema m/children second m/children)
        ok    (cs/handle-start {:agent-id "r" :directory "/d"} {:commit-msg "m" :task-ids ["t"]})
        miss  (cs/handle-start {} {})
        blank (cs/handle-start {} {:commit-msg "  "})]
    (is (= {:commit-msg "m" :task-ids ["t"] :agent-id "r" :directory "/d" :error nil} ok))
    (is (= "Missing required field: commit_msg" (:error miss)))
    (is (= "commit_msg cannot be empty" (:error blank)))
    (is (= ["t" "x"] (:task-ids (cs/handle-start {:merge-task-ids-fn (fn [ids _] (conj ids "x"))}
                                                 {:commit-msg "m" :task-ids ["t"]}))))
    (is (and (m/validate ok-arm ok) (not (m/validate err-arm ok))))
    (is (and (m/validate err-arm miss) (not (m/validate ok-arm miss))))))

(deftest side-effect-handlers
  (is (= {:skipped true} (:commit-result (cs/handle-commit {} {}))))
  (is (= ["m" "/d"] (:commit-result (cs/handle-commit {:git-commit-fn vector} {:commit-msg "m" :directory "/d"}))))
  (is (= {:moved 0 :skipped true} (:kanban-result (cs/handle-kanban {:kanban-done-fn boom} {}))))
  (is (= {:moved 1} (:kanban-result (cs/handle-kanban {:kanban-done-fn (constantly {:moved 1})}
                                                      {:task-ids ["t"]}))))
  (is (= {:skipped true} (:crystal-result (cs/handle-crystallize {} {}))))
  (let [seen (atom nil)]
    (is (true? (:shout-sent? (cs/handle-shout {:shout-fn (fn [& a] (reset! seen a))}
                                              {:agent-id "a" :commit-msg "m"}))))
    (is (= ["a" :completed "Session complete: m"] @seen)))
  (is (nil? (:plan-result (cs/handle-plan-check {} {}))))
  (is (= {:triggered? true} (:plan-result (cs/handle-plan-check {:plan-check-fn (constantly {:triggered? true})} {}))))
  (is (thrown? clojure.lang.ExceptionInfo (cs/handle-evict {:evict-fn boom} {}))
      "complete-session's evict is fatal: the throw propagates"))

(defn- resources []
  {:git-commit-fn  (constantly {:success true})
   :kanban-done-fn (fn [ids _] {:moved (count ids)})
   :crystallize-fn (constantly {:summary-id "s"})
   :evict-fn       (constantly {:evicted 1})
   :agent-id       "ling-1"
   :directory      "/d"})

(deftest-golden complete-session-summary
  "test/golden/workflows/complete-session-summary.edn"
  {:with-tasks (cs/run-session-complete (resources) {:commit-msg "feat: x" :task-ids ["t1" "t2"]})
   :no-tasks   (cs/run-session-complete (resources) {:commit-msg "feat: y"})})

(deftest runners
  (is (m/validate wfs/CompiledFsm (cs/compile-complete)))
  (is (= :ok (:status (cs/run-complete (cs/compile-complete) (resources) {:commit-msg "m"}))))
  (testing "no commit message routes to the throwing error handler"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Session complete workflow error"
                          (cs/run-complete (cs/compile-complete) (resources)))))
  (is (thrown? Exception (cs/run-session-complete (resources) "opts"))
      "the contract rejects a non-map initial-data"))
