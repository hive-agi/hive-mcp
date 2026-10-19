(ns hive-mcp.scheduler.dag-eligibility-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.scheduler.dag-eligibility :as elig]))

(deftest human-only-cards
  (testing "titles"
    (is (elig/human-only? {:title "PEDRO DECISION: visual identity"}))
    (is (elig/human-only? {:title "[X] pedro decision - pricing"}))
    (is (elig/human-only? {:title "HUMAN ONLY: sign the contract"}))
    (is (not (elig/human-only? {:title "Decide the cache key layout"})))
    (is (not (elig/human-only? {:title nil}))))
  (testing "tags"
    (doseq [t ["human-only" "needs-human" "needs-pedro" "decision"]]
      (is (elig/human-only? {:title "x" :tags [t]}) t))
    (is (not (elig/human-only? {:title "x" :tags ["wave:0" "priority-high"]})))))

(deftest waves
  (is (= 0 (elig/task-wave {:tags []})))
  (is (= 3 (elig/task-wave {:tags ["kanban" "wave:3"]})))
  (let [todos [{:id "a" :tags ["wave:0"]} {:id "b" :tags ["wave:1"]} {:id "c" :tags ["wave:1"]}]]
    (is (= ["a"] (map :id (elig/lowest-open-wave #{} todos))))
    (is (= ["b" "c"] (map :id (elig/lowest-open-wave #{"a"} todos))))
    (is (= [] (elig/lowest-open-wave #{"a" "b" "c"} todos)))))

(deftest eligible-filters-humans-and-orders-bounded-runs
  (let [todos [{:id "h" :title "PEDRO DECISION: logo" :tags ["wave:0"]}
               {:id "a" :title "build" :tags ["wave:0"]}
               {:id "b" :title "ship" :tags ["wave:1"]}]]
    (is (= ["a"] (map :id (elig/eligible true #{} todos))))
    (is (= ["a" "b"] (map :id (elig/eligible false #{} todos))))))

(deftest run-scope
  (is (= #{"x"} (elig/run-scope "p" {:task-ids ["x"]} #{"t"})) "explicit ids win")
  (is (nil? (elig/run-scope "p" {:board-wide? true} #{"t"})) "board-wide is explicit")
  (is (= #{"t"} (elig/run-scope "p" {} #{"t"})) "the plan's own cards")
  (is (nil? (elig/run-scope "p" {} #{})) "legacy: unlinked plan without the requirement")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no kanban cards linked"
                        (elig/run-scope "p" {:require-plan-scope? true} #{}))))
