(ns hive-mcp.scheduler.dag-eligibility-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.scheduler.dag-eligibility :as elig]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

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

;;; ---------------------------------------------------------------------------
;;; Facets. Titles are drawn from the separators the title regex allows
;;; (space, underscore, hyphen, any case) and from near-misses it must refuse
;;; (no word boundary, a different word), so both arms are reached. Tags mix
;;; vectors and sets, and include a keyword tag: tags are read through `str`,
;;; so `:human-only` renders as ":human-only" and reserves nothing.
;;; ---------------------------------------------------------------------------

(def ^:private Card
  [:map
   [:id [:enum "a" "b" "c" "d"]]
   [:title {:optional true}
    [:enum "pedro_decision: logo" "Human-Only review" "HUMAN DECISION x" "humanonly"
     "xPEDRO DECISION" "decide later" "build" :kw]]
   [:tags {:optional true}
    [:or [:vector {:max 3} [:enum "wave:0" "wave:2" "wave:x" "needs-pedro" "pedro-decision"
                            :human-only "kanban"]]
     [:set [:enum "wave:1" "kanban" "needs-human"]]]]])

(defn- human-oracle
  "Independent reading of the ns docstring: a title naming a Pedro / human
   decision (or human only) as whole words, or a reserved tag string."
  [{:keys [title tags]}]
  (boolean (or (contains? #{"pedro_decision: logo" "Human-Only review" "HUMAN DECISION x"} title)
               (some #(contains? elig/human-only-tags (str %)) tags))))

(deftrifecta-from-schema human-only?-honours-separators-and-word-bounds
  #'elig/human-only?
  {:in [:cat Card]
   :out :boolean
   :rel (fn [[c] out] (= out (human-oracle c)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 300})

(deftrifecta-from-schema eligible-unbounded-is-machine-cards-in-order
  #'elig/eligible
  {:in [:cat [:enum false] [:set [:enum "a"]] [:vector {:max 5} Card]]
   :out [:vector elig/Task]
   :rel (fn [[_ _ todos] out] (= out (vec (remove human-oracle todos))))
   :golden-path "test/golden/hive-mcp/scheduler/eligible-unbounded.edn"
   :num-tests 200})
