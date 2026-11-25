(ns hive-mcp.tools.kanban.diff-test
  "RUN-LEDGER-5 `kanban diff`: verdict flips between two runs.

   The pure core (`hive-mcp.tools.kanban.diff`) is held to schema-synthesized
   suites over a run-ledger-shaped board; the boundary is driven through the
   consolidated `kanban` handler with the board injected via `*board-source*`
   and the memory port stub. No concrete namespace is redefined."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.tools.consolidated.kanban :as kanban-tool]
            [hive-mcp.tools.consolidated.multi :as multi-tool]
            [hive-mcp.tools.consolidated.project :as project-tool]
            [hive-mcp.tools.kanban.diff :as kdiff]
            [hive-mcp.tools.kanban.diff.schema :as s]
            [hive-mcp.tools.kanban.list.source :as src]
            [hive-mcp.tools.memory-kanban.query :as query]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Generators: a run-ledger board (two runs, a few scenarios and personas)
;; =============================================================================

(def ^:private LedgerTag
  [:enum "kanban" "done" "todo" "run:A" "run:B" "scenario:s1" "scenario:s2"
   "persona:p1" "persona:p2" "verdict:pass" "verdict:fail-product"])

(def ^:private LedgerCard
  "The structured shape a ledger card is generated from: id, run, scenario,
   persona, verdict (or none), and whether it is a kanban card at all. Built
   this way so the runs share keys and hold verdicts often enough to reach
   every class; independent random tags almost never do."
  [:tuple [:enum "c1" "c2" "c3" "c4" "c5" "c6" "c7" "c8"]
   [:enum "run:A" "run:B"]
   [:enum "scenario:s1" "scenario:s2" "scenario:s2" nil]
   [:enum "persona:p1" "persona:p1" nil]
   [:enum "verdict:pass" "verdict:fail-product" "verdict:pass" "verdict:fail-product" nil]
   [:enum true true true false]])

(defn- ledger-card [[id run scenario persona verdict kanban?]]
  {:id      id
   :tags    (into (if kanban? ["kanban" "done"] ["todo"])
                  (remove nil?) [run scenario persona verdict])
   :content {:task-type "kanban"}})

(def ^:private LedgerEntry
  [:map {:closed true :gen/schema LedgerCard :gen/fmap ledger-card}
   [:id [:enum "c1" "c2" "c3" "c4" "c5" "c6" "c7" "c8"]]
   [:tags [:vector {:max 6} LedgerTag]]
   [:content [:map {:closed true} [:task-type [:= "kanban"]]]]])

(defn- distinct-ids
  "A board as a store answers it: one row per id."
  [entries]
  (vec (vals (into (sorted-map) (map (juxt :id identity)) entries))))

(def ^:private Verdicts [:enum "verdict:pass" "verdict:fail-product" nil])

(def ^:private Step
  "One (scenario x persona) step as both runs ran it: the key, each run's
   verdict, and whether each run holds the step at all."
  [:tuple [:enum "scenario:s1" "scenario:s2" nil] [:enum "persona:p1" "persona:p2" nil]
   Verdicts Verdicts [:enum true true true false] [:enum true true true false]])

(defn- steps->board
  "Both runs' cards of a list of steps, ids unique."
  [steps]
  (distinct-ids
   (into [] (comp (map-indexed
                   (fn [i [scenario persona va vb in-a? in-b?]]
                     (cond-> []
                       in-a? (conj (ledger-card [(str "a" i) "run:A" scenario persona va true]))
                       in-b? (conj (ledger-card [(str "b" i) "run:B" scenario persona vb true])))))
                  cat)
         steps)))

(def ^:private LedgerBoard
  [:vector {:max 16 :gen/schema [:vector {:min 1 :max 4} Step] :gen/fmap steps->board}
   [:map
    [:id :string]
    [:tags [:vector LedgerTag]]
    [:content [:map [:task-type [:= "kanban"]]]]]])

(def ^:private RunRequest
  [:map {:closed true}
   [:tags-a [:= ["run:A"]]]
   [:tags-b [:= ["run:B"]]]])

(defn- side-ids
  "Ids of the board's kanban cards carrying every tag."
  [board tags]
  (set (keep (fn [{:keys [id] :as e}]
               (when (every? (set (:tags e)) (cons "kanban" tags)) id))
             board)))

;; =============================================================================
;; diff : DiffRequest x board-a x board-b -> DiffReport
;; =============================================================================

(deftrifecta-from-schema diff-trifecta
  hive-mcp.tools.kanban.diff/diff
  ;; Both sides are generated from ONE mixed board, as the store holds both
  ;; runs: each side keeps its own run's cards, so keys are shared often.
  {:in  [:cat {:gen/schema [:tuple RunRequest LedgerBoard]
               :gen/fmap   (fn [[req board]] [req board board])}
         RunRequest LedgerBoard LedgerBoard]
   :out s/DiffReport
   :rel (fn [[{:keys [tags-a tags-b]} board-a board-b] {:keys [summary rows]}]
          (let [ids-a (mapcat :ids_a rows)
                ids-b (mapcat :ids_b rows)]
            (and
             ;; every card of each run is in exactly one row, on its side
             (= (sort ids-a) (sort (side-ids board-a tags-a)))
             (= (sort ids-b) (sort (side-ids board-b tags-b)))
             (= (count ids-a) (count (set ids-a)))
             ;; one row per key
             (= (count rows) (count (set (map :key rows))))
             ;; the summary counts the rows
             (= (:total summary) (count rows))
             (every? (fn [c] (= (get summary (keyword c))
                                (count (filter #(= c (:class %)) rows))))
                     s/classes)
             ;; each class follows from the two sides
             (every? (fn [{:keys [class verdict_a verdict_b ids_a ids_b]}]
                       (case class
                         "only-a"      (and (seq ids_a) (empty? ids_b))
                         "only-b"      (and (empty? ids_a) (seq ids_b))
                         "unverdicted" (and (seq ids_a) (seq ids_b)
                                            (or (nil? verdict_a) (nil? verdict_b)))
                         "same"        (and verdict_a (= verdict_a verdict_b))
                         "flipped"     (and verdict_a verdict_b (not= verdict_a verdict_b))))
                     rows))))
   :classify (fn [_ {:keys [summary]}]
               (if (pos? (:flipped summary)) :has-flip :no-flip))
   :classify-domain #{:has-flip :no-flip}
   :classify-floor  20
   :num-tests 200})

;; =============================================================================
;; classify : MaybeCell x MaybeCell -> DiffClass
;; =============================================================================

(deftrifecta-from-schema classify-trifecta
  hive-mcp.tools.kanban.diff/classify
  {:in  [:cat s/MaybeCell s/MaybeCell]
   :out s/DiffClass
   :rel (fn [[a b] c]
          (cond (and a (nil? b)) (= c "only-a")
                (and b (nil? a)) (= c "only-b")
                (and a b (:verdict a) (:verdict b))
                (= c (if (= (:verdict a) (:verdict b)) "same" "flipped"))
                :else (= c "unverdicted")))
   :mutation  false
   :num-tests 150})

;; =============================================================================
;; card-key : the matching key across runs
;; =============================================================================

(deftrifecta-from-schema card-key-trifecta
  hive-mcp.tools.kanban.diff/card-key
  {:in  LedgerEntry
   :out s/CardKey
   :rel (fn [{:keys [id tags]} {:keys [key scenario persona]}]
          (and (= scenario (some #(when (= "scenario:" (subs % 0 (min 9 (count %)))) (subs % 9)) tags))
               (if (or scenario persona)
                 (not= key (str "id:" id))
                 (= key (str "id:" id)))))
   :num-tests 150})

;; =============================================================================
;; request : boundary coercion
;; =============================================================================

(deftest request-coerces-and-refuses
  (is (= {:ok {:tags-a ["run:R3"] :tags-b ["run:R4"]}}
         (kdiff/request {:tags_a ["run:R3"] :tags_b ["run:R4"]})))
  (testing "a single tag string is one tag"
    (is (= {:ok {:tags-a ["run:R3"] :tags-b ["run:R4"]}}
           (kdiff/request {:tags_a "run:R3" :tags_b "run:R4"}))))
  (doseq [params [{} {:tags_a ["run:R3"]} {:tags_b ["run:R4"]}
                  {:tags_a [] :tags_b ["run:R4"]} {:tags_a 42 :tags_b ["x"]}]]
    (let [res (kdiff/request params)]
      (is (string? (:error res)) (pr-str params))
      (is (m/validate s/RequestResult res)))))

;; =============================================================================
;; A worked board: every class, through the MCP boundary
;; =============================================================================

(def ^:private pid "kanban-diff")

(defn- card [id status & tags]
  {:id         id
   :type       "note"
   :project-id pid
   :tags       (into ["kanban" status "priority-medium" (str "scope:project:" pid)] tags)
   :content    {:task-type "kanban" :title (str "card " id) :description "d"
                :status status :priority "medium"}})

(def ^:private board
  [(card "a1" "done"  "run:R3" "scenario:S1" "persona:viuva" "verdict:pass")
   (card "b1" "done"  "run:R4" "scenario:S1" "persona:viuva" "verdict:fail-product")
   (card "a2" "done"  "run:R3" "scenario:S2" "persona:viuva" "verdict:pass")
   (card "b2" "done"  "run:R4" "scenario:S2" "persona:viuva" "verdict:pass")
   (card "a3" "done"  "run:R3" "scenario:S3" "persona:filho" "verdict:pass")
   (card "b4" "todo"  "run:R4" "scenario:S4" "persona:filho")
   (card "a5" "done"  "run:R3" "scenario:S5" "persona:filho" "verdict:blocked-safety")
   (card "b5" "doing" "run:R4" "scenario:S5" "persona:filho")
   (card "x"  "done"  "run:R9" "scenario:S1" "persona:viuva" "verdict:pass")])

(use-fixtures :each
  (fn [f] (ms/with-stub-store (fn [] (ms/install! (ms/->stub board)) (f)))))

(defn- run-diff [params]
  (binding [query/*board-source* (src/->seq-source board)]
    (kanban-tool/handle-kanban (merge {:command "diff" :project_id pid :scope "all"} params))))

(deftest diff-classes-every-key-across-all-statuses
  (let [res    (run-diff {:tags_a ["run:R3"] :tags_b ["run:R4"]})
        report (json/read-str (:text res) :key-fn keyword)
        by-key (into {} (map (juxt :key identity)) (:rows report))]
    (is (not (:isError res)) (pr-str res))
    (is (= {:flipped 1 :same 1 :only-a 1 :only-b 1 :unverdicted 1 :total 5}
           (:summary report)))
    (is (= ["flipped" "only-a" "only-b" "unverdicted" "same"] (mapv :class (:rows report)))
        "rows ordered by class")
    (is (= {:key "scenario:S1 persona:viuva" :scenario "S1" :persona "viuva"
            :class "flipped" :verdict_a "pass" :verdict_b "fail-product"
            :ids_a ["a1"] :ids_b ["b1"]}
           (by-key "scenario:S1 persona:viuva")))
    (is (= "same" (:class (by-key "scenario:S2 persona:viuva"))))
    (is (= ["a3"] (:ids_a (by-key "scenario:S3 persona:filho"))))
    (is (= [] (:ids_b (by-key "scenario:S3 persona:filho"))))
    (is (= ["b4"] (:ids_b (by-key "scenario:S4 persona:filho"))) "a todo card is read")
    (is (= {:class "unverdicted" :verdict_a "blocked-safety" :verdict_b nil}
           (select-keys (by-key "scenario:S5 persona:filho") [:class :verdict_a :verdict_b]))
        "an in-progress card is read")
    (testing "a card of another run is never counted"
      (is (not-any? #(some #{"x"} (concat (:ids_a %) (:ids_b %))) (:rows report))))
    (is (m/validate s/DiffReport (:ok (binding [query/*board-source* (src/->seq-source board)]
                                        (query/diff-data {:project_id pid :scope "all"
                                                          :tags_a ["run:R3"] :tags_b ["run:R4"]})))))))

(deftest diff-refuses-a-missing-side
  (let [res (run-diff {:tags_a ["run:R3"]})]
    (is (:isError res))
    (is (re-find #"tags_b" (:text res)))))

(deftest diff-is-advertised
  (is (some #{"diff"} (get-in kanban-tool/tool-def [:inputSchema :properties "command" :enum])))
  (is (re-find #"diff \(" (:description kanban-tool/tool-def)))
  (doseq [[tool-name props] [["kanban" (get-in kanban-tool/tool-def [:inputSchema :properties])]
                             ["project" (get-in project-tool/tool-def [:inputSchema :properties])]
                             ["multi" (get-in multi-tool/tool-def [:inputSchema :properties])]]
          k ["tags_a" "tags_b"]]
    (testing (str tool-name " " k)
      (is (= "array" (get-in props [k :type])))
      (is (re-find #"diff" (get-in props [k :description]))))))
