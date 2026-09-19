(ns hive-mcp.tools.consolidated.workflow.forge-strike-dependency-proof-test
  "Drives the REAL forge belt (forge-belt-defaults registered, run-single-strike
   not redefined) through successive strikes over a diamond plan: ka and kb are
   independent, kc depends on both, and an unrelated todo card sits on the same
   board. Selection comes from the real forge-ops/survey reached through the
   real build-fsm-resources kanban-ops; cleanup comes from the real
   forge-ops/smite! through its ports arity. Spawning is a recording port.

   Contract pinned here:
   1. strike over a fresh plan sparks exactly ka and kb, selection :ready
   2. with ka done and kb open, nothing new is sparked and kc is reported
      blocked on kb :open
   3. with kb done, exactly kc is sparked
   4. with kc done, the plan reports :complete
   and a scoped smite kills only slaves whose project-id AND kanban task belong
   to the plan; foreign slaves survive."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.scheduler.vulcan :as vulcan]
            [hive-mcp.tools.consolidated.kanban :as c-kanban]
            [hive-mcp.tools.consolidated.workflow.forge-ops :as forge-ops]
            [hive-mcp.tools.consolidated.workflow.forge-cycle :as cycle]
            [hive-mcp.workflows.forge-belt :as belt]
            [hive-mcp.workflows.forge-belt-defaults :as defaults]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- fb-key? [k] (= "fb" (namespace k)))

(defn- fb-registry-fixture
  "Snapshot every :fb/* registration, install the FOSS defaults for the test,
   and restore the snapshot exactly afterwards (the registry is process-global)."
  [f]
  (let [snapshot (into {} (keep (fn [k] (when (fb-key? k) [k (ext/get-extension k)])))
                       (ext/registered-keys))]
    (try
      (doseq [k (filter fb-key? (ext/registered-keys))] (ext/deregister! k))
      (defaults/register-forge-belt-defaults!)
      (f)
      (finally
        (doseq [k (filter fb-key? (ext/registered-keys))] (ext/deregister! k))
        (when (seq snapshot) (ext/register-many! snapshot))))))

(use-fixtures :each fb-registry-fixture)

(def ^:private project-id "proof-proj")
(def ^:private directory "/test/proof-proj")

(def ^:private plan
  {:id "p" :title "Diamond"
   :steps [{:id "a" :title "A"} {:id "b" :title "B"}
           {:id "c" :title "C" :depends-on ["a" "b"]}]})

(def ^:private graph
  {"p" #{"ka" "kb" "kc"} "ka" #{} "kb" #{} "kc" #{"ka" "kb"} "ku" #{}})

(defn- card [id title status]
  {:id id :content (json/write-str {:task-type "kanban" :title title :status status})})

(defn- status-of [entry]
  (:status (json/read-str (:content entry) :key-fn keyword)))

(defn- board
  "In-memory kanban: the plan's three cards plus one unrelated todo card."
  []
  (atom {"ka" (card "ka" "A" "todo")
         "kb" (card "kb" "B" "todo")
         "kc" (card "kc" "C" "todo")
         "ku" (card "ku" "Unrelated" "todo")}))

(defn- set-status! [db id status]
  (swap! db update id (fn [e] (card id (:title (json/read-str (:content e) :key-fn keyword))
                                    status))))

(defn- list-todo
  "Kanban list port answer: every todo card on the board, in MCP text shape."
  [db]
  {:text (json/write-str
          (->> @db
               (filter (fn [[_ e]] (= "todo" (status-of e))))
               (mapv (fn [[id e]]
                       {:id id :title (:title (json/read-str (:content e) :key-fn keyword))}))))})

(defn- survey-ports [db]
  {:plan-entry-fn (constantly {:type "plan" :content (pr-str plan)})
   :task-entry-fn (fn [id] (get @db id))
   :deps-fn graph
   :state-fn (fn [id] (vulcan/entry-state (get @db id)))})

(defn- slaves []
  [{:slave/id "s-plan-done" :slave/depth 1 :slave/status :completed
    :slave/project-id project-id :slave/kanban-task-id "ka"}
   {:slave/id "s-foreign-task" :slave/depth 1 :slave/status :completed
    :slave/project-id project-id :slave/kanban-task-id "ku"}
   {:slave/id "s-foreign-project" :slave/depth 1 :slave/status :completed
    :slave/project-id "other-proj" :slave/kanban-task-id "ka"}
   {:slave/id "s-plan-active" :slave/depth 1 :slave/status :active
    :slave/project-id project-id :slave/kanban-task-id "kb"}])

(defn- smite-ports [db killed]
  {:agents-fn slaves
   :kill-fn (fn [agent]
              (swap! killed conj (:slave/id agent))
              {:id (:slave/id agent) :killed true})
   :plan-ids-fn #(forge-ops/plan-task-ids % (survey-ports db))
   :project-id-fn (constantly project-id)})

(defn- resources
  "Real build-fsm-resources (real survey behind kanban-ops) with the agent-ops
   port replaced by recording stubs: spawn marks sparked cards doing, kill runs
   the real scoped smite! over stub slaves."
  [db sparks killed]
  (-> (cycle/build-fsm-resources (merge {:directory directory :plan_id "p"}
                                        (survey-ports db)))
      (assoc :scope-fn (constantly project-id))
      (assoc :agent-ops
             {:kill-fn (fn [dir _project-id]
                         (forge-ops/smite! {:directory dir :plan_id "p"}
                                           (smite-ports db killed)))
              :spawn-fn (fn [{:keys [tasks]}]
                          (let [ids (mapv :id tasks)]
                            (swap! sparks conj ids)
                            (doseq [id ids] (set-status! db id "doing"))
                            {:spawned (mapv #(hash-map :task-id %) ids)
                             :failed [] :count (count ids)}))
              :dispatch-fn (constantly true)
              :wait-ready-fn (constantly true)})))

(defn- strike! [db sparks killed]
  (with-redefs [c-kanban/handle-kanban (fn [_] (list-todo db))]
    (belt/run-single-strike (resources db sparks killed))))

(deftest the-real-belt-is-the-one-under-test
  (is (some? (ext/get-extension :fb/strike))
      "run-single-strike must delegate to the registered default, not the noop"))

(deftest strikes-release-the-diamond-in-dependency-order
  (let [db (board) sparks (atom []) killed (atom [])]
    (testing "1. fresh plan: exactly the independent roots are sparked"
      (let [r (strike! db sparks killed)
            survey (:survey-result r)]
        (is (= :ready (:selection-status survey)))
        (is (= [#{"ka" "kb"}] (mapv set @sparks)))
        (is (= 2 (get-in r [:spark-result :count])))
        (is (= "p" (:plan-id survey)))
        (is (= {"kc" {"ka" :open "kb" :open}}
               (into {} (map (juxt :task-id :dependencies)) (:blocked survey))))))

    (testing "2. ka done, kb still open: nothing new, kc blocked on kb"
      (set-status! db "ka" "done")
      (let [before (count @sparks)
            r (strike! db sparks killed)
            survey (:survey-result r)]
        (is (= before (count @sparks)) "no spark while a predecessor is open")
        (is (nil? (:spark-result r)))
        (is (= :blocked (:selection-status survey)))
        (is (= [{:task-id "kc" :dependencies {"kb" :open}}] (:blocked survey)))
        (is (= {"ka" :done "kb" :open "kc" :open} (:plan-states survey)))))

    (testing "3. kb done: exactly kc is sparked"
      (set-status! db "kb" "done")
      (let [r (strike! db sparks killed)]
        (is (= :ready (get-in r [:survey-result :selection-status])))
        (is (= ["kc"] (last @sparks)))
        (is (= 1 (get-in r [:spark-result :count])))))

    (testing "4. kc done: the plan reports complete and nothing is sparked"
      (set-status! db "kc" "done")
      (let [before (count @sparks)
            r (strike! db sparks killed)
            survey (:survey-result r)]
        (is (= before (count @sparks)))
        (is (= :complete (:selection-status survey)))
        (is (true? (:plan-complete? survey)))
        (is (= 3 (:plan-completed-count survey)))))

    (testing "the unrelated todo card is never sparked and never changes"
      (is (not-any? #(some #{"ku"} %) @sparks))
      (is (= "todo" (status-of (get @db "ku")))))

    (testing "sparked sets across all strikes are exactly the plan, each once"
      (is (= ["ka" "kb" "kc"] (sort (mapcat identity @sparks)))))))

(deftest scoped-smite-kills-only-plan-owned-slaves
  (testing "through the belt's smite phase"
    (let [db (board) sparks (atom []) killed (atom [])
          r (strike! db sparks killed)]
      (is (= ["s-plan-done"] @killed))
      (is (= ["s-plan-done"] (mapv :id (get-in r [:smite-result :smited]))))
      (is (= 1 (:total-smited r)))))
  (testing "directly through the ports arity"
    (let [db (board) killed (atom [])
          res (forge-ops/smite! {:directory directory :plan_id "p"} (smite-ports db killed))]
      (is (= ["s-plan-done"] @killed)
          "foreign task, foreign project and non-terminal slaves all survive")
      (is (= 1 (:count res)))))
  (testing "a task_ids whitelist outside the plan kills nothing"
    (let [db (board) killed (atom [])
          res (forge-ops/smite! {:directory directory :plan_id "p" :task_ids ["ku"]}
                                (smite-ports db killed))]
      (is (empty? @killed))
      (is (zero? (:count res))))))
