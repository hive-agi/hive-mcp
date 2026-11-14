(ns hive-mcp.plan.conversion-lifecycle-test
  "Lifecycle of plan-to-kanban over a plan MEMORY entry: the type guard, the
   double-convert noop, force, and the state a conversion leaves behind
   (status active, task-ids + waves persisted, plan re-indexed).

   Collaborators are stubbed, never reached: the entry store (facade), the
   plans index, the KG edge layer, and the kanban write port. The schema,
   parser, FSM and wave computation run for real, so a passing test also
   proves the conversion itself still works underneath the guards."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-mcp.plan.tool :as sut]
            [hive-spi.kanban :as kanban]
            [hive-spi.kanban.registry :as kanban-registry]
            [hive-mcp.vectordb.facade]
            [hive-mcp.plan.plans]
            [hive-mcp.knowledge-graph.edges]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Doubles
;;; =============================================================================

(def ^:private created
  "Kanban task requests the conversion made, in order."
  (atom []))

(defn- entry
  "A stored plan entry. STATUS nil leaves the key off, as a pre-status entry."
  ([id] (entry id nil))
  ([id status]
   (cond-> {:id id
            :type "plan"
            :content "{:id \"p\" :title \"P\" :steps [{:id \"a\" :title \"A\"}]}"
            :project-id "proj"}
     status (assoc :plan-status status))))

(defn- port-fixture
  "Register a recording kanban write port, then restore whatever was there."
  [f]
  (let [prior (kanban-registry/registered? :IKanbanWrite)]
    (reset! created [])
    (kanban-registry/register!
     :IKanbanWrite
     (reify kanban/IKanbanWrite
       (create-task! [_ _request]
         (swap! created conj true)
         {:ok {:id (str "task-" (count @created))}})
       (transition! [_ _request] {:ok {}})))
    (try (f)
         (finally
           (reset! created [])
           (if prior
             (kanban-registry/register! :IKanbanWrite prior)
             (kanban-registry/unregister! :IKanbanWrite))))))

(use-fixtures :each port-fixture)

(defn- with-world
  "Run F with facade/plans/kg stubbed around ENTRIES (id -> entry).

   Writes are recorded into the log atom the stubs hand back, as
   {:updates [[id fields]] :indexed [entry]}, and applied to ENTRIES so a
   second call reads the persisted state."
  [entries f]
  (let [log (atom {:updates [] :indexed []})
        update-entry! (fn [id updates]
                        (swap! log update :updates conj [id updates])
                        (swap! entries update id merge updates)
                        (get @entries id))]
    (with-redefs [hive-mcp.vectordb.facade/get-entry-by-id #(get @entries %)
                  hive-mcp.vectordb.facade/update-entry! update-entry!
                  hive-mcp.plan.plans/update-plan-status!
                  (fn [id status]
                    (swap! log update :updates conj [id {:plan-status status}])
                    (swap! entries update id assoc :plan-status status)
                    id)
                  hive-mcp.plan.plans/index-plan!
                  (fn [e] (swap! log update :indexed conj e) (:id e))
                  hive-mcp.knowledge-graph.edges/add-edges! (constantly [])
                  hive-mcp.knowledge-graph.edges/add-edge! (constantly nil)]
      (f log))))

(defn- body
  "Decode an MCP response body."
  [resp]
  (json/read-str (:text resp) :key-fn keyword))

(def ^:private dir "/tmp/plan-lifecycle")

;;; =============================================================================
;;; Type guard
;;; =============================================================================

(deftest rejects-a-non-plan-entry-test
  (testing "a decision entry is refused, and nothing is created"
    (let [entries (atom {"dec-1" {:id "dec-1" :type "decision"
                                  :content "a decision, not a plan"}})]
      (with-world entries
        (fn [_log]
          (let [resp (sut/plan-to-kanban "dec-1" :directory dir)]
            (is (true? (:isError resp)))
            (is (= [] @created) "no kanban task may be created for a decision")))))))

;;; =============================================================================
;;; Double convert
;;; =============================================================================

(deftest second-convert-of-an-active-plan-is-a-noop-test
  (testing "an active plan that already has task ids answers already-converted"
    (let [entries (atom {"p1" (assoc (entry "p1" "active")
                                     :task-ids ["task-9"] :waves {"a" 0})})]
      (with-world entries
        (fn [log]
          (let [resp (sut/plan-to-kanban "p1" :directory dir)
                b (body resp)]
            (is (false? (boolean (:isError resp))))
            (is (true? (:already-converted b)))
            (is (= ["task-9"] (:task-ids b)) "the ids it already has are returned")
            (is (= {:a 0} (:waves b)))
            (is (= [] @created) "no duplicate board")
            (is (= [] (:updates @log)) "the entry is untouched")
            (is (= [] (:indexed @log)))))))))

(deftest force-converts-an-active-plan-again-test
  (testing "force=true re-runs an already-converted plan"
    (let [entries (atom {"p1" (assoc (entry "p1" "active") :task-ids ["task-9"])})]
      (with-world entries
        (fn [_log]
          (let [b (body (sut/plan-to-kanban "p1" :directory dir :force true))]
            (is (true? (:success b)))
            (is (nil? (:already-converted b)))
            (is (= 1 (count @created)) "a fresh task for the single step")
            (is (= ["task-1"] (:task-ids b)))))))))

;;; =============================================================================
;;; Spent plans
;;; =============================================================================

(deftest spent-plans-refuse-without-force-test
  (doseq [status ["completed" "superseded"]]
    (testing (str "a " status " plan refuses without force")
      (let [entries (atom {"p1" (entry "p1" status)})]
        (with-world entries
          (fn [_log]
            (let [resp (sut/plan-to-kanban "p1" :directory dir)]
              (is (true? (:isError resp)))
              (is (some? (re-find (re-pattern status) (:text resp))))
              (is (= [] @created)))))))))

(deftest force-converts-a-spent-plan-test
  (testing "force=true converts a completed plan"
    (let [entries (atom {"p1" (entry "p1" "completed")})]
      (with-world entries
        (fn [_log]
          (is (true? (:success (body (sut/plan-to-kanban "p1" :directory dir :force true)))))
          (is (= 1 (count @created))))))))

;;; =============================================================================
;;; What a conversion leaves behind
;;; =============================================================================

(deftest conversion-marks-the-plan-active-and-persists-the-result-test
  (testing "status goes active BEFORE the tasks and the ids are recorded after"
    (let [entries (atom {"p1" (entry "p1" "draft")})]
      (with-world entries
        (fn [log]
          (let [b (body (sut/plan-to-kanban "p1" :directory dir))
                updates (:updates @log)]
            (is (true? (:success b)))
            (testing "the first write is the status, ahead of any task"
              (is (= ["p1" {:plan-status "active"}] (first updates))))
            (testing "both stores get the status"
              (is (= 2 (count (filter #(= {:plan-status "active"} (second %)) updates)))))
            (testing "task-ids are a vector and waves are the step->wave map"
              (let [[id fields] (last updates)]
                (is (= "p1" id))
                (is (= ["task-1"] (:task-ids fields)))
                (is (vector? (:task-ids fields)))
                (is (= {"a" 0} (:waves fields)))
                (is (map? (:waves fields)))))
            (testing "the plan is re-indexed carrying the new status"
              (let [indexed (last (:indexed @log))]
                (is (= "p1" (:id indexed)))
                (is (= "active" (:plan-status indexed)))
                (is (= 1 (:steps-count indexed)))
                (is (= 1 (:wave-count indexed)))))
            (testing "the store now holds what the response reported"
              (is (= ["task-1"] (get-in @entries ["p1" :task-ids]))))))))))

(deftest a-converted-plan-is-a-noop-on-the-next-call-test
  (testing "the state persisted by one conversion suppresses the next"
    (let [entries (atom {"p1" (entry "p1" "draft")})]
      (with-world entries
        (fn [_log]
          (let [first-run (body (sut/plan-to-kanban "p1" :directory dir))
                second-run (body (sut/plan-to-kanban "p1" :directory dir))]
            (is (= 1 (count @created)))
            (is (= (:task-ids first-run) (:task-ids second-run)))
            (is (true? (:already-converted second-run)))))))))

(deftest a-plan-path-still-converts-without-an-entry-test
  (testing "the file route has no entry, so it every time and records nothing"
    (let [f (doto (java.io.File/createTempFile "plan-lifecycle" ".md") (.deleteOnExit))]
      (spit f "{:id \"p\" :title \"P\" :steps [{:id \"a\" :title \"A\"}]}")
      (let [entries (atom {})]
        (with-world entries
          (fn [log]
            (let [a (sut/plan-to-kanban nil :directory dir :plan-path (str f))
                  b (sut/plan-to-kanban nil :directory dir :plan-path (str f))]
              (is (false? (boolean (:isError a))))
              (is (false? (boolean (:isError b))) "no status to refuse on")
              (is (= 2 (count @created)))
              (is (= "file" (:plan-source (body b))))
              (is (= [] (:updates @log)))
              (is (= [] (:indexed @log))))))))))
