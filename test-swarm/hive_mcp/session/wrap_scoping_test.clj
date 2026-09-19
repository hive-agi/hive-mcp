(ns hive-mcp.session.wrap-scoping-test
  "Closes the residue of kanban 20260915164015-7e057e5b.

   The foundation (47ae7c85) built the ownership algebra and gave the registry
   scoped reads and scoped clears, but nothing USED them: rows were written
   with no :session-id, the harvest still read fleet-wide, and the wrap still
   ended with the unscoped 0-arity clears. Each of those alone is enough to
   reproduce the original defect, where the first wrap to finish on a box
   destroyed every other live session's unharvested records.

   These tests pin the wiring, not the algebra — identity's own rules are
   covered by hive-mcp.session.identity-test."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.crystal.hooks :as hooks]
            [hive-mcp.session.current :as current]
            [hive-mcp.session.identity :as sid]
            [hive-mcp.swarm.datascript.coordination.session-registry :as reg]
            [hive-mcp.swarm.datascript.coordination.wrap-queue :as wq]
            [hive-mcp.workflows.wrap-session :as wrap]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [t]
    (reg/clear-completed-tasks!)
    (reg/clear-kanban-movements!)
    (try (t)
         (finally
           (reg/clear-completed-tasks!)
           (reg/clear-kanban-movements!)))))

(defn- coord [id] (sid/session-ref {:id id :kind :coordinator :project-id "hive"}))

;; =============================================================================
;; session.current/parent-of — the map the ownership walk needs
;; =============================================================================

(deftest parent-of-projects-slave-rows-onto-session-ids
  (testing "a ling's session maps to its parent slave's session"
    (is (= {"sess-ling" "sess-coord"}
           (current/parent-of
            {:slaves {"ling-1" {:slave/id "ling-1"
                                :slave/session-id "sess-ling"
                                :slave/parent-id "coord-1"}
                      "coord-1" {:slave/id "coord-1"
                                 :slave/session-id "sess-coord"}}}))))

  (testing "rows that cannot name both ends are dropped, not guessed"
    (is (= {} (current/parent-of {:slaves {"a" {:slave/id "a"
                                                :slave/session-id "s-a"}}}))
        "no parent-id -> no entry")
    (is (= {} (current/parent-of {:slaves {"a" {:slave/id "a"
                                                :slave/parent-id "b"}
                                           "b" {:slave/id "b"}}}))
        "parent with no session-id -> no entry"))

  (testing "a self-parent is refused, or the ancestor walk would spin"
    (is (= {} (current/parent-of {:slaves {"a" {:slave/id "a"
                                                :slave/session-id "s"
                                                :slave/parent-id "a"}}})))))

;; =============================================================================
;; clear-harvested! — the pairing that makes a wrap safe
;; =============================================================================

(deftest clear-harvested-retracts-only-what-this-wrap-read
  (reg/register-completed-task! "t-mine"  {:title "mine"  :project-id "hive" :session-id "s-a"})
  (reg/register-completed-task! "t-yours" {:title "yours" :project-id "hive" :session-id "s-b"})

  (testing "only the harvested ids go"
    (let [out (hooks/clear-harvested! (coord "s-a")
                                      {:harvested-task-ids ["t-mine"]
                                       :harvested-movement-ids []})
          left (set (map :completed-task/id (reg/get-completed-tasks-this-session)))]
      (is (= 1 (:tasks out)))
      (is (= #{"t-yours"} left)
          "a concurrent session's unharvested row must survive the wrap"))))

(deftest clear-harvested-refuses-to-clear-an-unscoped-harvest
  (reg/register-completed-task! "t-1" {:title "t" :project-id "hive" :session-id "s-a"})

  (testing "no session-ref means the harvest read rows it does not own"
    (let [out (hooks/clear-harvested! nil {:harvested-task-ids ["t-1"]})]
      (is (= :unscoped (:skipped out)))
      (is (= 1 (count (reg/get-completed-tasks-this-session)))
          "clearing what an unscoped harvest read would delete another session's rows")))

  (testing "a ref with no id is the same case"
    (let [out (hooks/clear-harvested! {} {:harvested-task-ids ["t-1"]})]
      (is (= :unscoped (:skipped out))))))

;; =============================================================================
;; The FSM: harvest-fn receives the ref, adoption produces :contains
;; =============================================================================

(deftest gather-hands-the-session-ref-to-the-harvest
  (testing "harvest-fn is called with the opts map, ref included"
    (let [seen (atom nil)
          ref  (coord "s-a")
          out  (wrap/handle-gather
                {:harvest-fn (fn [opts] (reset! seen opts) {:progress-notes []})}
                {:directory "/tmp/x" :agent-id "a1"
                 :session-ref ref :parent-of {"c" "p"}})]
      (is (= ref (:session-ref @seen))
          "without this the harvest is fleet-wide and the wrap reports other sessions' work")
      (is (= {"c" "p"} (:parent-of @seen)))
      (is (= "/tmp/x" (:directory @seen)))
      (is (= {:progress-notes []} (:harvested out))))))

(deftest adopted-entry-ids-collects-what-the-absorbed-wraps-created
  (is (= ["m-1" "m-2" "m-3"]
         (wrap/adopted-entry-ids
          [{:wrap-queue/id "w-1" :wrap-queue/created-ids ["m-1" "m-2"]}
           {:wrap-queue/id "w-2" :wrap-queue/created-ids ["m-2" "m-3"]}
           {:wrap-queue/id "w-3"}]))
      "deduped, nils dropped, order kept")
  (is (= [] (wrap/adopted-entry-ids []))))

(deftest kg-edges-links-adopted-wraps-with-contains
  (let [contains-calls (atom [])
        resources {:kg-edge-fn (fn [& _] {:created-count 2})
                   :contains-edge-fn (fn [summary-id child-ids pid aid]
                                       (swap! contains-calls conj
                                              [summary-id child-ids pid aid])
                                       {:created-count (count child-ids)})}]

    (testing "a coordinator that adopted wraps nests them under its summary"
      (let [out (wrap/handle-kg-edges
                 resources
                 {:project-id "hive" :agent-id "coord-1"
                  :crystal-result {:summary-id "sum-1"}
                  :source-ids ["src-1"]
                  :adopted [{:wrap-queue/created-ids ["m-1" "m-2"]}]})]
        (is (= 2 (get-in out [:kg-result :created-count]))
            ":derived-from still runs")
        (is (= 2 (get-in out [:contains-result :created-count])))
        (is (= [["sum-1" ["m-1" "m-2"] "hive" "coord-1"]] @contains-calls))))

    (testing "a ling adopts nothing, so there is nothing to nest"
      (reset! contains-calls [])
      (let [out (wrap/handle-kg-edges
                 resources
                 {:project-id "hive" :agent-id "ling-1"
                  :crystal-result {:summary-id "sum-2"}
                  :source-ids ["src-1"]
                  :adopted []})]
        (is (true? (get-in out [:contains-result :skipped])))
        (is (= [] @contains-calls))))

    (testing "a :contains failure must not cost the :derived-from edges"
      (let [out (wrap/handle-kg-edges
                 (assoc resources :contains-edge-fn
                        (fn [& _] (throw (ex-info "contains boom" {}))))
                 {:project-id "hive" :agent-id "coord-1"
                  :crystal-result {:summary-id "sum-3"}
                  :source-ids ["src-1"]
                  :adopted [{:wrap-queue/created-ids ["m-9"]}]})]
        (is (= 2 (get-in out [:kg-result :created-count]))
            "the leg that makes the summary reachable survives")
        (is (true? (get-in out [:contains-result :degraded])))))))

;; =============================================================================
;; A ling's wrap-queue row has to say where it permeates TO
;; =============================================================================

(deftest wrap-queue-row-carries-parent-session-so-a-coordinator-can-find-it
  (let [wid (str "w-" (System/nanoTime))]
    (wq/add-wrap-notification! wid {:agent-id "ling-1"
                                    :session-id "s-ling"
                                    :project-id "hive"
                                    :parent-session-id "s-coord"
                                    :depth 1
                                    :created-ids ["m-1"]})
    (testing "the coordinator selects it by parent-session-id, not by project"
      (let [mine (wq/get-unprocessed-wraps-for-session "s-coord")]
        (is (= 1 (count (filter #(= wid (:wrap-queue/id %)) mine)))))
      (is (empty? (filter #(= wid (:wrap-queue/id %))
                          (wq/get-unprocessed-wraps-for-session "s-other")))
          "another coordinator in the same project must not see it"))
    (wq/mark-wrap-processed! wid)))
