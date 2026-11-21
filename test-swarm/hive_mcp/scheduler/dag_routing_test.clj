(ns hive-mcp.scheduler.dag-routing-test
  "A DAG-dispatched card is routed exactly like a direct `agent spawn`: its
   tier / work class reach the :spawn/opts-overlay router as the spawn
   request (spawn-ctx :request), the key a direct spawn fills from its params.

   Everything is wired through ports, no with-redefs:
     kanban      a table-backed IKanbanRead/IKanbanWrite in hive-spi.kanban.registry
     KG          a fresh ephemeral store (:kg-conn isolation)
     dispatch    the real :task-dispatch chain, served by :local-headless
     router      a recording contributor under :spawn.opts-overlay/<key>
     backend     the recording stub headless backend in the headless registry"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.isolation-methods]
            [hive-mcp.scheduler.dag-waves :as dag]
            [hive-mcp.scheduler.task-dispatch :as td]
            [hive-mcp.test.stub.headless-backend :as hb]
            [hive-mcp.transport.registry :as transport]
            [hive-spi.kanban :as kanban]
            [hive-spi.kanban.registry :as kanban-port]
            [hive-test.isolation :as iso]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ---------------------------------------------------------------------------
;; Ports
;; ---------------------------------------------------------------------------

(def ^:private board (atom {}))

(defrecord TableKanban []
  kanban/IKanbanRead
  (list-tasks [_ {:keys [status]}]
    (vec (filter #(or (nil? status) (= status (:status %))) (vals @board))))
  (get-task [_ id] (get @board id))
  kanban/IKanbanWrite
  (transition! [_ {:keys [task-id new-status]}]
    (swap! board assoc-in [task-id :status] new-status)
    {:ok {:id task-id :status new-status}})
  (create-task! [_ _] {:err {:error :kanban/read-only :message "test double"}}))

(defn- with-kanban [f]
  (let [prior (into {} (for [p [:IKanbanRead :IKanbanWrite] :when (kanban-port/registered? p)]
                         [p (kanban-port/provider p)]))
        double (->TableKanban)]
    (try
      (kanban-port/register! :IKanbanRead double)
      (kanban-port/register! :IKanbanWrite double)
      (f)
      (finally
        (doseq [p [:IKanbanRead :IKanbanWrite]]
          (if-let [impl (get prior p)]
            (kanban-port/register! p impl)
            (kanban-port/unregister! p)))))))

(def ^:private router-key :spawn.opts-overlay/dag-routing-test)

(def ^:private requests
  "Every spawn request the router saw, oldest first."
  (atom []))

(defn- recording-router
  "A :spawn/opts-overlay contributor that reads the request the way hive-agent's
   router does (spawn-ctx :request) and routes tier :cheap to a fixed member."
  [opts spawn-ctx]
  (let [req (:request spawn-ctx)]
    (swap! requests conj req)
    (if (= :cheap (:tier req))
      (assoc opts :model "deepseek-v4-flash" :provider "venice"
             :router/choice {:pool :cheap :work-class :grunt})
      opts)))

(defn- with-router [f]
  (let [prior (ext/get-extension router-key)]
    (reset! requests [])
    (ext/register! router-key recording-router)
    (try (f)
         (finally
           (ext/deregister! router-key)
           (when prior (ext/register! router-key prior))))))

(defn- with-fresh-dispatch [f]
  (transport/clear!)
  (reset! dag/dag-state {:active true :plan-id "plan-r" :max-slots 5 :wave-log []
                         :dispatched {} :completed #{} :failed #{} :opts {}})
  (try (f)
       (finally
         (transport/clear!)
         (reset! dag/dag-state {:active false :plan-id nil :max-slots 5 :wave-log []
                                :dispatched {} :completed #{} :failed #{} :opts {}}))))

(use-fixtures :each
  (iso/with-isolations :swarm-ds :kg-conn)
  with-kanban with-router with-fresh-dispatch)

(def ^:private backend-id :test-headless-dag-routing)

(defn- run-wave!
  "Put CARDS on the board, find the ready ones and dispatch one wave through
   the real chain onto a recording stub backend. Answers
   {:ready :result :spawns [[ctx opts] ...]}."
  [cards]
  (reset! board (into {} (map (juxt :id identity)) cards))
  (let [backend (hb/->backend backend-id)]
    (hb/with-backend backend-id backend
      (let [ready  (dag/find-ready-tasks "/tmp/dag-routing" #{} {} #{})
            result (dag/dispatch-wave! ready 5 {:cwd "/tmp/dag-routing"
                                                :project-id "dag-routing"
                                                :max-budget-usd 0})]
        {:ready ready :result result :spawns (hb/calls-of backend :spawn!)}))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest a-dag-card-tagged-tier-cheap-reaches-the-router-as-tier-cheap
  (let [{:keys [ready result spawns]}
        (run-wave! [{:id "card-cheap" :title "Cheap work" :status "todo"
                     :tags ["kanban" "todo" "tier:cheap"]}])]
    (testing "the ready task carries the card's routing"
      (is (= {:tier :cheap} (:routing (first ready)))))
    (testing "the router saw the same request a direct `agent spawn tier=cheap` gives it"
      (is (= 1 (:dispatched-count result)) (pr-str result))
      (is (= [{:tier :cheap}] @requests)))
    (testing "the backend ran the routed member, not the default"
      (is (= 1 (count spawns)))
      (let [[ctx opts] (first spawns)]
        (is (= "deepseek-v4-flash" (:model ctx)))
        (is (= "deepseek-v4-flash" (:model opts)))
        (is (= "venice" (:provider opts)))))))

(deftest a-plan-step-execution-tier-and-work-class-route-too
  (let [{:keys [result]}
        (run-wave! [{:id "card-step" :title "Step" :status "todo"
                     :tags ["kanban" "todo" "role:reviewer"]
                     :context {:plan-step-id "step-1"
                               :execution {:tier "cheap" :work-class "review"}}}])]
    (is (= 1 (:dispatched-count result)) (pr-str result))
    (is (= [{:role "reviewer" :tier :cheap :work-class :review}] @requests))))

(deftest a-card-naming-no-routing-spawns-unrouted
  (let [{:keys [ready result spawns]}
        (run-wave! [{:id "card-plain" :title "Plain" :status "todo"
                     :tags ["kanban" "todo"]}])]
    (is (nil? (:routing (first ready))))
    (is (= 1 (:dispatched-count result)) (pr-str result))
    (is (= [nil] @requests) "no :spawn/request: the router has no opinion")
    (is (not= "deepseek-v4-flash" (:model (second (first spawns)))))))

(deftest task-routing-reads-task-then-tags-then-plan-step
  (is (= {} (dag/task-routing nil)))
  (is (= {} (dag/task-routing {:tags ["tier: " "work-class:"]})) "blank values name nothing")
  (is (= {:tier :cheap} (dag/task-routing {:tags ["tier:cheap"]})))
  (is (= {:tier :frontier :work-class :grunt}
         (dag/task-routing {:tier "frontier" :tags ["tier:cheap" "work-class:grunt"]}))
      "an explicit key wins over its tag")
  (is (= {:tier :cheap}
         (dag/task-routing {:tags ["tier:cheap"] :context {:execution {:tier "frontier"}}}))
      "a tag wins over the plan step"))

(deftest create-ling-opts-carries-tier-and-work-class-in-the-spawn-request
  (is (= {:role "r" :tier :cheap :work-class :grunt}
         (:spawn/request (td/create-ling-opts {:task-id "c" :prompt "p" :role "r"
                                               :tier :cheap :work-class :grunt}
                                              {:max-budget-usd 0}))))
  (is (not (contains? (td/create-ling-opts {:task-id "c" :prompt "p"} nil) :spawn/request))))
