(ns hive-mcp.tools.kanban.port-test
  "Core's kanban provider behind the kanban ports, driven over the
   atom-backed memory store: it passes the same conformance cases the
   recording stub passes, speaks the public status vocabulary, and is
   reached through the registry rather than a captured value."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [malli.core :as m]
            [hive-mcp.isolation-methods]
            [hive-mcp.spi.kanban :as kanban]
            [hive-mcp.spi.kanban.registry :as kanban-port]
            [hive-mcp.test.stub.kanban :as kport]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-spi.memory.registry :as sreg]
            [hive-test.isolation :as iso]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  mem-stub/with-stub-store
  kport/with-core-kanban
  (iso/with-isolations :kg-conn))

(def ^:private pid "kanban-port-test")

(defn- entry [id title status priority]
  {:id         id
   :type       "note"
   :content    {:task-type "kanban" :title title :status status :priority priority
                :description (str "about " title)}
   :tags       ["kanban" status (str "priority-" priority) (str "scope:project:" pid)]
   :project-id pid})

(defn- seed-board! []
  (mem-stub/seed! (sreg/get-store)
                  [(entry "kp-1" "Setup" "todo" "high")
                   (entry "kp-2" "Ship" "doing" "medium")
                   (entry "kp-3" "Review" "review" "low")]))

(deftest core-provider-passes-the-contract-conformance
  (seed-board!)
  (let [impl   (kanban-port/provider :IKanbanRead)
        report (kanban/conformance impl impl {:directory "/tmp/kanban-port-test"})]
    (is (every? :ok? report) (pr-str (remove :ok? report)))))

(deftest list-tasks-speaks-the-public-vocabulary
  (seed-board!)
  (testing "a public status filter reaches the stored tag and comes back public"
    (is (= [{:id "kp-2" :status "inprogress"}]
           (mapv #(select-keys % [:id :status])
                 (kanban-port/list-tasks {:status "inprogress" :project-id pid})))))
  (testing "the whole scoped board, every row a valid Task"
    (let [rows (kanban-port/list-tasks {:project-id pid})]
      (is (= #{"kp-1" "kp-2" "kp-3"} (set (map :id rows))))
      (is (= #{"todo" "inprogress" "inreview"} (set (map :status rows))))
      (is (every? #(m/validate kanban/Task %) rows)))))

(deftest get-task-returns-the-task-or-nil
  (seed-board!)
  (let [t (kanban-port/get-task "kp-3")]
    (is (= "kp-3" (:id t)))
    (is (= "inreview" (:status t)))
    (is (= "about Review" (:description t)))
    (is (= pid (:project t))))
  (is (nil? (kanban-port/get-task "kp-none"))))

(deftest transition-moves-the-stored-entry
  (seed-board!)
  (let [res (kanban-port/transition! {:task-id "kp-1" :new-status "done"
                                      :directory "/tmp/kanban-port-test"})]
    (is (= "done" (get-in res [:ok :status])) (pr-str res))
    (is (= "done" (:status (kanban-port/get-task "kp-1"))))))

(deftest create-task-lands-in-the-store
  (let [{:keys [ok err]} (kanban-port/create-task! {:title "Fresh" :priority "low"
                                                    :directory "/tmp/kanban-port-test"})]
    (is (nil? err) (pr-str err))
    (is (string? (:id ok)))
    (is (= "Fresh" (:title (kanban-port/get-task (:id ok)))))))

(deftest the-registry-is-read-at-call-time
  (seed-board!)
  (is (seq (kanban-port/list-tasks {:project-id pid})))
  (kport/with-recording-kanban
    (fn []
      (is (= [] (kanban-port/list-tasks {:project-id pid}))
          "a later registration is what the facade answers from")))
  (is (seq (kanban-port/list-tasks {:project-id pid}))
      "and the prior provider comes back when the double is removed"))
