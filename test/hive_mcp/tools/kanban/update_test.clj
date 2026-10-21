(ns hive-mcp.tools.kanban.update-test
  "Facets for the one-commit `kanban update`.

   update-fx is the pure core: a trifecta over its contract with generated
   entries and edits, and a golden over its effect map per arm (move only
   to doing, into done with the completion hooks, non-kanban entry). The
   handler dispatch-update! gets a golden over its request guards and one
   committed run on the stub memory store (the memory port satisfied by
   hive-mcp.test.stub.memory-store; no with-redefs)."
  (:require [clojure.walk :as walk]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.tools.kanban.schema :as ks]
            [hive-mcp.tools.kanban.update :as ku]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- scrub
  "Drop the wall-clock stamps a move writes into content."
  [x]
  (walk/postwalk #(if (map? %) (dissoc % :completed :started :updated) %) x))

(def ^:private Coeffects
  [:map
   [:kanban/entry
    [:maybe [:map
             [:id [:= "t1"]]
             [:type [:= "note"]]
             [:tags [:= ["kanban" "todo" "priority-medium" "scope:project:p"]]]
             [:content [:map
                        [:task-type [:enum "kanban" "note"]]
                        [:title [:enum "T"]]
                        [:description [:enum "d"]]
                        [:status [:enum "todo"]]
                        [:priority [:enum "medium"]]]]]]]
   [:kanban/project-id [:= "p"]]])

(def ^:private Event
  [:tuple [:= :kanban/update]
   [:map
    [:task-id [:= "t1"]]
    [:new-status [:enum "doing" "review" "done"]]
    [:title {:optional true} [:enum "T2" "Other"]]
    [:priority {:optional true} [:enum "high" "low"]]]])

(deftrifecta-from-schema update-fx-commits-move-and-edit-as-one-update
  hive-mcp.tools.kanban.update/update-fx
  {:in        [:cat Coeffects Event]
   :out       [:maybe ks/FxMap]
   :rel       (fn [[{entry :kanban/entry} [_ {:keys [new-status title priority]}]] out]
                (if-not (= "kanban" (get-in entry [:content :task-type]))
                  (nil? out)
                  (let [{:keys [content tags]} (get-in out [:kanban/facade-update :payload])]
                    (and (= new-status (:status content))
                         (some #{new-status} tags)
                         (= (or title "T") (:title content))
                         (= (or priority "medium") (:priority content))
                         (= (= "done" new-status) (contains? out :kanban/notify-done))))))
   :num-tests 200})

(def ^:private entry
  {:id "t1" :type "note" :tags ["kanban" "todo" "priority-medium" "scope:project:p"]
   :content {:task-type "kanban" :title "T" :description "d" :status "todo" :priority "medium"}})

(defn- fx [e payload]
  (scrub (ku/update-fx {:kanban/entry e :kanban/project-id "p"}
                       [:kanban/update (merge {:task-id "t1"} payload)])))

(deftest-golden update-fx-arms
  "test/golden/tools/kanban/update-fx.edn"
  {:to-doing-priority (fx entry {:new-status "doing" :priority "high"})
   :to-done-title     (fx entry {:new-status "done" :title "T2"})
   :append            (fx entry {:new-status "review" :append-description "more"})
   :missing-entry     (fx nil {:new-status "done" :title "x"})
   :non-kanban        (fx (assoc-in entry [:content :task-type] "note") {:new-status "done" :title "x"})})

(defn- seeded [f]
  (ms/with-stub-store (fn [] (ms/install! (ms/->stub [entry])) (f))))

(use-fixtures :each seeded)

(deftest-golden dispatch-update-guards-and-commit
  "test/golden/tools/kanban/update-dispatch.edn"
  {:no-task-id     (ku/dispatch-update! {:new-status "done" :title "x"})
   :bad-status     (ku/dispatch-update! {:task-id "t1" :new-status "bogus" :title "x"})
   :nothing-to-do  (ku/dispatch-update! {:task-id "t1" :new-status "done"})
   :bad-priority   (ku/dispatch-update! {:task-id "t1" :new-status "done" :priority "urgent"})
   :missing-entry  (select-keys (ku/dispatch-update! {:task-id "nope" :new-status "done" :title "x"}) [:error])
   :committed      (-> (ku/dispatch-update! {:task-id "t1" :new-status "inprogress" :title "Moved"
                                             :directory "/tmp/kanban-update-test"})
                       (get-in [:ok :kanban/facade-update :payload])
                       scrub)})

(deftest register-is-idempotent
  (is (true? (ku/register!)))
  (is (true? (ku/register!))))
