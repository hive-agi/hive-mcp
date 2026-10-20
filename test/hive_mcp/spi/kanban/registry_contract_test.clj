(ns hive-mcp.spi.kanban.registry-contract-test
  "Contract facets for the kanban DIP registry and the port's conformance
   suite, plus the spi/kanban coverage gate. The provider table is process
   global, so every test snapshots it and puts it back."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.spi.kanban :as kanban]
            [hive-mcp.spi.kanban.registry :as kreg]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-providers [f]
  (let [prior @@#'kreg/-providers]
    (try (kreg/reset-providers!) (f)
         (finally (kreg/reset-providers!) (reset! @#'kreg/-providers prior)))))

(use-fixtures :each own-providers)

(deftrifecta-from-schema noop-is-membership-in-the-noop-table
  #'kreg/noop?
  {:in [:cat [:or kreg/Provider :keyword :nil]]
   :out :boolean
   :rel (fn [[x] out] (= out (contains? (set (vals kreg/noops)) x)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 100})

(defrecord BoardStub [tasks]
  kanban/IKanbanRead
  (list-tasks [_ _] (vec (vals tasks)))
  (get-task [_ id] (get tasks id))
  kanban/IKanbanWrite
  (transition! [_ {:keys [task-id new-status]}]
    (if-let [t (get tasks task-id)]
      {:ok (assoc t :status new-status)}
      {:err {:error :kanban/not-found}}))
  (create-task! [_ {:keys [title]}]
    (if (seq title) {:ok {:id "new-1"}} {:err {:error :kanban/blank-title}})))

(golden/deftest-golden registry-lifecycle-golden
  "test/golden/hive-mcp/spi/kanban-registry.edn"
  (let [board (->BoardStub {"t1" {:id "t1" :title "one" :status "todo"}})
        degraded {:list (kreg/list-tasks {}) :get (kreg/get-task "t1")
                  :transition (kreg/transition! {:task-id "t1" :new-status "done"})
                  :create (kreg/create-task! {:title "x"})
                  :registered? (kreg/registered? :IKanbanRead)
                  :noop? (kreg/noop? (kreg/provider :IKanbanRead))}
        _ (kreg/register! :IKanbanRead board)
        _ (kreg/register! :IKanbanWrite board)
        live {:list (kreg/list-tasks {}) :get (kreg/get-task "t1")
              :transition (kreg/transition! {:task-id "t1" :new-status "done"})
              :create (kreg/create-task! {:title "x"})
              :registered? (kreg/registered? :IKanbanRead)
              :conformance (mapv #(dissoc % :detail) (kanban/conformance board board))}
        refusals {:unknown-port (try (kreg/register! :Nope board) (catch Exception e (ex-message e)))
                  :wrong-impl   (try (kreg/register! :IKanbanRead :x) (catch Exception e (ex-message e)))}
        _ (kreg/unregister! :IKanbanRead)]
    {:degraded degraded :live live :refusals refusals
     :after-unregister (kreg/registered? :IKanbanRead)
     :reset (kreg/reset-providers!)}))

(deftest conformance-reports-a-throwing-impl-instead-of-throwing
  (let [bad (reify kanban/IKanbanRead
              (list-tasks [_ _] (throw (ex-info "down" {})))
              (get-task [_ _] {:id "x"}))
        rows (kanban/conformance bad (:IKanbanWrite kreg/noops) {:missing-id "m"})]
    (is (m/validate [:vector kanban/ConformanceRow] rows))
    (is (= [false false false true true] (mapv :ok? rows))
        "the read cases fail without throwing; the Noop writer answers {:err ..}")))

(coverage/deftest-contract-coverage spi-kanban-contracts
  {:paths ["src/hive_mcp/spi/kanban"]})

(deftest every-kanban-registry-contract-generates-both-sides
  (doseq [[fn-sym {:keys [schema]}] (get (m/function-schemas) 'hive-mcp.spi.kanban.registry)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (testing (str fn-sym)
      (is (= 3 (count (mg/sample side {:size 3 :seed 7})))))))
