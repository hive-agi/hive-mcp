(ns hive-mcp.tools.migrate.kanban.usecases-contract-test
  "Contract facets for the kanban migrator use cases over injected ports: a
   stub id lister and a FileState in a temp file, so Phase A and the status
   snapshot run with no store at all."
  (:require [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.state :as st]
            [hive-mcp.tools.migrate.kanban.usecases :as uc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-state []
  (st/make (str (java.io.File/createTempFile "kanban-mig-uc" ".edn"))))

(defn- lister [ids]
  (reify ports/IIdLister
    (list-ids [_] (r/ok {:ids ids :per-collection {"kanban" (count ids)}}))))

(defn phase-a
  "Subject: Phase A over IDS into a fresh state, then the status snapshot."
  [ids]
  (let [deps {:state (temp-state) :source-lister (lister ids)}
        listed (uc/init-ids! deps)]
    {:listed listed :status (:ok (uc/status deps))}))

(deftrifecta-from-schema phase-a-lists-every-id-and-status-counts-them
  #'phase-a
  {:in [:vector {:max 6} [:enum "a" "b" "c" "d"]]
   :out [:map [:listed uc/Result] [:status :map]]
   :rel (fn [ids {:keys [listed status]}]
          (and (= (count ids) (get-in listed [:ok :total]))
               (= (count ids) (:total status) (:remaining status))
               (zero? (:cursor status))))
   :num-tests 30})

(golden/deftest-golden usecases-phase-a-golden
  "test/golden/hive-mcp/tools/migrate/kanban-usecases.edn"
  (let [{:keys [listed status]} (phase-a ["x" "y"])]
    {:listed listed
     :status (dissoc status :phase-a-at)}))

(deftest a-failing-lister-is-an-err-and-nothing-is-listed
  (let [deps {:state (temp-state)
              :source-lister (reify ports/IIdLister (list-ids [_] (r/err :milvus/down {})))}]
    (is (= :milvus/down (:error (uc/init-ids! deps))))
    (is (zero? (:total (:ok (uc/status deps)))))))
