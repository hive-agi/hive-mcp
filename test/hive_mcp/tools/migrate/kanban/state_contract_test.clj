(ns hive-mcp.tools.migrate.kanban.state-contract-test
  "Contract facets for the FileState adapter: a state saved through the
   IState port reads back unchanged, and a reset answers the initial state."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-mcp.tools.migrate.kanban.state :as st]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-path []
  (str (java.io.File/createTempFile "kanban-mig-state" ".edn")))

(defn round-trip
  "Subject: save STATE through a fresh FileState and load it back."
  [state]
  (let [s (st/make (temp-path))]
    (ports/save-state! s state)
    (:ok (ports/load-state s))))

(deftrifecta-from-schema saved-state-loads-back
  #'round-trip
  {:in [:map [:cursor [:int {:min 0 :max 100}]]
        [:phase [:enum :ready :ids-listed :running :done]]
        [:all-ids [:vector {:max 4} [:enum "a" "b" "c"]]]]
   :out :map
   :rel (fn [in out] (= in (select-keys out (keys in))))
   :num-tests 40})

(golden/deftest-golden fresh-state-golden
  "test/golden/hive-mcp/tools/migrate/kanban-state-fresh.edn"
  (let [s (st/make (str (temp-path) ".absent"))]
    {:fresh (:ok (ports/load-state s))
     :reset (:ok (ports/reset-state! s))}))

(deftest make-builds-the-port
  (is (satisfies? ports/IState (st/make "/tmp/m29-any.edn"))))
