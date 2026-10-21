(ns hive-mcp.knowledge-graph.store.fixtures-test
  "The backward-compatible fixture facade: each fixture runs its test fn
   against an ephemeral store and answers what the test fn answers;
   backend-fixture maps every backend keyword onto its fixture."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.connection.store :as cstore]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.protocols.kg :as kg]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- write-then-count
  "A test fn: write one edge into whatever store the fixture installed, then
   count edges. A fresh store per fixture call always answers 1."
  []
  (let [s (cstore/ensure-store!)]
    (kg/transact! s [{:kg-edge/id (str (random-uuid))}])
    (kg/query s '[:find (count ?e) . :where [?e :kg-edge/id _]])))

(deftest-golden fixtures-facade-golden
  "test/golden/hive-mcp/knowledge-graph/store-fixtures-facade.edn"
  (let [global-before (when (kg/store-set?) (kg/get-store))]
    {:datascript       [(fixtures/datascript-fixture write-then-count)
                        (fixtures/datascript-fixture write-then-count)]
     :global           (fixtures/global-datascript-fixture write-then-count :sync-writes? true)
     :passes-value     (fixtures/datascript-fixture (constantly :ran))
     :backend-fixtures (into (sorted-map)
                             (for [b [:datascript :datalevin :datahike]]
                               [b (= ({:datascript fixtures/datascript-fixture
                                       :datalevin  fixtures/datalevin-fixture
                                       :datahike   fixtures/datahike-fixture} b)
                                     (fixtures/backend-fixture b))]))
     :current-backend  fixtures/*current-backend*
     :global-restored? (identical? global-before (when (kg/store-set?) (kg/get-store)))}))

(deftest unknown-backend-is-refused
  (is (thrown? IllegalArgumentException (fixtures/backend-fixture :nope))))
