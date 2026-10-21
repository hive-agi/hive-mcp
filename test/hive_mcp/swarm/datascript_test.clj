(ns hive-mcp.swarm.datascript-test
  "Facets for hive-mcp.swarm.datascript, the backward-compatibility facade
   over the swarm DataScript store.

     trifecta  q-db, the one fn here that takes its db as a value: over any
               generated set of entities, a :find query answers exactly the
               values transacted
     golden    the facade map: which sub-module var each re-export aliases,
               and where the store-backed fns (db-handle, current-db, q, ...)
               delegate without hive-datascript on the unit classpath (see
               hive-mcp.test.swarm-shim). The store itself is exercised in
               test-swarm/hive_mcp/swarm/datascript_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [datascript.core :as d]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private DbOfNames
  "A db holding entities {:slave/name n}, built from a generated name vector."
  [:fn {:gen/schema [:vector {:gen/max 6} [:enum "a" "b" "c" "d"]]
        :gen/fmap (fn [names] (d/db-with (d/empty-db) (mapv (fn [n] {:slave/name n}) names)))}
   d/db?])

(def ^:private names-query '[:find [?n ...] :where [_ :slave/name ?n]])

(hst/deftrifecta-from-schema q-db-answers-the-transacted-values
  hive-mcp.swarm.datascript/q-db
  {:in  [:cat DbOfNames [:= {} names-query]]
   :out [:maybe [:sequential [:enum "a" "b" "c" "d"]]]
   :rel (fn [[db _] out]
          (= (set (map :v (d/datoms db :aevt :slave/name))) (set out)))
   :num-tests 200})

(deftest-golden q-db-cases-golden
  "test/golden/hive-mcp/swarm/datascript-q-db.edn"
  (let [db (d/db-with (d/empty-db) [{:slave/name "a" :slave/status :idle}
                                    {:slave/name "b" :slave/status :working}])]
    {:names      (sort (ds/q-db db names-query))
     :with-input (ds/q-db db '[:find [?n ...] :in $ ?s :where [?e :slave/status ?s] [?e :slave/name ?n]]
                          :working)
     :empty      (ds/q-db (d/empty-db) names-query)}))

(def ^:private facade-vars
  [#'ds/get-conn #'ds/reset-conn! #'ds/add-slave! #'ds/update-slave! #'ds/remove-slave!
   #'ds/can-kill? #'ds/add-task! #'ds/claim-file! #'ds/release-claim! #'ds/complete-task!
   #'ds/get-slave #'ds/get-all-slaves #'ds/get-slaves-by-status #'ds/get-slaves-by-project
   #'ds/get-slave-ids-by-project #'ds/has-conflict? #'ds/check-file-conflicts
   #'ds/get-claims-for-file #'ds/get-task #'ds/db-stats #'ds/add-wrap-notification!
   #'ds/get-unprocessed-wraps-for-project #'ds/get-unprocessed-wraps-for-hierarchy
   #'ds/get-unprocessed-wraps-for-session #'ds/mark-wrap-processed! #'ds/register-coordinator!
   #'ds/mark-coordinator-terminated! #'ds/cleanup-stale-claims! #'ds/register-completed-task!
   #'ds/get-completed-task #'ds/get-completed-tasks-this-session #'ds/clear-completed-tasks!
   #'ds/register-kanban-movement! #'ds/get-kanban-movements-this-session
   #'ds/clear-kanban-movements!])

(deftest-golden facade-golden
  "test/golden/hive-mcp/swarm/datascript-facade.edn"
  {:aliases   (into (sorted-map) (map (fn [v] [(str (:name (meta v))) (symbol @v)])) facade-vars)
   :store-fns {:db-handle  (shim/target ds/db-handle)
               :current-db (shim/target ds/current-db)
               :q          (shim/target ds/q names-query)
               :pull       (shim/target ds/pull '[*] 1)
               :transact!  (shim/target ds/transact! [])}})

(deftest db-schemas-test
  (is (m/validate ds/Db (d/empty-db)))
  (is (m/validate ds/Query '[:find ?e :where [?e :a]]))
  (is (m/validate ds/Query '{:find [?e] :where [[?e :a]]})))
