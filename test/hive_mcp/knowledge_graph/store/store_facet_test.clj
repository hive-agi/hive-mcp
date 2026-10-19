(ns hive-mcp.knowledge-graph.store.store-facet-test
  "Goldens over the DataScript store, the disposable-store harness, the
   fixtures facade and the two typed backend configs. Every store is
   ephemeral; the process-global store is checked to be restored."
  (:require [hive-di.resolve :as resolve]
            [hive-mcp.knowledge-graph.protocol :as proto]
            [hive-mcp.knowledge-graph.store.datahike-config :as dhc]
            [hive-mcp.knowledge-graph.store.datalevin-config :as dlc]
            [hive-mcp.knowledge-graph.store.datascript :as ds-store]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.knowledge-graph.store.harness :as harness]
            [hive-mcp.protocols.kg :as kg]
            [hive-test.golden :as golden]
            [hive-mcp.knowledge-graph.connection.store :as cstore]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(golden/deftest-golden datascript-store-golden
  "test/golden/hive-mcp/m14-store-datascript.edn"
  (let [s (ds-store/create-store)]
    (kg/transact! s [{:kg-edge/id "e1" :kg-edge/from "a" :kg-edge/to "b" :kg-edge/relation :implements}
                     {:kg-edge/id "e2" :kg-edge/from "b" :kg-edge/to "c" :kg-edge/relation :refines}])
    (let [eid (kg/entid s [:kg-edge/id "e1"])]
      {:satisfies? (satisfies? kg/IKGStore s)
       :query (sort (kg/query s '[:find [?to ...] :where [_ :kg-edge/to ?to]]))
       :query-in (kg/query s '[:find ?to . :in $ ?from :where [?e :kg-edge/from ?from] [?e :kg-edge/to ?to]] ["a"])
       :pull (kg/pull-entity s [:kg-edge/from :kg-edge/relation] eid)
       :entity-from (:kg-edge/from (kg/entity s eid))
       :eids (count (kg/eids-by-attr s :kg-edge/id))
       :reset-empty (do (kg/reset-conn! s) (count (kg/eids-by-attr s :kg-edge/id)))
       :close (kg/close! s)})))

(defn- global-before [] (when (proto/store-set?) (proto/get-store)))

(golden/deftest-golden harness-golden
  "test/golden/hive-mcp/m14-store-harness.edn"
  (let [prior (global-before)
        seen (atom nil)
        thread-local (harness/with-disposable-store
                       (harness/datascript-factory)
                       (harness/thread-local-isolation)
                       (fn [] (reset! seen (cstore/ensure-store!)) :thread-local-ran))
        global (harness/with-disposable-store
                 (harness/datascript-factory)
                 (harness/global-isolation :sync-writes? true)
                 (fn [] (identical? (proto/get-store) (cstore/ensure-store!))))]
    {:thread-local thread-local
     :ephemeral-was-a-store? (satisfies? kg/IKGStore @seen)
     :global-installed-ephemeral? global
     :global-restored? (identical? prior (global-before))
     :available {:datascript (harness/available? (harness/datascript-factory))
                 :datalevin-boolean? (boolean? (harness/available? (harness/datalevin-factory)))
                 :datahike-boolean? (boolean? (harness/available? (harness/datahike-factory)))}
     :skip (with-out-str (harness/skip-if-unavailable "M14" #(throw (Exception. "no driver"))))
     :skip-pass (harness/skip-if-unavailable "M14" (constantly :ran))
     :ds-fixture (harness/datascript-store-fixture (constantly :ran))
     :global-fixture (harness/global-datascript-store-fixture (constantly :ran) :sync-writes? true)
     :still-restored? (identical? prior (global-before))}))

(golden/deftest-golden fixtures-golden
  "test/golden/hive-mcp/m14-store-fixtures.edn"
  (let [prior (global-before)
        calls (atom 0)]
    (with-out-str (fixtures/dual-backend-fixture (fn [] (swap! calls inc))))
    {:datascript (fixtures/datascript-fixture (constantly :ran))
     :global (fixtures/global-datascript-fixture (constantly :ran) :sync-writes? true)
     :global-map (fixtures/global-datascript-fixture (constantly :ran) {:sync-writes? false})
     :backend-is-fixture (= fixtures/datascript-fixture (fixtures/backend-fixture :datascript))
     :backend-others (mapv #(fn? (fixtures/backend-fixture %)) [:datalevin :datahike])
     :dual-ran-datascript? (pos? @calls)
     :bound-backend (fixtures/datascript-fixture (fn [] fixtures/*current-backend*))
     :restored? (identical? prior (global-before))}))

(defn- resolved [fields env file]
  (:ok (resolve/resolve-config fields {} {:env-fn env :file-fn file})))

(golden/deftest-golden backend-config-golden
  "test/golden/hive-mcp/m14-store-configs.edn"
  (let [file (constantly {:services {:datahike {:path "/f/dh" :id "abcd"}
                                     :datalevin {:path "/f/dl" :cache-limit 7}}})
        env (fn [v] (get {"HIVE_KG_DB_PATH" "/e/dh" "HIVE_KG_DATALEVIN_PATH" "/e/dl"
                          "HIVE_KG_DH_BACKEND" "memory"} v))
        home (System/getProperty "user.home")
        rel #(when % (.replace ^String % home "~"))]
    {:datahike {:file (resolved dhc/DatahikeKGConfig-fields (constantly nil) file)
                :env (resolved dhc/DatahikeKGConfig-fields env file)
                :default (update (resolved dhc/DatahikeKGConfig-fields (constantly nil) (constantly nil)) :db-path rel)}
     :datalevin {:file (resolved dlc/DatalevinKGConfig-fields (constantly nil) file)
                 :env (resolved dlc/DatalevinKGConfig-fields env file)
                 :default (update (resolved dlc/DatalevinKGConfig-fields (constantly nil) (constantly nil)) :db-path rel)}
     :defaults [(rel dhc/default-db-path) (rel dlc/default-db-path) dlc/default-cache-limit]}))
