(ns hive-mcp.knowledge-graph.store.harness-test
  "The disposable test-store harness: both ports (StoreFactory,
   IsolationStrategy) exercised with the in-memory DataScript factory, a
   recording StoreFactory stub to prove dispose! always runs, and the
   skip-if-unavailable guard. The golden checks the safety invariant: the
   process-global store slot reads the same before and after every isolation."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.connection.store :as cstore]
            [hive-mcp.knowledge-graph.store.datascript :as kds]
            [hive-mcp.knowledge-graph.store.harness :as harness]
            [hive-mcp.protocols.kg :as kg]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord RecordingFactory [events]
  harness/StoreFactory
  (available? [_] true)
  (create [_]
    (swap! events conj :create)
    (let [s (kds/create-store)] (kg/ensure-conn! s) (harness/->EphemeralStore s nil)))
  (dispose! [_ _] (swap! events conj :dispose)))

(defn- global-slot [] (when (kg/store-set?) (kg/get-store)))

(defn- observe
  "What the body sees under a strategy: whether the thread-local override is
   bound, whether the strategy changed the global slot from its prior value,
   sync-writes."
  [strategy]
  (let [events (atom [])
        before (global-slot)
        seen   (harness/with-disposable-store
                 (->RecordingFactory events) strategy
                 (fn [] {:thread-local?   (some? cstore/*test-store*)
                         :global-changed? (not (identical? before (global-slot)))
                         :sync-writes?    (boolean conn/*sync-writes*)}))]
    {:seen seen :events @events :global-restored? (identical? before (global-slot))}))

(deftest-golden harness-isolations-golden
  "test/golden/hive-mcp/knowledge-graph/store-harness-isolations.edn"
  (let [events (atom [])
        threw  (try (harness/with-disposable-store
                      (->RecordingFactory events) (harness/thread-local-isolation)
                      #(throw (ex-info "body failed" {})))
                    (catch clojure.lang.ExceptionInfo e (ex-message e)))]
    {:thread-local       (observe (harness/thread-local-isolation))
     :thread-local-async (observe (harness/thread-local-isolation :sync-writes? false))
     :global             (observe (harness/global-isolation))
     :global-sync        (observe (harness/global-isolation :sync-writes? true))
     :body-throws        {:message threw :events @events}
     :skip               (with-out-str (harness/skip-if-unavailable
                                        "Fake" #(throw (Exception. "no driver"))))
     :skip-ok            (harness/skip-if-unavailable "Fake" (constantly :ran))
     :ds-available?      (harness/available? (harness/datascript-factory))
     :fixture            (harness/datascript-store-fixture
                          #(kg/query (cstore/ensure-store!) '[:find (count ?e) . :where [?e :kg-edge/id _]]))}))

(deftest datascript-dispose-resets-the-conn
  (let [f  (harness/datascript-factory)
        es (harness/create f)]
    (kg/transact! (:store es) [{:kg-edge/id "x"}])
    (harness/dispose! f es)
    (is (empty? (kg/query (:store es) '[:find ?e :where [?e :kg-edge/id _]])))))
