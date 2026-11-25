(ns hive-mcp.knowledge-graph.connection.query-with-db-test
  "Trifecta for `hive-mcp.knowledge-graph.connection/query-with-db`: run a
   Datalog query against an EXPLICIT db value (pre-tx vs post-tx snapshots),
   falling back to the live store when no db is given.

   The snapshots are real in-memory datahike db values built here (the same
   backend `query-with-db` resolves first); the fallback runs against the real
   in-memory DataScript KG store the global fixture installs. No redefinition.

     golden    : pre-tx, post-tx, parameterised and fallback answers
     property  : a snapshot answers exactly what was transacted into it, and
                 never what was transacted after it was taken
     mutation  : a mutant that ignores the db (always queries live) is caught"
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [datahike.api :as dh]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as hmut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (fixtures/global-datascript-fixture f :sync-writes? true)))

(def ^:private all-ks '[:find ?k :where [_ :k ?k]])

(defn- with-db
  "Call (f transact! snapshot) over a throwaway in-memory datahike database."
  [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :read :keep-history? false :value-caps :default}]
    (dh/create-database cfg)
    (let [c (dh/connect cfg)]
      (try (f (fn [ks] (dh/transact c (mapv (fn [k] {:k k}) ks)))
              (fn [] (dh/db c)))
           (finally (dh/release c) (dh/delete-database cfg))))))

(defn- live-edge! [id]
  (conn/transact! [{:kg-edge/id id :kg-edge/from "a" :kg-edge/to "b"
                    :kg-edge/relation :implements :kg-edge/confidence 1.0}]))

(deftest-golden query-with-db-answers-golden
  "test/golden/hive-mcp/knowledge_graph/query-with-db.edn"
  (with-db
    (fn [tx! db]
      (tx! ["a"])
      (let [pre (db)]
        (tx! ["b"])
        (live-edge! "live-1")
        {:pre-tx        (conn/query-with-db pre all-ks)
         :post-tx       (conn/query-with-db (db) all-ks)
         :with-input    (conn/query-with-db (db) '[:find ?k :in $ ?x :where [_ :k ?k] [(= ?k ?x)]] "b")
         :nil-db-falls-back-to-live
         (conn/query-with-db nil '[:find ?id :where [_ :kg-edge/id ?id]])}))))

(defspec a-snapshot-answers-what-it-held-not-what-came-after 25
  (prop/for-all [before (gen/set gen/string-alphanumeric {:max-elements 5})
                 after  (gen/set gen/string-alphanumeric {:max-elements 5})]
    (with-db
      (fn [tx! db]
        (tx! before)
        (let [snap (db)]
          (tx! after)
          (and (= (set (map vector before)) (conn/query-with-db snap all-ks))
               (= (set (map vector (into before after))) (conn/query-with-db (db) all-ks))))))))

(deftest a-nil-db-queries-the-live-store
  (live-edge! "live-2")
  (is (= #{["live-2"]} (conn/query-with-db nil '[:find ?id :where [_ :kg-edge/id ?id]]))))

(hmut/deftest-mutations query-with-db-mutations
  hive-mcp.knowledge-graph.connection/query-with-db
  [["ignores the db and queries live" (fn [_db q & inputs] (apply conn/query q inputs))]
   ["returns nil"                     (fn [& _] nil)]
   ["returns the live store's db"     (fn [_db q & inputs] (apply conn/query q inputs))]]
  (fn []
    (with-db
      (fn [tx! db]
        (tx! ["a" "b"])
        (is (= #{["a"] ["b"]} (conn/query-with-db (db) all-ks)))))))
