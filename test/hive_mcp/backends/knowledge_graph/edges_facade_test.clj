(ns hive-mcp.backends.knowledge-graph.edges-facade-test
  "Tests for the graph-algos node facade in edges.clj:
   get-all-node-ids / node-ids-by-tag / neighbors.

   These fns are resolved by hive-knowledge
   graph-algos.adapters.default/DatahikeKgReader via requiring-resolve,
   so their names and arities are load-bearing across repos.

   Also covers norm 005-synth-schema.edn: :synth/node-id must be
   :db.unique/identity on a fresh Datahike DB so synth writers upsert
   instead of accreting duplicate entities (GAV2 review C1)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [datahike.api :as d]
            [datahike.norm.norm :as norm]
            [hive-mcp.knowledge-graph.edges :as edges]
            [hive-mcp.knowledge-graph.store.fixtures :as fixtures]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.test-support.registry-isolation :as iso]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Fixtures
;; =============================================================================

(use-fixtures :each fixtures/datascript-fixture)

;; =============================================================================
;; get-all-node-ids
;; =============================================================================

(deftest get-all-node-ids-empty-graph-test
  (testing "get-all-node-ids returns [] on empty graph"
    (is (= [] (edges/get-all-node-ids)))))

(deftest get-all-node-ids-union-test
  (testing "get-all-node-ids returns distinct union of :kg-edge/from and :kg-edge/to"
    (edges/add-edge! {:from "a" :to "b" :relation :implements})
    (edges/add-edge! {:from "b" :to "c" :relation :refines})
    ;; "b" appears as both from and to — must not be duplicated
    (let [ids (edges/get-all-node-ids)]
      (is (= #{"a" "b" "c"} (set ids)))
      (is (= (count ids) (count (distinct ids))) "no duplicates")
      (is (every? string? ids)))))

(deftest get-all-node-ids-scope-test
  (testing "scope arity only returns nodes on edges in that scope"
    (edges/add-edge! {:from "s1-a" :to "s1-b" :relation :implements :scope "scope-1"})
    (edges/add-edge! {:from "s2-a" :to "s2-b" :relation :implements :scope "scope-2"})
    (edges/add-edge! {:from "ns-a" :to "ns-b" :relation :implements})
    (is (= #{"s1-a" "s1-b"} (set (edges/get-all-node-ids "scope-1"))))
    (is (= #{"s2-a" "s2-b"} (set (edges/get-all-node-ids "scope-2"))))
    (testing "nil scope behaves like 0-arity"
      (is (= (set (edges/get-all-node-ids))
             (set (edges/get-all-node-ids nil)))))))

;; =============================================================================
;; node-ids-by-tag
;; =============================================================================
;;
;; Tags live in the memory store, reached through the IMemoryStore port. The
;; store is a contract stub (hive-mcp.test.stub.memory-store), wrapped in the
;; observing store so the port call itself is asserted; no backend is named.

(deftest node-ids-by-tag-queries-the-memory-port-test
  (testing "node-ids-by-tag asks the :default memory store for the tag with the 10k limit"
    (iso/with-restored-store-registry
      (fn []
        (let [inner (mem-stub/->stub)
              _     (mem-stub/seed! inner [{:id "entry-1" :type "note" :tags ["kanban"]}
                                           {:id "entry-2" :type "note" :tags ["kanban" "todo"]}
                                           {:id "entry-3" :type "note" :tags ["other"]}])
              store (mem-stub/install! (mem-stub/->observing inner))]
          (is (= #{"entry-1" "entry-2"} (set (edges/node-ids-by-tag "kanban"))))
          (let [[[opts] :as qcalls] (mem-stub/calls-of store :query-entries)]
            (is (= 1 (count qcalls)))
            (is (= ["kanban"] (:tags opts)))
            (is (= 10000 (:limit opts)))))))))

(deftest node-ids-by-tag-degrades-loudly-test
  (testing "node-ids-by-tag returns [] (not throw) when the store's query fails"
    (iso/with-restored-store-registry
      (fn []
        (let [store (mem-stub/install!
                     (mem-stub/->observing (mem-stub/->stub)
                                           {:query-entries "No embedding provider available"}))]
          (is (= [] (edges/node-ids-by-tag "kanban")))
          (is (= 1 (count (mem-stub/calls-of store :query-entries)))
              "the failing port call was made, so the [] is the degrade path"))))))

(deftest node-ids-by-tag-no-store-test
  (testing "node-ids-by-tag returns [] when no memory store is registered"
    (mem-stub/with-no-store
      (fn [] (is (= [] (edges/node-ids-by-tag "kanban")))))))

(deftest node-ids-by-tag-empty-result-test
  (testing "node-ids-by-tag returns [] when no entries carry the tag"
    (mem-stub/with-stub-store
      (fn [] (is (= [] (edges/node-ids-by-tag "no-such-tag")))))))

;; =============================================================================
;; neighbors
;; =============================================================================

(defn- seed-neighbor-graph!
  "a->b, a->c, d->a, plus self-loop a->a and unrelated x->y."
  []
  (edges/add-edge! {:from "a" :to "b" :relation :implements})
  (edges/add-edge! {:from "a" :to "c" :relation :refines})
  (edges/add-edge! {:from "d" :to "a" :relation :depends-on})
  (edges/add-edge! {:from "a" :to "a" :relation :refines})
  (edges/add-edge! {:from "x" :to "y" :relation :implements}))

(deftest neighbors-out-test
  (testing "direction :out returns targets of outgoing edges"
    (seed-neighbor-graph!)
    (is (= #{"b" "c"} (set (edges/neighbors "a" :out))))))

(deftest neighbors-in-test
  (testing "direction :in returns sources of incoming edges"
    (seed-neighbor-graph!)
    (is (= #{"d"} (set (edges/neighbors "a" :in))))))

(deftest neighbors-both-test
  (testing "direction :both returns the union, self-loops excluded"
    (seed-neighbor-graph!)
    (let [ids (edges/neighbors "a" :both)]
      (is (= #{"b" "c" "d"} (set ids)))
      (is (= (count ids) (count (distinct ids))) "no duplicates")
      (is (not-any? #{"a"} ids) "self-loop excluded"))))

(deftest neighbors-default-direction-test
  (testing "1-arity defaults to :both; nil direction treated as :both"
    (seed-neighbor-graph!)
    (is (= (set (edges/neighbors "a" :both))
           (set (edges/neighbors "a"))))
    (is (= (set (edges/neighbors "a" :both))
           (set (edges/neighbors "a" nil))))))

(deftest neighbors-unknown-node-test
  (testing "unknown node returns []"
    (seed-neighbor-graph!)
    (is (= [] (edges/neighbors "no-such-node" :both)))))

(deftest neighbors-invalid-direction-test
  (testing "invalid direction throws ex-info (loud, not silent)"
    (seed-neighbor-graph!)
    (is (thrown? clojure.lang.ExceptionInfo
                 (edges/neighbors "a" :sideways)))))

;; =============================================================================
;; Norm 005 — :synth/node-id unique identity (throwaway in-memory Datahike)
;; =============================================================================

(def ^:private dh-test-cfg
  {:store {:backend :memory
           :id (java.util.UUID/randomUUID)}
   :schema-flexibility :read
   :index :datahike.index/persistent-set})

(defn- fresh-dh-conn []
  (let [cfg (assoc-in dh-test-cfg [:store :id] (java.util.UUID/randomUUID))]
    (d/create-database cfg)
    (d/connect cfg)))

(defn- release-dh-conn [conn]
  (let [cfg (:config @conn)]
    (d/release conn)
    (when (d/database-exists? cfg)
      (d/delete-database cfg))))

(deftest synth-norm-declares-unique-identity-test
  (testing "norm 005 declares :synth/node-id unique identity + typed metric attrs"
    (let [conn (fresh-dh-conn)]
      (try
        (norm/ensure-norms! conn (io/resource "hive_mcp/norms/kg"))
        (let [schema (d/schema (d/db conn))]
          (is (= :db.unique/identity (:db/unique (schema :synth/node-id)))
              ":synth/node-id must be unique identity for idempotent upserts")
          (is (= :db.type/double (:db/valueType (schema :synth/betweenness))))
          (is (= :db.type/long (:db/valueType (schema :synth/k-core))))
          (is (= :db.type/string (:db/valueType (schema :synth/community-id))))
          ;; every attr from hive-mcp schema.clj synth-schema is mirrored
          (doseq [attr [:synth/node-id :synth/community-id :synth/betweenness
                        :synth/k-core :synth/hits-hub :synth/hits-auth
                        :synth/conductance :synth/modularity-q :synth/katz
                        :synth/eigenvector :synth/triangle-count
                        :synth/clustering-coef]]
            (is (some? (schema attr)) (str attr " missing from norm 005"))))
        (finally
          (release-dh-conn conn))))))

(deftest synth-norm-upsert-no-duplicates-test
  (testing "writing the same :synth/node-id twice upserts a single entity (C1 regression)"
    (let [conn (fresh-dh-conn)]
      (try
        (norm/ensure-norms! conn (io/resource "hive_mcp/norms/kg"))
        (d/transact conn {:tx-data [{:synth/node-id "node-1" :synth/k-core 2}]})
        (d/transact conn {:tx-data [{:synth/node-id "node-1" :synth/k-core 3}]})
        (let [eids (d/q '[:find [?e ...] :where [?e :synth/node-id "node-1"]]
                        (d/db conn))]
          (is (= 1 (count eids)) "second write must upsert, not create"))
        (is (= 3 (:synth/k-core (d/entity (d/db conn) [:synth/node-id "node-1"])))
            "lookup-ref read sees the updated value")
        (finally
          (release-dh-conn conn))))))
