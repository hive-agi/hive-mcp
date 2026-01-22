(ns hive-mcp.knowledge-graph.connection-test
  "Unit tests for Knowledge Graph connection lifecycle.

   Tests cover:
   - create-conn: Creates fresh DataScript connection
   - get-conn / ensure-conn: Singleton connection management
   - reset-conn!: Connection reset and cleanup
   - gen-edge-id: Unique ID generation with timestamp
   - now: Timestamp generation"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datascript.core :as d]
            [hive-mcp.knowledge-graph.connection :as conn]
            [hive-mcp.knowledge-graph.schema :as schema]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Test Fixtures
;; =============================================================================

(defn reset-conn-fixture
  "Reset connection before and after each test for isolation."
  [f]
  (conn/reset-conn!)
  (f)
  (conn/reset-conn!))

(use-fixtures :each reset-conn-fixture)

;; =============================================================================
;; create-conn Tests
;; =============================================================================

(deftest create-conn-returns-datascript-conn-test
  (testing "create-conn returns a DataScript connection"
    (let [c (conn/create-conn)]
      (is (some? c))
      ;; DataScript connections are datascript.conn.Conn (deref-able)
      (is (instance? datascript.conn.Conn c)))))

(deftest create-conn-has-kg-schema-test
  (testing "create-conn creates connection with KG schema"
    (let [c (conn/create-conn)
          db @c]
      ;; Check that schema attributes are present
      (is (some? (:kg-edge/id (d/schema db))))
      (is (some? (:kg-edge/from (d/schema db))))
      (is (some? (:kg-edge/to (d/schema db))))
      (is (some? (:kg-edge/relation (d/schema db)))))))

(deftest create-conn-creates-fresh-db-test
  (testing "create-conn creates empty database each time"
    (let [c1 (conn/create-conn)
          c2 (conn/create-conn)]
      ;; Both should be empty
      (is (= 0 (count (d/datoms @c1 :eavt))))
      (is (= 0 (count (d/datoms @c2 :eavt))))
      ;; They should be different connections
      (is (not= c1 c2)))))

;; =============================================================================
;; get-conn / ensure-conn Tests
;; =============================================================================

(deftest get-conn-returns-singleton-test
  (testing "get-conn returns same connection on repeated calls"
    (let [c1 (conn/get-conn)
          c2 (conn/get-conn)]
      (is (= c1 c2)))))

(deftest ensure-conn-returns-same-as-get-conn-test
  (testing "ensure-conn is equivalent to get-conn"
    (let [c1 (conn/get-conn)
          c2 (conn/ensure-conn)]
      (is (= c1 c2)))))

(deftest get-conn-creates-if-needed-test
  (testing "get-conn creates connection if none exists"
    ;; After fixture reset, get-conn should create new conn
    (let [c (conn/get-conn)]
      (is (some? c))
      (is (instance? datascript.conn.Conn c)))))

;; =============================================================================
;; reset-conn! Tests
;; =============================================================================

(deftest reset-conn-clears-data-test
  (testing "reset-conn! clears all data"
    (let [c (conn/ensure-conn)]
      ;; Add some data
      (d/transact! c [{:kg-edge/id "test-edge"
                       :kg-edge/from "node-1"
                       :kg-edge/to "node-2"
                       :kg-edge/relation :implements}])
      ;; Verify data exists
      (is (= 1 (count (d/q '[:find [?e ...] :where [?e :kg-edge/id _]] @c))))
      ;; Reset
      (conn/reset-conn!)
      ;; Verify data is gone
      (let [new-c (conn/ensure-conn)]
        (is (= 0 (count (d/q '[:find [?e ...] :where [?e :kg-edge/id _]] @new-c))))))))

(deftest reset-conn-creates-new-connection-test
  (testing "reset-conn! creates a new connection instance"
    (let [c1 (conn/ensure-conn)]
      (conn/reset-conn!)
      (let [c2 (conn/ensure-conn)]
        ;; They should be different atom instances
        (is (not= c1 c2))))))

(deftest reset-conn-preserves-schema-test
  (testing "reset-conn! preserves the KG schema"
    (conn/reset-conn!)
    (let [c (conn/ensure-conn)
          db @c]
      (is (some? (:kg-edge/id (d/schema db))))
      (is (= :db.unique/identity (get-in (d/schema db) [:kg-edge/id :db/unique]))))))

;; =============================================================================
;; gen-edge-id Tests
;; =============================================================================

(deftest gen-edge-id-returns-string-test
  (testing "gen-edge-id returns a string"
    (let [id (conn/gen-edge-id)]
      (is (string? id)))))

(deftest gen-edge-id-starts-with-edge-prefix-test
  (testing "gen-edge-id starts with 'edge-' prefix"
    (let [id (conn/gen-edge-id)]
      (is (clojure.string/starts-with? id "edge-")))))

(deftest gen-edge-id-contains-timestamp-test
  (testing "gen-edge-id contains timestamp in format yyyyMMdd'T'HHmmss"
    (let [id (conn/gen-edge-id)]
      ;; Format: edge-20260120T143052-a1b2c3
      ;; The timestamp part should have digits
      (is (re-matches #"edge-\d{8}T\d{6}-[a-f0-9]{6}" id)))))

(deftest gen-edge-id-unique-test
  (testing "gen-edge-id generates unique IDs"
    (let [ids (repeatedly 100 conn/gen-edge-id)
          unique-ids (set ids)]
      ;; All 100 IDs should be unique
      (is (= 100 (count unique-ids))))))

(deftest gen-edge-id-sortable-test
  (testing "gen-edge-id generates chronologically sortable IDs"
    ;; Generate IDs with small delay to ensure different timestamps
    (let [id1 (conn/gen-edge-id)
          _ (Thread/sleep 1)
          id2 (conn/gen-edge-id)]
      ;; When sorted lexicographically, id1 should come before or equal to id2
      ;; compare returns negative when id1 < id2, so check <= 0
      (is (<= (compare id1 id2) 0)))))

;; =============================================================================
;; now Tests
;; =============================================================================

(deftest now-returns-date-test
  (testing "now returns java.util.Date"
    (let [t (conn/now)]
      (is (instance? java.util.Date t)))))

(deftest now-returns-current-time-test
  (testing "now returns approximately current time"
    (let [before (System/currentTimeMillis)
          t (conn/now)
          after (System/currentTimeMillis)]
      ;; The date should be between before and after
      (is (<= before (.getTime t)))
      (is (>= after (.getTime t))))))

(deftest now-returns-different-instances-test
  (testing "now returns new Date instances each call"
    (let [t1 (conn/now)
          t2 (conn/now)]
      ;; They may have the same value but should be different objects
      (is (not (identical? t1 t2))))))

;; =============================================================================
;; Schema Integration Tests
;; =============================================================================

(deftest schema-has-unique-edge-id-test
  (testing "Schema enforces unique :kg-edge/id"
    (let [c (conn/ensure-conn)]
      ;; Add first edge
      (d/transact! c [{:kg-edge/id "unique-edge-1"
                       :kg-edge/from "a"
                       :kg-edge/to "b"
                       :kg-edge/relation :implements}])
      ;; Adding same ID should upsert, not create duplicate
      (d/transact! c [{:kg-edge/id "unique-edge-1"
                       :kg-edge/from "c"  ;; Different from
                       :kg-edge/to "d"
                       :kg-edge/relation :supersedes}])
      ;; Should still have only one entity with that ID
      (let [edges (d/q '[:find [?e ...]
                         :where [?e :kg-edge/id "unique-edge-1"]]
                       @c)]
        (is (= 1 (count edges))))
      ;; The entity should have the updated values
      (let [entity (d/entity @c [:kg-edge/id "unique-edge-1"])]
        (is (= "c" (:kg-edge/from entity)))
        (is (= "d" (:kg-edge/to entity)))
        (is (= :supersedes (:kg-edge/relation entity)))))))

(deftest schema-supports-all-edge-attributes-test
  (testing "Schema supports all KG edge attributes"
    (let [c (conn/ensure-conn)
          now (conn/now)]
      (d/transact! c [{:kg-edge/id "full-edge"
                       :kg-edge/from "memory-123"
                       :kg-edge/to "memory-456"
                       :kg-edge/relation :implements
                       :kg-edge/scope "hive-mcp:agora"
                       :kg-edge/confidence 0.85
                       :kg-edge/created-by "agent:coordinator"
                       :kg-edge/created-at now}])
      (let [entity (d/entity @c [:kg-edge/id "full-edge"])]
        (is (= "memory-123" (:kg-edge/from entity)))
        (is (= "memory-456" (:kg-edge/to entity)))
        (is (= :implements (:kg-edge/relation entity)))
        (is (= "hive-mcp:agora" (:kg-edge/scope entity)))
        (is (= 0.85 (:kg-edge/confidence entity)))
        (is (= "agent:coordinator" (:kg-edge/created-by entity)))
        (is (= now (:kg-edge/created-at entity)))))))
