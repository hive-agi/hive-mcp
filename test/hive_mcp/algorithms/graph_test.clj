(ns hive-mcp.algorithms.graph-test
  "Tests for graph algorithms including construction, traversal, and pathfinding."
  (:require [clojure.test :refer [deftest testing is are]]
            [hive-mcp.algorithms.graph.domain :as domain]
            [hive-mcp.algorithms.graph.impl :as impl]))

;; Test Fixtures

(def simple-graph
  "Simple directed graph: A->B->C, A->D"
  (-> (domain/empty-graph)
      (domain/add-node :A)
      (domain/add-node :B)
      (domain/add-node :C)
      (domain/add-node :D)
      (domain/add-edge :A :B)
      (domain/add-edge :B :C)
      (domain/add-edge :A :D)))

(def weighted-graph
  "Weighted graph for Dijkstra's algorithm testing"
  (-> (domain/empty-graph)
      (domain/add-node :A)
      (domain/add-node :B)
      (domain/add-node :C)
      (domain/add-node :D)
      (domain/add-edge :A :B 1)
      (domain/add-edge :A :C 4)
      (domain/add-edge :B :C 2)
      (domain/add-edge :B :D 5)
      (domain/add-edge :C :D 1)))

(def cyclic-graph
  "Graph with a cycle: A->B->C->A"
  (-> (domain/empty-graph)
      (domain/add-node :A)
      (domain/add-node :B)
      (domain/add-node :C)
      (domain/add-edge :A :B)
      (domain/add-edge :B :C)
      (domain/add-edge :C :A)))

(def disconnected-graph
  "Graph with isolated node: A->B, C (isolated)"
  (-> (domain/empty-graph)
      (domain/add-node :A)
      (domain/add-node :B)
      (domain/add-node :C)
      (domain/add-edge :A :B)))

;; Tests

(deftest graph-construction-test
  "Test graph construction operations."
  
  (testing "Empty graph creation"
    (let [g (domain/empty-graph)]
      (is (empty? (domain/nodes g)) "Empty graph should have no nodes")
      (is (empty? (domain/edges g)) "Empty graph should have no edges")))
  
  (testing "Adding nodes"
    (let [g (-> (domain/empty-graph)
                (domain/add-node :A)
                (domain/add-node :B))]
      (is (= #{:A :B} (set (domain/nodes g))) "Graph should contain added nodes")))
  
  (testing "Adding edges"
    (let [g (-> (domain/empty-graph)
                (domain/add-node :A)
                (domain/add-node :B)
                (domain/add-edge :A :B))]
      (is (= #{[:A :B]} (set (domain/edges g))) "Graph should contain added edge")))
  
  (testing "Creating graph from edges"
    (let [g (domain/graph-from-edges [[:A :B] [:B :C] [:A :D]])]
      (is (= #{:A :B :C :D} (set (domain/nodes g))) "Graph should contain all nodes from edges")
      (is (= #{[:A :B] [:B :C] [:A :D]} (set (domain/edges g))) "Graph should contain all edges"))))

(deftest bfs-test
  "Test BFS traversal algorithm."
  
  (testing "Basic BFS traversal"
    (let [result (impl/bfs simple-graph :A)]
      (is (contains? result :A) "BFS should visit starting node")
      (is (contains? result :B) "BFS should visit connected nodes")
      (is (contains? result :C) "BFS should visit nodes at depth 2")
      (is (contains? result :D) "BFS should visit all reachable nodes")))
  
  (testing "BFS returns correct order"
    (let [result (impl/bfs simple-graph :A)]
      (are [x y] (= x y)
           (:A result) 0
           (:B result) 1
           (:D result) 1
           (:C result) 2)))
  
  (testing "BFS handles disconnected graphs"
    (let [result (impl/bfs disconnected-graph :A)]
      (is (not (contains? result :C)) "BFS should not visit disconnected nodes"))))

(deftest dfs-test
  "Test DFS traversal algorithm."
  
  (testing "Basic DFS traversal"
    (let [result (impl/dfs simple-graph :A)]
      (is (contains? result :A) "DFS should visit starting node")
      (is (contains? result :B) "DFS should visit connected nodes")
      (is (contains? result :C) "DFS should visit nodes at depth 2")
      (is (contains? result :D) "DFS should visit all reachable nodes")))
  
  (testing "DFS handles cycles"
    (let [result (impl/dfs cyclic-graph :A)]
      (is (contains? result :A) "DFS should visit starting node")
      (is (contains? result :B) "DFS should visit connected nodes")
      (is (contains? result :C) "DFS should visit nodes in cycle")))
  
  (testing "DFS returns correct order (one possible valid order)"
    (let [result (impl/dfs simple-graph :A)]
      ;; DFS order can vary, but should be valid
      (is (<= (:A result) (:B result)) "Starting node should be visited first or at same time")
      (is (<= (:A result) (:D result)) "Starting node should be visited first or at same time"))))

(deftest dijkstra-test
  "Test Dijkstra's shortest path algorithm."
  
  (testing "Finds shortest path"
    (let [path (impl/dijkstra weighted-graph :A :D)]
      (is (= [:A :B :C :D] path) "Should find shortest path A->B->C->D")))
  
  (testing "Returns nil for unreachable nodes"
    (let [g (-> (domain/empty-graph)
                (domain/add-node :A)
                (domain/add-node :B))
          path (impl/dijkstra g :A :B)]
      (is (nil? path) "Should return nil for unreachable nodes")))
  
  (testing "Handles weighted edges correctly"
    (let [path (impl/dijkstra weighted-graph :A :C)]
      (is (= [:A :B :C] path) "Should prefer path with lower total weight"))))

(deftest edge-cases-test
  "Test edge cases in graph algorithms."
  
  (testing "Empty graph"
    (let [g (domain/empty-graph)]
      (is (empty? (impl/bfs g :A)) "BFS on empty graph should return empty result")
      (is (empty? (impl/dfs g :A)) "DFS on empty graph should return empty result")
      (is (nil? (impl/dijkstra g :A :B)) "Dijkstra on empty graph should return nil")))
  
  (testing "Single node graph"
    (let [g (domain/add-node (domain/empty-graph) :A)]
      (is (= {:A 0} (impl/bfs g :A)) "BFS on single node should return just that node")
      (is (= {:A 0} (impl/dfs g :A)) "DFS on single node should return just that node")))
  
  (testing "Self-loop"
    (let [g (-> (domain/empty-graph)
                (domain/add-node :A)
                (domain/add-edge :A :A))]
      (is (contains? (impl/bfs g :A) :A) "BFS should handle self-loops")
      (is (contains? (impl/dfs g :A) :A) "DFS should handle self-loops"))))

;; Run tests when this namespace is loaded
(comment
  (clojure.test/run-tests 'hive-mcp.algorithms.graph-test))