(ns hive-mcp.algorithms.graph-test
  "Tests for graph algorithms including construction, traversal, and pathfinding."
  (:require [clojure.test :refer [deftest testing is]]
            [hive-mcp.algorithms.graph.domain :as domain]
            [hive-mcp.algorithms.graph.impl :as impl]))

;; Helper functions for test assertions
(defn graph-nodes
  "Get all node ids from a graph."
  [graph]
  (keys (:nodes graph)))

(defn graph-edges
  "Get all edges as [from to] pairs from a graph."
  [graph]
  (for [[from edges] (:adj-map graph)
        {:keys [to]} edges]
    [from to]))

;; Test Fixtures

(def simple-graph
  "Simple directed graph: A->B->C, A->D"
  (-> (impl/empty-graph)
      (domain/add-node :A (domain/make-node :A))
      (domain/add-node :B (domain/make-node :B))
      (domain/add-node :C (domain/make-node :C))
      (domain/add-node :D (domain/make-node :D))
      (domain/add-edge :A :B 1)
      (domain/add-edge :B :C 1)
      (domain/add-edge :A :D 1)))

(def weighted-graph
  "Weighted graph for Dijkstra's algorithm testing"
  (-> (impl/empty-graph)
      (domain/add-node :A (domain/make-node :A))
      (domain/add-node :B (domain/make-node :B))
      (domain/add-node :C (domain/make-node :C))
      (domain/add-node :D (domain/make-node :D))
      (domain/add-edge :A :B 1)
      (domain/add-edge :A :C 4)
      (domain/add-edge :B :C 2)
      (domain/add-edge :B :D 5)
      (domain/add-edge :C :D 1)))

(def cyclic-graph
  "Graph with a cycle: A->B->C->A"
  (-> (impl/empty-graph)
      (domain/add-node :A (domain/make-node :A))
      (domain/add-node :B (domain/make-node :B))
      (domain/add-node :C (domain/make-node :C))
      (domain/add-edge :A :B 1)
      (domain/add-edge :B :C 1)
      (domain/add-edge :C :A 1)))

(def disconnected-graph
  "Graph with isolated node: A->B, C (isolated)"
  (-> (impl/empty-graph)
      (domain/add-node :A (domain/make-node :A))
      (domain/add-node :B (domain/make-node :B))
      (domain/add-node :C (domain/make-node :C))
      (domain/add-edge :A :B 1)))

;; Tests

(deftest graph-construction-test
  ;; Test graph construction operations.
  (testing "Empty graph creation"
    (let [g (impl/empty-graph)]
      (is (empty? (graph-nodes g)) "Empty graph should have no nodes")
      (is (empty? (graph-edges g)) "Empty graph should have no edges")))

  (testing "Adding nodes"
    (let [g (-> (impl/empty-graph)
                (domain/add-node :A (domain/make-node :A))
                (domain/add-node :B (domain/make-node :B)))]
      (is (= #{:A :B} (set (graph-nodes g))) "Graph should contain added nodes")))

  (testing "Adding edges"
    (let [g (-> (impl/empty-graph)
                (domain/add-node :A (domain/make-node :A))
                (domain/add-node :B (domain/make-node :B))
                (domain/add-edge :A :B 1))]
      (is (= #{[:A :B]} (set (graph-edges g))) "Graph should contain added edge")))

  (testing "Creating graph from edges"
    (let [g (impl/graph-from-edges [{:from :A :to :B} {:from :B :to :C} {:from :A :to :D}])]
      (is (= #{:A :B :C :D} (set (graph-nodes g))) "Graph should contain all nodes from edges")
      (is (= #{[:A :B] [:B :C] [:A :D]} (set (graph-edges g))) "Graph should contain all edges"))))

(deftest bfs-test
  ;; Test BFS traversal algorithm.
  (testing "Basic BFS traversal"
    (let [result (impl/bfs simple-graph :A)]
      (is (some #{:A} result) "BFS should visit starting node")
      (is (some #{:B} result) "BFS should visit connected nodes")
      (is (some #{:C} result) "BFS should visit nodes at depth 2")
      (is (some #{:D} result) "BFS should visit all reachable nodes")))

  (testing "BFS returns correct order"
    (let [result (impl/bfs simple-graph :A)
          idx-map (into {} (map-indexed (fn [i v] [v i]) result))]
      (is (< (idx-map :A) (idx-map :B)) "A should be visited before B")
      (is (< (idx-map :A) (idx-map :D)) "A should be visited before D")
      (is (< (idx-map :B) (idx-map :C)) "B should be visited before C")))

  (testing "BFS handles disconnected graphs"
    (let [result (impl/bfs disconnected-graph :A)]
      (is (not (some #{:C} result)) "BFS should not visit disconnected nodes"))))

(deftest dfs-test
  ;; Test DFS traversal algorithm.
  (testing "Basic DFS traversal"
    (let [result (impl/dfs simple-graph :A)]
      (is (some #{:A} result) "DFS should visit starting node")
      (is (some #{:B} result) "DFS should visit connected nodes")
      (is (some #{:C} result) "DFS should visit nodes at depth 2")
      (is (some #{:D} result) "DFS should visit all reachable nodes")))

  (testing "DFS handles cycles"
    (let [result (impl/dfs cyclic-graph :A)]
      (is (some #{:A} result) "DFS should visit starting node")
      (is (some #{:B} result) "DFS should visit connected nodes")
      (is (some #{:C} result) "DFS should visit nodes in cycle")))

  (testing "DFS returns correct order (one possible valid order)"
    (let [result (impl/dfs simple-graph :A)
          idx-map (into {} (map-indexed (fn [i v] [v i]) result))]
      ;; DFS order can vary, but starting node should be first
      (is (= 0 (idx-map :A)) "Starting node should be visited first"))))

(deftest dijkstra-test
  ;; Test Dijkstra's shortest path algorithm.
  (testing "Finds shortest path"
    (let [result (impl/dijkstra weighted-graph :A :D)]
      (is (= [:A :B :C :D] (:path result)) "Should find shortest path A->B->C->D")))

  (testing "Returns nil for unreachable nodes"
    (let [g (-> (impl/empty-graph)
                (domain/add-node :A (domain/make-node :A))
                (domain/add-node :B (domain/make-node :B)))
          result (impl/dijkstra g :A :B)]
      (is (nil? result) "Should return nil for unreachable nodes")))

  (testing "Handles weighted edges correctly"
    (let [result (impl/dijkstra weighted-graph :A :C)]
      (is (= [:A :B :C] (:path result)) "Should prefer path with lower total weight"))))

(deftest edge-cases-test
  ;; Test edge cases in graph algorithms.
  (testing "Empty graph"
    (let [g (impl/empty-graph)]
      (is (empty? (impl/bfs g :A)) "BFS on empty graph should return empty result")
      (is (empty? (impl/dfs g :A)) "DFS on empty graph should return empty result")
      (is (nil? (impl/dijkstra g :A :B)) "Dijkstra on empty graph should return nil")))

  (testing "Single node graph"
    (let [g (domain/add-node (impl/empty-graph) :A (domain/make-node :A))]
      (is (= [:A] (impl/bfs g :A)) "BFS on single node should return just that node")
      (is (= [:A] (impl/dfs g :A)) "DFS on single node should return just that node")))

  (testing "Self-loop"
    (let [g (-> (impl/empty-graph)
                (domain/add-node :A (domain/make-node :A))
                (domain/add-edge :A :A 1))]
      (is (some #{:A} (impl/bfs g :A)) "BFS should handle self-loops")
      (is (some #{:A} (impl/dfs g :A)) "DFS should handle self-loops"))))

;; Run tests when this namespace is loaded
(comment
  (clojure.test/run-tests 'hive-mcp.algorithms.graph-test))
