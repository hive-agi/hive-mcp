(ns hive-mcp.algorithms.graph.domain)

;; VALUE OBJECTS (records)
(defrecord Node [id data])
(defrecord Edge [from to weight])

;; GRAPH PROTOCOL (DIP - Dependency Inversion)
(defprotocol IGraph
  "Protocol for graph operations."
  (nodes [this] "Return all nodes")
  (edges [this] "Return all edges")
  (neighbors [this node-id] "Return neighbor node-ids")
  (add-node [this node] "Add a node, return new graph")
  (add-edge [this edge] "Add an edge, return new graph")
  (has-node? [this node-id] "Check if node exists")
  (has-edge? [this from-id to-id] "Check if edge exists")
  (get-edge-weight [this from-id to-id] "Get weight of edge, nil if not exists"))

;; PURE HELPER FUNCTIONS
(defn make-node
  "Factory function for creating a Node.
  
  Arity 1: Creates a node with the given id and no data.
  Arity 2: Creates a node with the given id and data."
  ([id] (->Node id nil))
  ([id data] (->Node id data)))

(defn make-edge
  "Factory function for creating an Edge.
  
  Arity 2: Creates an edge with the given from and to nodes, and default weight 1.
  Arity 3: Creates an edge with the given from, to nodes, and weight."
  ([from to] (->Edge from to 1))
  ([from to weight] (->Edge from to weight)))

(defn node-id
  "Extracts the id from a node. Works with Node record or raw id."
  [node]
  (if (instance? Node node)
    (:id node)
    node))