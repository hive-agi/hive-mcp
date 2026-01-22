(ns hive-mcp.knowledge-graph.edges
  "CRUD operations for Knowledge Graph edges.

   Edges represent relationships between knowledge entries (nodes stored in Chroma).
   This module provides:
   - add-edge! - Create edge with validation
   - get-edge - Retrieve edge by ID
   - get-edges-from - Query edges by source node
   - get-edges-to - Query edges by target node
   - get-edges-by-relation - Query edges by relation type
   - get-edges-by-scope - Query edges by scope
   - update-edge-confidence! - Update confidence value
   - remove-edge! - Delete edge

   SOLID-S: Single Responsibility - edge lifecycle only.
   DDD: Repository pattern for KG edges."
  (:require [datascript.core :as d]
            [taoensso.timbre :as log]
            [hive-mcp.knowledge-graph.schema :as schema]
            [hive-mcp.knowledge-graph.connection :as conn]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Helper Functions
;;; =============================================================================

(defn- entity->map
  "Convert DataScript entity to plain map.
   Removes :db/id for clean API responses."
  [entity]
  (when entity
    (dissoc (into {} entity) :db/id)))

;;; =============================================================================
;;; Create Operations
;;; =============================================================================

(defn add-edge!
  "Create a new edge in the Knowledge Graph.

   Arguments:
     opts - Map with keys:
            :from       - Source node ID (required, memory entry ID)
            :to         - Target node ID (required, memory entry ID)
            :relation   - Relation type keyword (required, must be valid)
            :scope      - Scope where edge was discovered (optional)
            :confidence - Confidence score 0.0-1.0 (optional, default 1.0)
            :created-by - Agent ID creating edge (optional)

   Returns:
     The generated edge ID

   Throws:
     AssertionError if relation is invalid or required fields missing

   Example:
     (add-edge! {:from \"memory-123\"
                 :to \"memory-456\"
                 :relation :implements
                 :scope \"hive-mcp\"
                 :confidence 0.9
                 :created-by \"agent:coordinator\"})"
  [{:keys [from to relation scope confidence created-by]
    :or {confidence 1.0}}]
  {:pre [(string? from)
         (string? to)
         (keyword? relation)
         (schema/valid-relation? relation)
         (<= 0.0 confidence 1.0)]}
  (let [c (conn/ensure-conn)
        edge-id (conn/gen-edge-id)
        tx-data (cond-> {:kg-edge/id edge-id
                         :kg-edge/from from
                         :kg-edge/to to
                         :kg-edge/relation relation
                         :kg-edge/confidence confidence
                         :kg-edge/created-at (conn/now)}
                  scope (assoc :kg-edge/scope scope)
                  created-by (assoc :kg-edge/created-by created-by))]
    (log/debug "Adding KG edge:" edge-id relation "from:" from "to:" to)
    (d/transact! c [tx-data])
    edge-id))

;;; =============================================================================
;;; Read Operations
;;; =============================================================================

(defn get-edge
  "Get an edge by its ID.

   Arguments:
     edge-id - Edge identifier

   Returns:
     Map with edge attributes, or nil if not found"
  [edge-id]
  {:pre [(string? edge-id)]}
  (let [c (conn/ensure-conn)
        db @c]
    (entity->map (d/entity db [:kg-edge/id edge-id]))))

(defn get-edges-from
  "Get all edges originating from a node.

   Arguments:
     node-id - Source node ID (memory entry ID)

   Returns:
     Sequence of edge maps"
  [node-id]
  {:pre [(string? node-id)]}
  (let [c (conn/ensure-conn)
        db @c
        eids (d/q '[:find [?e ...]
                    :in $ ?from
                    :where [?e :kg-edge/from ?from]]
                  db node-id)]
    (map #(entity->map (d/entity db %)) eids)))

(defn get-edges-to
  "Get all edges pointing to a node.

   Arguments:
     node-id - Target node ID (memory entry ID)

   Returns:
     Sequence of edge maps"
  [node-id]
  {:pre [(string? node-id)]}
  (let [c (conn/ensure-conn)
        db @c
        eids (d/q '[:find [?e ...]
                    :in $ ?to
                    :where [?e :kg-edge/to ?to]]
                  db node-id)]
    (map #(entity->map (d/entity db %)) eids)))

(defn get-edges-by-relation
  "Get all edges with a specific relation type.

   Arguments:
     relation - Relation keyword (e.g., :implements, :supersedes)

   Returns:
     Sequence of edge maps"
  [relation]
  {:pre [(keyword? relation)]}
  (let [c (conn/ensure-conn)
        db @c
        eids (d/q '[:find [?e ...]
                    :in $ ?rel
                    :where [?e :kg-edge/relation ?rel]]
                  db relation)]
    (map #(entity->map (d/entity db %)) eids)))

(defn get-edges-by-scope
  "Get all edges discovered in a specific scope.

   Arguments:
     scope - Scope string (e.g., 'hive-mcp', 'hive-mcp:agora', 'global')

   Returns:
     Sequence of edge maps"
  [scope]
  {:pre [(string? scope)]}
  (let [c (conn/ensure-conn)
        db @c
        eids (d/q '[:find [?e ...]
                    :in $ ?scope
                    :where [?e :kg-edge/scope ?scope]]
                  db scope)]
    (map #(entity->map (d/entity db %)) eids)))

(defn get-all-edges
  "Get all edges in the Knowledge Graph.

   Returns:
     Sequence of edge maps"
  []
  (let [c (conn/ensure-conn)
        db @c
        eids (d/q '[:find [?e ...]
                    :where [?e :kg-edge/id _]]
                  db)]
    (map #(entity->map (d/entity db %)) eids)))

(defn find-edge
  "Find an edge between two specific nodes with optional relation filter.

   Arguments:
     from     - Source node ID
     to       - Target node ID
     relation - Optional relation type filter

   Returns:
     Edge map if found, nil otherwise"
  ([from to]
   (find-edge from to nil))
  ([from to relation]
   {:pre [(string? from) (string? to)]}
   (let [c (conn/ensure-conn)
         db @c
         query (if relation
                 '[:find ?e .
                   :in $ ?from ?to ?rel
                   :where
                   [?e :kg-edge/from ?from]
                   [?e :kg-edge/to ?to]
                   [?e :kg-edge/relation ?rel]]
                 '[:find ?e .
                   :in $ ?from ?to
                   :where
                   [?e :kg-edge/from ?from]
                   [?e :kg-edge/to ?to]])
         args (if relation [db from to relation] [db from to])
         eid (apply d/q query args)]
     (when eid
       (entity->map (d/entity db eid))))))

;;; =============================================================================
;;; Update Operations
;;; =============================================================================

(defn update-edge-confidence!
  "Update the confidence score of an edge.

   Arguments:
     edge-id    - Edge to update
     confidence - New confidence score (0.0-1.0)

   Returns:
     Transaction report or nil if edge not found"
  [edge-id confidence]
  {:pre [(string? edge-id)
         (<= 0.0 confidence 1.0)]}
  (let [c (conn/ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:kg-edge/id edge-id]))]
      (log/debug "Updating edge confidence:" edge-id "to:" confidence)
      (d/transact! c [{:db/id eid :kg-edge/confidence confidence}]))))

(defn increment-confidence!
  "Increment edge confidence by delta (clamped to [0.0, 1.0]).

   Used for Socratic validation: valid assertion increases confidence.

   Arguments:
     edge-id - Edge to update
     delta   - Amount to add (can be negative)

   Returns:
     New confidence value, or nil if edge not found"
  [edge-id delta]
  {:pre [(string? edge-id) (number? delta)]}
  (let [c (conn/ensure-conn)
        db @c]
    (when-let [entity (d/entity db [:kg-edge/id edge-id])]
      (let [current (or (:kg-edge/confidence entity) 1.0)
            new-conf (-> (+ current delta)
                         (max 0.0)
                         (min 1.0))]
        (d/transact! c [{:db/id (:db/id entity) :kg-edge/confidence new-conf}])
        new-conf))))

;;; =============================================================================
;;; Delete Operations
;;; =============================================================================

(defn remove-edge!
  "Delete an edge from the Knowledge Graph.

   Arguments:
     edge-id - Edge to remove

   Returns:
     Transaction report or nil if edge not found"
  [edge-id]
  {:pre [(string? edge-id)]}
  (let [c (conn/ensure-conn)
        db @c]
    (when-let [eid (:db/id (d/entity db [:kg-edge/id edge-id]))]
      (log/debug "Removing KG edge:" edge-id)
      (d/transact! c [[:db/retractEntity eid]]))))

(defn remove-edges-for-node!
  "Remove all edges connected to a node (both incoming and outgoing).

   Useful when a memory entry is deleted from Chroma.

   Arguments:
     node-id - Node whose edges should be removed

   Returns:
     Count of edges removed"
  [node-id]
  {:pre [(string? node-id)]}
  (let [c (conn/ensure-conn)
        db @c
        ;; Find all edges where node is source or target
        eids (d/q '[:find [?e ...]
                    :in $ ?nid
                    :where
                    (or [?e :kg-edge/from ?nid]
                        [?e :kg-edge/to ?nid])]
                  db node-id)]
    (when (seq eids)
      (log/debug "Removing" (count eids) "edges for node:" node-id)
      (d/transact! c (mapv (fn [eid] [:db/retractEntity eid]) eids)))
    (count eids)))

;;; =============================================================================
;;; Statistics
;;; =============================================================================

(defn edge-stats
  "Get statistics about the Knowledge Graph edges.

   Returns:
     Map with:
       :total-edges      - Total edge count
       :by-relation      - Count per relation type
       :by-scope         - Count per scope"
  []
  (let [c (conn/ensure-conn)
        db @c
        edges (get-all-edges)
        by-relation (frequencies (map :kg-edge/relation edges))
        by-scope (frequencies (keep :kg-edge/scope edges))]
    {:total-edges (count edges)
     :by-relation by-relation
     :by-scope by-scope}))
