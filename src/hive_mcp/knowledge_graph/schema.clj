(ns hive-mcp.knowledge-graph.schema
  "DataScript schema definitions for Knowledge Graph edges.

   The Knowledge Graph captures relationships between knowledge entries
   (stored in Chroma). Nodes are memory entries, edges are relationships.

   This module contains:
   - Edge schema for DataScript
   - Valid relation types (enumeration)
   - Schema documentation

   Architecture:
   - Hybrid storage: Chroma for nodes (semantic search), DataScript for edges (graph queries)
   - See docs/KNOWLEDGE_GRAPH_ARCHITECTURE.md for full design rationale

   SOLID-S: Single Responsibility - only schema definitions.
   DDD: Value Objects for relation enums, schema as domain model.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Relation Types (Value Object / Enumeration)
;;; =============================================================================

(def valid-relations
  "Valid knowledge graph relation types.

   | Relation       | Meaning                          | Example                           |
   |----------------|----------------------------------|-----------------------------------|
   | :implements    | Realizes a principle             | decision implements pattern       |
   | :supersedes    | Replaces older knowledge         | new-convention supersedes old     |
   | :refines       | Improves without replacing       | v2 refines v1                     |
   | :contradicts   | Conflicts with                   | decision-A contradicts decision-B |
   | :depends-on    | Requires                         | snippet depends-on convention     |
   | :derived-from  | Synthetic origin                 | synthesis derived-from thesis     |
   | :applies-to    | Scope of applicability           | convention applies-to submodule   |

   Multi-valued logic: edges have confidence scores 0.0-1.0, not binary truth."
  #{:implements :supersedes :refines :contradicts
    :depends-on :derived-from :applies-to})

(defn valid-relation?
  "Check if a relation keyword is valid.

   Arguments:
     relation - Keyword to validate

   Returns:
     true if relation is in valid-relations set"
  [relation]
  (contains? valid-relations relation))

;;; =============================================================================
;;; Schema Definition
;;; =============================================================================

(def kg-schema
  "DataScript schema for Knowledge Graph edges.

   Design notes:
   - Edges connect memory entry IDs (strings from Chroma)
   - :db/unique on :kg-edge/id for identity lookups
   - Confidence is multi-valued (0.0-1.0) not binary
   - Scope tracks where relationship was discovered
   - Provenance via created-by (agent attribution)

   Edge structure:
     {:kg-edge/id        \"edge-20260120-abc123\"
      :kg-edge/from      \"memory-entry-id-1\"
      :kg-edge/to        \"memory-entry-id-2\"
      :kg-edge/relation  :implements
      :kg-edge/scope     \"hive-mcp:agora\"
      :kg-edge/confidence 0.9
      :kg-edge/created-by \"agent:ling-task-123\"
      :kg-edge/created-at #inst \"2026-01-20T...\"}"

  {:kg-edge/id
   {:db/unique :db.unique/identity
    :db/doc "Unique edge identifier (auto-generated: edge-<timestamp>-<uuid>)"}

   :kg-edge/from
   {:db/doc "Source node ID (memory entry ID from Chroma)"}

   :kg-edge/to
   {:db/doc "Target node ID (memory entry ID from Chroma)"}

   :kg-edge/relation
   {:db/doc "Relation keyword: :implements, :supersedes, :refines, :contradicts, :depends-on, :derived-from, :applies-to"}

   :kg-edge/scope
   {:db/doc "Scope where edge was discovered (e.g., 'hive-mcp:agora', 'global')"}

   :kg-edge/confidence
   {:db/doc "Confidence score 0.0-1.0 (multi-valued logic, not binary)"}

   :kg-edge/created-by
   {:db/doc "Agent ID that created edge (e.g., 'agent:coordinator', 'agent:ling-task-123')"}

   :kg-edge/created-at
   {:db/doc "Timestamp when edge was created (java.util.Date)"}})
