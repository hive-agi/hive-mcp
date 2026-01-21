# Knowledge Graph Architecture for Autonomous Swarms

## Position in the Philosophical Stack

```
┌─────────────────────────────────────────┐
│     General Semantics (Epistemology)    │
│   "How do we know what we know?"        │
│   Map ≠ Territory, Non-Allness, etc.   │
└─────────────────────┬───────────────────┘
                      │ grounds
                      ▼
┌─────────────────────────────────────────┐
│     Knowledge Graph (Ontology)          │
│   "What do we know and how does it      │  ← THIS DOCUMENT
│    relate?"                             │
└─────────────────────┬───────────────────┘
                      │ structures
                      ▼
┌─────────────────────────────────────────┐
│     Hellenic Patterns (Discourse)       │
│   "How do we dialogue toward truth?"    │
│   Agora, Dialectic, Socratic, etc.     │
└─────────────────────┬───────────────────┘
                      │ implements
                      ▼
┌─────────────────────────────────────────┐
│     Hive Principles (Operations)        │
│   "How do we coordinate work?"          │
│   Token hierarchy, Trust axioms, etc.  │
└─────────────────────────────────────────┘
```

## Core Premise

Memory entries in isolation lose meaning. A decision "use X pattern" gains context when we know:
- What it supersedes
- What principle it implements
- Who discovered it and when
- What depends on it

The Knowledge Graph captures **relationships** between knowledge, not just the knowledge itself.

---

## Design Constraints

### 1. Project-Scoped

Each project maintains its own knowledge graph. Cross-project knowledge requires explicit promotion to global scope.

```
Why: Prevent knowledge leakage between unrelated projects.
     A convention for project-A shouldn't pollute project-B.
```

### 2. Hierarchically Aware

Projects can have submodules. The graph understands nesting and inheritance.

```
Why: Monorepos and submodules are common. A parent project's
     conventions should apply to children automatically.
```

### 3. Git-Aligned

Project boundaries align with git repositories and submodules.

```
Why: Developers already think in terms of repos. The knowledge
     graph should match their mental model.
```

---

## Hierarchical Structure

### Scope Levels

```
global/                                    ← Foundational (all projects)
│
├── hellenic-patterns/
│   ├── agora
│   ├── dialectic
│   ├── socratic
│   └── ...
│
├── general-semantics/
│   ├── map-territory
│   ├── non-allness
│   └── ...
│
└── hive-principles/
    ├── token-hierarchy
    ├── trust-axioms
    └── ...

hive-mcp/                                  ← Project scope
│
├── [project-level knowledge]
│   ├── swarm-conventions
│   ├── memory-patterns
│   └── cider-protocol
│
├── agora/                                 ← Submodule scope
│   ├── nash-equilibrium-decision
│   ├── dialogue-protocol
│   └── [inherits: hive-mcp/*, global/*]
│
└── tools/                                 ← Another submodule
    ├── drone-patterns
    └── [inherits: hive-mcp/*, global/*]

other-project/                             ← Isolated project
│
└── [cannot see hive-mcp/* unless in global/]
```

### Inheritance Rules

| Direction | Behavior | Example |
|-----------|----------|---------|
| **Down** (parent → child) | Automatic | `hive-mcp:convention-X` visible in `hive-mcp:agora` |
| **Up** (child → parent) | Explicit promotion only | `agora:discovery-Y` stays local until promoted |
| **Across** (sibling → sibling) | Via common ancestor | `agora` and `tools` share only `hive-mcp/*` |
| **Cross-project** | Via global only | `sisf` sees `global/*` but not `hive-mcp/*` |

---

## Graph Structure

### Nodes (Knowledge Entries)

Each node represents a knowledge entry:

```clojure
{:id "20260120-abc123"
 :type :decision           ; :convention, :snippet, :note, :session
 :content "..."
 :scope "hive-mcp:agora"   ; Where this knowledge lives
 :created "2026-01-20"
 :created-by "agent:ling-task-123"
 :tags ["architecture" "agora"]}
```

### Edges (Relationships)

Edges capture how knowledge relates:

```clojure
{:from "hive-mcp:agora:nash-decision"
 :to "global:hellenic:telos"
 :relation :implements
 :discovered-at "hive-mcp:agora"    ; Scope where relationship was found
 :discovered-by "agent:coordinator"
 :confidence 0.9}                    ; Multi-valued, not binary
```

### Relationship Types

| Relation | Meaning | Example |
|----------|---------|---------|
| `:implements` | Realizes a principle | decision implements pattern |
| `:supersedes` | Replaces older knowledge | new-convention supersedes old |
| `:refines` | Improves without replacing | v2 refines v1 |
| `:contradicts` | Conflicts with | decision-A contradicts decision-B |
| `:depends-on` | Requires | snippet depends-on convention |
| `:discovered-by` | Provenance | knowledge discovered-by session |
| `:applies-to` | Scope of applicability | convention applies-to submodule |
| `:derived-from` | Synthetic origin | synthesis derived-from thesis + antithesis |

---

## Submodule Detection

### Via `.hive-project.edn`

```clojure
;; /project-root/.hive-project.edn
{:project-id "hive-mcp"
 :parent-id nil}           ; Root project

;; /project-root/agora/.hive-project.edn
{:project-id "hive-mcp:agora"
 :parent-id "hive-mcp"}    ; Points to parent
```

### Via Git Submodules

```bash
# Automatically detect from .gitmodules
[submodule "agora"]
    path = src/hive_mcp/agora
    url = ...
```

### Via Directory Convention

```
# Infer from nested .hive-project.edn files
# or from directory structure matching known patterns:
# - src/*/          → potential submodules
# - packages/*/     → monorepo packages
# - modules/*/      → explicit modules
```

---

## Knowledge Flow Patterns

### Pattern 1: Percolate Down (Automatic Inheritance)

```
global:convention-X
    │
    └──[inherits]──→ hive-mcp (sees convention-X)
                         │
                         └──[inherits]──→ hive-mcp:agora (sees convention-X)
```

Child scopes automatically see parent knowledge. No action required.

### Pattern 2: Bubble Up (Explicit Promotion)

```
hive-mcp:agora:local-pattern-Y
    │
    ├── [default: stays local to agora]
    │
    └── [explicit: promote-knowledge!]
            │
            └──→ hive-mcp:pattern-Y (now visible to all hive-mcp submodules)
                     │
                     └── [explicit: promote-knowledge!]
                             │
                             └──→ global:pattern-Y (now visible to all projects)
```

Promotion requires explicit action. This prevents noise propagation.

### Pattern 3: Cross-Pollination (Via Global)

```
project-A:valuable-insight
    │
    └── [promote to global]
            │
            global:valuable-insight
            │
            └── [inherits down]
                    │
                    └──→ project-B can now see valuable-insight
```

Projects share knowledge only through the global scope.

---

## Hellenic Pattern Integration

The knowledge graph is the **arena** where Hellenic patterns operate:

### Agora → Creates Nodes and Edges

```
Dialogue begins
    │
    ├── Position nodes created for each participant
    │
    ├── Challenge edges connect opposing positions
    │
    └── Synthesis node emerges, edges to sources marked :derived-from
```

### Dialectic → Transforms Edges

```
Thesis (node) ←─[:contradicts]─→ Antithesis (node)
                    │
                    │ [dialectic resolution]
                    ▼
              Synthesis (node)
                    │
                    ├──[:derived-from]──→ Thesis
                    └──[:derived-from]──→ Antithesis

Thesis & Antithesis marked [:superseded-by] → Synthesis
```

### Socratic Method → Validates Edges

```
Assumption (node) ──[:implements]──→ Principle (node)
                          │
                          │ [socratic questioning]
                          ▼
              Edge validated OR contradiction found
                          │
                          ├── Valid: edge.confidence += 0.1
                          └── Invalid: edge removed, :contradicts edge added
```

### Time-Binding → Graph Growth

```
Session 1: Graph has N nodes, M edges
Session 2: Catchup loads graph, session adds nodes/edges
Session N: Graph has grown, evolved, pruned
    │
    └── Crystallization adds new knowledge
    └── Expiration prunes stale knowledge
    └── Promotion elevates valuable knowledge
```

---

## General Semantics Integration

### Map ≠ Territory

The knowledge graph IS a map. It represents our understanding, not reality.

```clojure
;; Every node should have epistemic metadata
{:id "..."
 :confidence 0.8           ; Not certain
 :last-validated "2026-01"  ; May be stale
 :context ["test-env"]      ; May not apply elsewhere
 }
```

### Non-Allness

The graph is always incomplete. We can never capture "all" relationships.

```clojure
;; Queries should acknowledge incompleteness
(query-knowledge {:relation :implements
                  :warning "Results may be incomplete"})
```

### Abstraction Levels (Structural Differential)

```
Event Level     → [not in graph - reality itself]
Object Level    → Snippets (concrete code examples)
Descriptive     → Conventions (first-order patterns)
Inferential     → Decisions (higher-order choices)
Meta-levels     → Principles (abstractions about abstractions)
```

Nodes have an implicit abstraction level. Relationships should respect levels:
- Snippets `:demonstrates` Conventions (up one level)
- Conventions `:implements` Decisions (up one level)
- Cross-level jumps should be explicit and questioned

---

## Query Patterns

### Scope-Aware Queries

```clojure
;; Query from a specific scope
(query-knowledge
  {:from-scope "hive-mcp:agora"   ; Sees: agora + hive-mcp + global
   :relation :implements})

;; Query with inheritance
(query-knowledge
  {:from-scope "hive-mcp"          ; Sees: hive-mcp + global (NOT agora internals)
   :include-children false})        ; Don't look into submodules

;; Query only global
(query-knowledge
  {:from-scope "global"
   :relation :supersedes})
```

### Traversal Queries

```clojure
;; What depends on this convention?
(traverse-graph
  {:start "hive-mcp:convention-X"
   :direction :incoming
   :relation :depends-on
   :max-depth 3})

;; What principles does this decision implement?
(traverse-graph
  {:start "hive-mcp:agora:nash-decision"
   :direction :outgoing
   :relation :implements})

;; Find contradictions
(find-contradictions {:scope "hive-mcp"})
```

### Impact Analysis

```clojure
;; Before changing a convention, what might break?
(impact-analysis
  {:node "hive-mcp:convention-X"
   :change :modify})
;; Returns all nodes with :depends-on edges to X
```

---

## Implementation Architecture

### Storage Layer

```
Option A: Extend Chroma
  - Add relationship metadata to entries
  - Use metadata queries for edge traversal
  - Pro: Single storage system
  - Con: Not optimized for graph queries

Option B: DataScript Graph
  - Dedicated graph in DataScript
  - Already used for swarm state
  - Pro: Datalog queries, in-memory speed
  - Con: Not persistent across restarts (need serialization)

Option C: Hybrid
  - Chroma for node content (semantic search)
  - DataScript for edges (graph queries)
  - Pro: Best of both
  - Con: Sync complexity

Recommendation: Option C (Hybrid)
```

### API Surface

```clojure
;; Node operations (delegate to memory system)
(kg/add-node! {:type :decision :content "..." :scope "hive-mcp"})
(kg/get-node "node-id")
(kg/query-nodes {:type :convention :scope "hive-mcp"})

;; Edge operations (new)
(kg/add-edge! {:from "node-a" :to "node-b" :relation :implements})
(kg/get-edges {:from "node-a"})
(kg/get-edges {:to "node-b" :relation :depends-on})

;; Graph operations (new)
(kg/traverse {:start "node-a" :direction :outgoing :max-depth 2})
(kg/find-path {:from "node-a" :to "node-b"})
(kg/subgraph {:scope "hive-mcp:agora"})

;; Scope operations (new)
(kg/promote! "node-id" :to-scope "hive-mcp")  ; Bubble up
(kg/visible-from "hive-mcp:agora")             ; What can this scope see?
```

---

## Migration Path

### Phase 1: Edge Metadata in Chroma

Add relationship fields to existing memory entries:

```clojure
{:id "..."
 :type :decision
 :content "..."
 ;; New fields
 :kg-supersedes ["old-decision-id"]
 :kg-implements ["principle-id"]
 :kg-scope "hive-mcp:agora"}
```

### Phase 2: DataScript Edge Store

Dedicated DataScript DB for edges:

```clojure
(def kg-schema
  {:kg/from   {:db/type :db.type/ref}
   :kg/to     {:db/type :db.type/ref}
   :kg/rel    {:db/type :db.type/keyword}
   :kg/scope  {:db/type :db.type/string}
   :kg/conf   {:db/type :db.type/float}})
```

### Phase 3: Scope Hierarchy

Implement `.hive-project.edn` parent-child linking:

```clojure
(defn visible-scopes [scope]
  (loop [s scope, acc [scope]]
    (if-let [parent (get-parent-scope s)]
      (recur parent (conj acc parent))
      (conj acc "global"))))
```

### Phase 4: Query API

Build traversal and analysis functions on top of storage.

### Phase 5: Hellenic Integration

Wire Agora/Dialectic/Socratic patterns to create/modify graph.

---

## Eudaimonia Metrics for Knowledge Graph

| Metric | Healthy | Unhealthy |
|--------|---------|-----------|
| Graph connectivity | Nodes have edges | Orphan nodes |
| Contradiction rate | Low, resolved quickly | High, unresolved |
| Promotion rate | Valuable knowledge bubbles up | Everything stays local |
| Staleness | Recent validation timestamps | Old, unvalidated nodes |
| Depth distribution | Balanced abstraction levels | All snippets, no principles |

---

## Summary

The Knowledge Graph adds **ontological structure** to hive-mcp's memory system:

1. **Nodes** are knowledge entries (already exist in Chroma)
2. **Edges** capture relationships (new capability)
3. **Scopes** provide project isolation with inheritance (new capability)
4. **Traversal** enables impact analysis and discovery (new capability)

This transforms flat memory into a living, interconnected knowledge base that grows smarter across sessions while respecting project boundaries.

---

*"Knowledge is a graph, not a list. Relationships are as important as facts."*
