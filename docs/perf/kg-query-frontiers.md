# KG query cost: frontier batching

## Ownership and attribution limits

The metered entry is `hive-mcp.protocols.kg/query`, reached through
`hive-mcp.knowledge-graph.connection/query` and `knowledge-graph.slots/query`.
It delegates to `hive-spi.kg.protocol/query`; the Datahike implementation is
`hive-datahike.kg.store/DatahikeStore` in the sibling hive-datahike repo.

Read-only live snapshot on 2026-10-24, nREPL 7910:

| Backend / operation | Calls | Total | Max | Errors |
|---|---:|---:|---:|---:|
| Datahike query | 66,620 | 600.261 s | 84.878 s | 14 |
| Datalevin query | 46,615 | 509.149 s | 18.622 s | 0 |

Datahike p50 0.524 ms, p95 8.389 ms, p99 33.554 ms. A later snapshot
was already at 67,007 Datahike queries: these counters cover concurrent
activity and cannot be attributed to the tools invoked by this investigation.

The installed StatsSink is a counter/total/max plus 64-bucket histogram,
not a ring of events. Its observe interface receives only key, elapsed time,
and outcome. It retains no query text, caller, event timestamp, or exception.
Consequently **the 84.878-second cause and historical top-caller counts are
unknown**. Neither GC nor lock contention nor a particular query shape was
established. No tracers, var changes, reloads, tests, or writes were made in
the live JVM. The clojure_eval session bridge failed for 7910; bounded,
read-only nREPL evals were sent over a direct socket instead.

Static Carto callers (stale index, so not a runtime ranking):

* connection/query: 77 call sites / 51 distinct functions overall; 30 sites /
  18 functions under src in the indexed report. The report separately puts
  17 sites in other worktrees and 30 in tests; it is not a complete census.
* edges/get-edges-from: 12 src sites / 10 functions. Includes traversal
  adjacency, node context, DAG dependency reads, kanban summary, memory
  retrieval, and synthetic cleanup.
* queries/traverse: 3 src callers: impact-analysis, find-connected-nodes,
  and the KG traverse tool handler.
* Memory retrieval and kanban get each read outgoing and incoming edges for
  one id (two queries). Batch edge accessors already exist. This investigation
  did not establish that catchup/frontier or memory-entry enrichment accounts
  for the aggregate 56k/67k queries.

## Change

`queries/traverse` now reads a BFS frontier in chunks of at most 128 queue
entries via the existing IEdgeReader batch accessors. It preserves per-node
visited updates, parallel-edge behavior, scope/relation filtering before
node discovery, depth cutoff, and outgoing-before-incoming concatenation.
It does not expand the depth boundary or cache graph rows across requests.
A batch failure propagates rather than masquerading as an empty graph.

The optional `:edge-reader` provides explicit dependency injection for tests
and the benchmark, with the existing default reader used by production.
Maximum input batch size is bounded, not the degree of a single node: a
high-degree node can still produce a large pull. Snapshot semantics are no
stronger than before, though nodes in one batch now share a query snapshot.
Database result order is unspecified; exact parent selection among equal
shortest paths is therefore not promised across different backend queries.

## Isolated benchmark

Own nREPL JVM with `-Xmx1500m`, Datahike 0.8.1910; production-resolved
classpath with this worktree's src/test/resources/dev first. No production
DB path used. `kg-frontier-bench/run!` creates a randomly identified memory
store, loads the production 001 edge schema, transacts 30,600 synthetic edges
in batches of 1,000, then closes the scratch connection in finally. Query
result cache is disabled by normal store initialization.

Shape: root -> 300 intermediates -> 300 leaves; 30,000 unrelated background
edges with shared sources. Indexed from/to/scope, keyword relations,
confidence and creation time follow production edge attributes. This is a
synthetic fanout regression workload, not a production graph export or a
claim about production speedup. Reader calls use the real DatahikeStore
bounded-read pool and scalar/collection-binding query shapes from the edge
accessors.

Two warmup pairs, six measured pairs alternating AB/BA; direction both,
max depth 2. All pairs assert equal sets of complete result rows.

| Pair | Before ms | After ms | Before queries | After queries |
|---|---:|---:|---:|---:|
| 1 (AB) | 303.487 | 24.604 | 602 | 8 |
| 2 (BA) | 280.300 | 20.414 | 602 | 8 |
| 3 (AB) | 277.785 | 28.793 | 602 | 8 |
| 4 (BA) | 313.891 | 24.566 | 602 | 8 |
| 5 (AB) | 328.295 | 33.122 | 602 | 8 |
| 6 (BA) | 257.133 | 29.205 | 602 | 8 |

Median 291.893 -> 26.699 ms (**10.93x**); mean 293.482 -> 26.784 ms.
Query count reduction **98.67%**. Both return 600 rows.

Reproduce in an isolated nREPL with this repo's src/test/dev/resources and
hive-datahike on its classpath:

```clojure
(require 'kg-frontier-bench)
(kg-frontier-bench/run!)
(require 'hive-mcp.knowledge-graph.frontier-batch-test
         'hive-mcp.knowledge-graph.queries-test)
(clojure.test/run-tests 'hive-mcp.knowledge-graph.frontier-batch-test
                        'hive-mcp.knowledge-graph.queries-test)
```

Focused tests: **24 tests / 265 assertions, zero failures/errors**. New tests
use an explicit IEdgeReader, no with-redefs; compare the old scalar BFS over
216 direction/depth/start/relation combinations, cover parallel edges,
cycles, absent nodes, 128-entry chunk boundaries, no depth-boundary reads,
scope filtering before discovery, and propagated batch failures. Existing
DataScript traversal and tool-handler tests also pass.
