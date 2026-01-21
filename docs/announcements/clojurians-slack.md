# Clojurians Slack Announcement

**Channel**: #announcements

---

Announcing hive-mcp: LLM Memory + Multi-Agent Coordination in Clojure

Built an MCP server that gives Claude persistent, project-scoped memory and parallel agent coordination.

**The Clojure bits:**
- DataScript for entity persistence (lings, tasks, claims)
- core.logic for conflict detection & deadlock prevention
- mcp-clojure-sdk for the protocol layer
- ~29k LOC of Clojure, REPL-driven throughout

**Key patterns:**
- Progressive crystallization (5-tier memory TTL)
- Advisory locking with queue-based conflicts
- Token hierarchy (expensive coordinator -> cheap drones via OpenRouter)

It's Emacs-integrated but the Clojure backend is the interesting part - especially using core.logic for "is this file claimed by SOMEONE ELSE?" (negation-as-failure) and cycle detection.

Blog post with architecture details: https://www.buddhilw.com/posts-output/2026-01-20-hive-mcp/
Repo: https://github.com/hive-agi/hive-mcp

Built over 28 days, largely self-hosted (Claude using hive-mcp to build hive-mcp).

Feedback welcome, especially on the core.logic patterns!
