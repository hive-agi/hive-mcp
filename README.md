# emacs-mcp

**Your AI finally remembers.**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![MCP](https://img.shields.io/badge/MCP-Compatible-green.svg)](https://modelcontextprotocol.io)
[![Emacs](https://img.shields.io/badge/Emacs-28.1+-purple.svg)](https://www.gnu.org/software/emacs/)

> *"Every other AI tool forgets everything between sessions. This one doesn't."*

---

## The Problem

You're deep in a debugging session with Claude. You've explained the architecture, the constraints, the patterns. Then you hit the context limit. New session. **Claude forgets everything.**

Or worse: you come back tomorrow. Same project. Same questions. Same explanations. **Every. Single. Time.**

## The Solution

```
Session 1                         Session 2
─────────────────────────────────────────────────
You: "Our auth uses JWT with..."
Claude: *learns*                  You: /catchup
         ↓                        Claude: "I see from memory:
     /wrap                         - Auth uses JWT with refresh
         ↓                         - Convention: validate at boundaries
    [Memory]  ─────────────────►   - Decision: chose bcrypt over argon2
                                   What should we work on?"
```

**emacs-mcp** gives Claude persistent, project-scoped memory with semantic search. Conventions, decisions, snippets—stored locally, queryable by meaning, never forgotten.

---

## Key Features

### 1. Persistent Project Memory

```elisp
;; Claude stores what it learns
(mcp_memory_add :type "convention"
                :content "Always validate at API boundaries"
                :tags ["security" "validation"])

;; And retrieves it next session
(mcp_memory_search_semantic :query "input validation patterns")
;; => Finds the convention, even without exact keywords
```

**Memory types:** notes, conventions, decisions, snippets
**Durations:** ephemeral (1 day) → short (7 days) → long (90 days) → permanent
**Search:** Keyword or semantic (via local Ollama embeddings)

### 2. Session Continuity

```bash
# End of session
/wrap   # Stores accomplishments, decisions, conventions

# Start of next session
/catchup   # Restores full context from memory
```

No more re-explaining your codebase. No more lost context.

### 3. Multi-Agent Swarm

```elisp
;; Spawn parallel Claude instances with specialized roles
(swarm_spawn :name "tester" :preset "tdd")
(swarm_spawn :name "reviewer" :preset "code-review")

;; Dispatch tasks
(swarm_dispatch :slave-id "tester" :prompt "Write tests for auth.clj")

;; Human-in-the-loop for risky operations
;; File claims prevent conflicts between agents
```

### 4. Full Emacs Integration

50+ MCP tools for buffer management, git (Magit), projects (Projectile), Clojure (CIDER), and org-mode. Claude controls your editor, not just reads files.

---

## Quick Start

### 1. Install

```bash
git clone https://github.com/BuddhiLW/emacs-mcp.git
cd emacs-mcp
```

### 2. Configure Claude Code

Add to `~/.claude.json`:

```json
{
  "mcpServers": {
    "emacs-mcp": {
      "command": "clojure",
      "args": ["-X:mcp"],
      "cwd": "/path/to/emacs-mcp"
    }
  }
}
```

### 3. Enable in Emacs

```elisp
;; In init.el
(add-to-list 'load-path "/path/to/emacs-mcp/elisp")
(require 'emacs-mcp)
(emacs-mcp-mode 1)
(server-start)  ; Required for emacsclient
```

### 4. (Optional) Enable Semantic Search

```bash
# Local embeddings via Ollama
ollama pull nomic-embed-text

# Vector database via Docker
docker compose up -d
```

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         Claude                              │
│                           │                                 │
│                     MCP Protocol                            │
│                           │                                 │
├───────────────────────────┼─────────────────────────────────┤
│                           ▼                                 │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              emacs-mcp (Clojure)                    │   │
│  │                                                     │   │
│  │  Memory ──► JSON + Chroma (semantic)               │   │
│  │  Channel ──► Unix socket (push events)             │   │
│  │  Swarm ──► Parallel Claude instances               │   │
│  └─────────────────────────────────────────────────────┘   │
│                           │                                 │
│                     emacsclient                             │
│                           │                                 │
│                           ▼                                 │
│  ┌─────────────────────────────────────────────────────┐   │
│  │                 Emacs (daemon)                      │   │
│  │                                                     │   │
│  │  emacs-mcp.el ──► Memory, Context, Workflows       │   │
│  │  Addons ──► Magit, CIDER, Projectile, Org...       │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

---

## Tools Reference

<details>
<summary><b>Memory & Context</b> (click to expand)</summary>

| Tool | Description |
|------|-------------|
| `mcp_memory_add` | Store notes, conventions, decisions, snippets |
| `mcp_memory_query` | Query by type with optional tag filter |
| `mcp_memory_search_semantic` | Vector similarity search |
| `mcp_memory_promote` / `demote` | Adjust retention duration |
| `mcp_get_context` | Full context (buffer, project, git, memory) |

</details>

<details>
<summary><b>Workflows & Automation</b></summary>

| Tool | Description |
|------|-------------|
| `mcp_run_workflow` | Execute named workflow with args |
| `mcp_list_workflows` | List available workflows |
| `mcp_notify` | Show notification in Emacs |

**Built-in workflows:** `wrap`, `catchup`, `quick-note`, `commit`

</details>

<details>
<summary><b>Swarm (Multi-Agent)</b></summary>

| Tool | Description |
|------|-------------|
| `swarm_spawn` | Create Claude instance with preset |
| `swarm_dispatch` | Send task to agent |
| `swarm_collect` | Get task result |
| `swarm_broadcast` | Send to all agents |
| `swarm_status` | View all agents and tasks |

**Presets:** tdd, code-review, docs, clarity, security

</details>

<details>
<summary><b>Git (Magit)</b></summary>

| Tool | Description |
|------|-------------|
| `magit_status` | Full repo status |
| `magit_stage` / `commit` / `push` | Standard git ops |
| `magit_branches` | Branch management |
| `magit_log` / `diff` | History and changes |

</details>

<details>
<summary><b>Emacs Core</b></summary>

| Tool | Description |
|------|-------------|
| `eval_elisp` | Execute arbitrary elisp |
| `list_buffers` / `get_buffer_content` | Buffer ops |
| `find_file` / `save_buffer` | File ops |
| `projectile_*` | Project navigation |
| `cider_*` | Clojure REPL |

</details>

<details>
<summary><b>Org-Mode</b></summary>

| Tool | Description |
|------|-------------|
| `org_clj_parse` | Parse org to JSON |
| `org_kanban_render` | Visual kanban board |
| `mcp_kanban_*` | Task management |

</details>

---

## Why Emacs?

Every "programmable editor" eventually reinvents:
- A config language (that becomes Turing complete)
- A plugin system (with lifecycle hooks)
- An eval mechanism (for "dynamic" features)
- A REPL (for debugging)

...and at that point you've just built a worse Lisp.

**McCarthy figured this out in 1958.** Emacs is the editor that didn't fight it.

| VSCode | Emacs |
|--------|-------|
| Extension API → LSP → JSON-RPC → Sandbox | `(eval elisp)` → done |
| Security boundary, creativity constraint | Everything is data |

LLMs need exactly what Lisp provides: homoiconicity, runtime metaprogramming, data-as-code. The "weird" features are suddenly the killer features.

---

## Documentation

| Document | Description |
|----------|-------------|
| [Tool Reference](docs/TOOLS.md) | Complete tool documentation |
| [Addon Guide](docs/addon-development.md) | Create custom addons |
| [Architecture](docs/PROJECT_SUMMARY.md) | Technical deep-dive |
| [Contributing](docs/CONTRIBUTING.md) | How to contribute |

---

## License

MIT

---

<p align="center">
  <i>"Lisp was dismissed as 'academic' for decades. Now LLMs need exactly what Lisp provides."</i>
</p>
