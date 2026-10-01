# hive-mcp

<!-- hive-badges -->

[![Clojars Project](https://img.shields.io/clojars/v/io.github.hive-agi/hive-mcp.svg)](https://clojars.org/io.github.hive-agi/hive-mcp)
[![cljdoc](https://cljdoc.org/badge/io.github.hive-agi/hive-mcp)](https://cljdoc.org/d/io.github.hive-agi/hive-mcp/CURRENT)
[![CI](https://github.com/hive-agi/hive-mcp/actions/workflows/ci.yml/badge.svg)](https://github.com/hive-agi/hive-mcp/actions/workflows/ci.yml)
[![License: AGPL-3.0-or-later](https://img.shields.io/badge/License-AGPL--3.0--or--later-blue.svg)](https://www.gnu.org/licenses/agpl-3.0.txt)

<!-- /hive-badges -->

**Your AI finally remembers.**

[![MCP](https://img.shields.io/badge/MCP-Compatible-green.svg)](https://modelcontextprotocol.io)

---

## The Problem

You explain your codebase to Claude. Architecture, constraints, patterns. Then you hit the context limit. New session. **Claude forgets everything.**

## The Solution

```
Session 1                         Session 2
───────────────────────────────────────────────────
You: "Our auth uses JWT..."       You: catch me up
Claude: *learns*                  Claude: "I remember:
         ↓                         - Auth uses JWT with refresh
    *wrap up*                      - Convention: validate at boundaries
         ↓                         What should we work on?"
    [Memory]  ────────────────►
```

Persistent, project-scoped memory with semantic search. Conventions, decisions, snippets — stored locally, never forgotten.

### The two rituals

**Catch up** — reconstruct everything at the start of a session:

> hive `project workflow catchup` using pwd as dir

**Wrap up** — crystallize what you learned before you lose it:

> make memories on all learnings this session, kg connect them, sync kanban, create remaining kanban tasks if any, and `workflow wrap`. Can use `multi` command to do all at once.

Both are plain requests to the model, not slash commands — it reaches for the `project`, `memory`, `kg`, and `kanban` tools itself. `multi` batches the whole wrap into a single call.

---

## What Sets hive-mcp Apart

| Capability | hive-mcp | Typical MCP servers |
|---|---|---|
| **Knowledge Graph** | Structural edges - how knowledge relate? | Flat key-value or vector-only |
| **Session Continuity** | `/wrap` crystallizes, `/catchup` reconstructs — zero-down re-explaining across sessions | Manual copy-paste or lost |
| **Multi-Agent Coordination** | Agentic lings with file claims, hivemind shouts, and a continuous production belt | Single-agent only |
| **Scoped Memory** | Hierarchical Context Retrieval (HCR) - project scoping - with TTL decay | Global namespace or none |
| **Extension Architecture** | `requiring-resolve` stubs with noop fallbacks - plug your extensions and play | Monolithic |

---

## Quick Start

### 1. Install

Install with `curl -fsSL https://hive-mcp.com/install.sh | sh` then `hive setup`.
`hive setup` installs [bb-mcp](https://github.com/hive-agi/bb-mcp), publishes it at
`~/.local/share/hive-mcp/bb-mcp`, and registers `hive` with your MCP clients (Claude
Code, Codex) through bb-mcp. Already have bb-mcp checked out somewhere? Run
`<that checkout>/bb-mcp setup`: wherever bb-mcp lives becomes the reference. Verify with
`bb-mcp setup --check`.

A running hive is two processes. The **backend** is the hive-mcp JVM serving nREPL on
port 7910 (`bin/hive-mcp-foss`). The **client entry point** is bb-mcp, the lightweight
Babashka MCP server your client launches. bb-mcp is the only thing a client config
names, and it names it through one well-known anchor, `~/.local/share/hive-mcp/bb-mcp`
(a symlink to wherever bb-mcp is checked out), so the same config works on every
machine and for every user.

**Option A: One line (recommended)**

```bash
curl -fsSL https://hive-mcp.com/install.sh | sh
hive setup
```

`install.sh` installs the `hive` CLI and the setup skills into `~/.claude/skills`.
`hive setup` checks the prerequisites (babashka is required), reuses the bb-mcp the
anchor already points at or clones it to `${BB_MCP_DIR:-$HOME/bb-mcp}`, and runs
`<bb-mcp>/bb-mcp setup --client all`, which publishes the anchor and registers `hive`
with Claude Code and Codex. Client registration lives only in `bb-mcp setup`; the CLI
does not register anything itself. Start the backend with `hive start`, or with
`bin/hive-mcp-foss` from a hive-mcp checkout. `hive doctor` runs
`bb-mcp setup --check` and offers `bb-mcp setup` as the fix.

To let the assistant drive the rest (starter pack, store gateway, credentials, what to
check when a step fails), start Claude Code and say what you want:

> help me set up the hive-mcp harness locally, I have a key

> help me set up a FOSS build of the hive-mcp harness

**Option B: Batteries included, fully FOSS, by hand**

Clone both repositories. `bin/hive-mcp-foss` starts the open-source services (Chroma
for memory, the clojure-lsp sidecar), waits until each is actually reachable, merges
the starter pack and boots the backend on nREPL 7910. `bb-mcp setup` then registers
the client side:

```bash
git clone https://github.com/hive-agi/hive-mcp.git
git clone https://github.com/hive-agi/bb-mcp.git

(cd hive-mcp && bin/hive-mcp-foss)        # backend: hive-mcp JVM on nREPL 7910

bb-mcp/bb-mcp setup --dry-run              # print the plan without writing
bb-mcp/bb-mcp setup                        # publish the anchor, register hive with your clients
```

`bb-mcp setup` takes `--client claude|codex|all`, `--scope user|project` and `--port N`
(only when the backend is not on 7910). Wherever you cloned bb-mcp becomes the
reference; move it and rerun `bb-mcp setup` to repoint the anchor.

To register by hand instead, a client that expands environment variables takes this
entry (it is the committed [`.mcp.json`](.mcp.json)):

```json
{
  "mcpServers": {
    "hive": {
      "command": "${HOME}/.local/share/hive-mcp/bb-mcp/start-bb-mcp.sh",
      "env": { "BB_MCP_NREPL_PORT": "7910" }
    }
  }
}
```

No arguments are needed: `start-bb-mcp.sh` takes the project directory from the
client's working directory and the nREPL port from `<project>/.nrepl-port`, else 7910.
For a client without variable expansion, write the expanded anchor path
(`/home/<you>/.local/share/hive-mcp/bb-mcp/start-bb-mcp.sh`), never the path of the real
checkout.

The **starter pack** is [`starter.deps.edn`](starter.deps.edn): the FOSS addons that
turn the bare core into a working harness, merged over `deps.edn` at boot
(`HIVE_STARTER=0` boots the bare core instead).

| Role | Addon | Needs on the host | Status |
|---|---|---|---|
| Vessel, where lings run | hive-emacs | an Emacs daemon | shipped |
| Knowledge graph store | hive-datahike | nothing, embedded | shipped |
| Code intelligence | lsp-mcp, clj-kondo-mcp, scc-mcp, basic-tools-mcp | clojure-lsp, clj-kondo, scc | fixed, pending release |
| Vessel, headless | hive-tmux | tmux, Python 3 with libtmux | pending: host-defined IVessel |
| Harness bridge | hive-claude | Claude Code | pending: host-defined headless protocols |

Every shipped row was measured to mount or load in a cold boot; the pending rows sit
commented in the file with their coordinates. A row moves up when its release both
ships a `META-INF/hive-addons` manifest and constructs an `IAddon` with no hive-mcp
on the classpath, which is what `dev/foss_compliance.clj` and
`dev/addon_boot_probe.clj` check. A missing host tool degrades that one addon with a
logged reason; the boot still completes. No private registry, no VPN, no credential
store. The knobs
(`HIVE_TELEMETRY=1`, `HIVE_NATS=1`, remote Chroma/Ollama hosts) are in the
[FOSS Quickstart](https://github.com/hive-agi/hive-mcp/wiki/FOSS-Quickstart).

**Option C: Container**

```bash
docker build -t hive-mcp .                 # bakes the starter pack into the image
docker run -p 7910:7910 -e HIVE_PROFILE=k8s-headless hive-mcp
```

The image runs the backend from source with the same starter pack. Build with
`--build-arg DEPS_OVERLAY=` for the bare core; `HIVE_HEAP` sets the JVM cap (default 2g).
Clients still connect through bb-mcp: run `bb-mcp setup` on the client machine (add
`--port N` when nREPL is published on another port).

**Option D: From the parts**

```bash
docker compose up -d chroma lsp-sidecar                    # services
clojure -Sdeps "$(cat starter.deps.edn)" -M:dev:nrepl      # backend + starter pack
```

Drop the `-Sdeps` argument for the bare core, or point it at your own overlay the
way `bin/hive-mcp` merges a gitignored `local.deps.edn`. Clients connect through bb-mcp
exactly as in Option B.

### 2. Verify

```bash
bb-mcp setup --check     # anchor, client entries, babashka, nREPL reachability
claude mcp list          # lists "hive", launched from the anchor
```

`--check` exits non-zero when a check fails: the anchor does not resolve to a bb-mcp
with an executable `start-bb-mcp.sh`, a client's `hive` entry points somewhere other
than the anchor (another user's home, the real checkout path), or `bb` is missing. An
nREPL that does not answer yet is only a warning; start the backend and check again.

### 3. Optional: Semantic Search

```bash
ollama pull nomic-embed-text      # local embeddings; Chroma is already up
```

### Prerequisites

| Requirement | Version | Install |
|---|---|---|
| Claude Code | Latest | [claude.ai/download](https://claude.ai/download) |
| Babashka | recent | [babashka.org](https://babashka.org) (runs bb-mcp, the client entry point; required) |
| Java | 21 | `apt install openjdk-21-jdk` (CI and the image run 21) |
| Clojure CLI | 1.12+ | [clojure.org/guides/install_clojure](https://clojure.org/guides/install_clojure) |
| Docker | recent | Chroma and the LSP sidecar; `HIVE_SKIP_COMPOSE=1` if they run elsewhere |

**Optional**:
- tmux for the headless vessel, or Emacs 28.1+ for the Emacs vessel, swarm vterm UI and
  buffer integration. See [Emacs Configuration](https://github.com/hive-agi/hive-mcp/wiki/Emacs-Configuration).
  The Emacs vessel is the most exercised path; headless runs through hive-tmux.
- Ollama for semantic search over memory.

---

## What hive-mcp Is

hive-mcp is a **host**: a runtime that other things mount into. It is not a library you
depend on, and it is not the thing you type into.

Three words circulate for systems in this space, and they aren't synonyms:

- **Harness** — the scaffolding that drives a model: prompt loop, tool dispatch, context
  management. Claude Code is a harness. hive-mcp is a harness *for the agents it spawns* —
  lings get their loop, presets, budget and context from it — but it is not the
  harness you talk to.
- **MCP server** — the wire protocol. True, but that's transport, not architecture.
- **Host** — the runtime addons mount into and are amalgamated by. This is the load-bearing one.

In one sentence: **hive-mcp is an addon host that doubles as an agent harness.** Your MCP
client talks to it; addons supply the capabilities; it runs the sub-agents.

The rule that shapes everything else:

> hive-mcp is **CLOSED for modification, OPEN for extension via IAddon.**

Core owns protocols, registries, orchestrators, the server, memory CRUD, KG edges, swarm
coordination, the session ritual — and a **working noop default for every extension point**.
Everything else is an addon. That boundary is what makes the FOSS stack a complete system
rather than a demo, and what keeps the open core clean as the product layer grows.

---

## The Tool Surface

Tools are grouped into **domain roots**, each a namespace with subcommands (`memory add`,
`kg traverse`, `agent spawn`). Core ships these roots:

| Tool | Purpose |
|---|---|
| `memory` | Persistent entries with semantic search, TTL decay, scoping |
| `kg` | Knowledge Graph — edges, subgraphs |
| `agent` | Spawn/kill/dispatch lings |
| `hivemind` | Shout/ask coordination between agents |
| `session` | Wrap, catchup, whoami, context store |
| `workflow` | Forge belt, FSM-driven production cycles |
| `kanban` | Task management with plan-to-kanban |
| `magit` | Git operations — status, stage, commit, push |
| `cider` | Clojure nREPL eval, doc, completions |
| `preset` | Agent presets — list, search, generate headers |
| `analysis` | Kondo lint, SCC metrics, complexity hotspots |
| `lsp` | Code analysis — callers, calls, namespace graph |
| `project` | Projectile — files, search, hierarchy scan |
| `emacs` | Eval elisp, buffers, notifications |
| `olympus` | Grid layout control for multi-agent UI |
| `agora` | Multi-agent dialogue and consensus |
| `config` | Runtime configuration management |
| `migration` | KG/memory backup, restore, backend switching |
| `multi` | Meta-facade — batches any of the above into one call |

Several of these (`cider`, `lsp`, `analysis`, `olympus`, `agora`) arrive from addons rather
than core. That's the point: **anything an addon registers that doesn't collide with a core
domain name becomes a new top-level tool root automatically** — no core edit, no allowlist
entry, no release. A config-driven visibility gate (`[:tool-roots :visible]`) can shrink the
advertised surface without breaking callers; hidden tools stay dispatchable by name, they
just leave `tools/list`.

---

## Architecture

```
┌──────────────────────────────────────────────────────────┐
│      Claude Code / any MCP client   (your harness)       │
└─────────────────────────┬────────────────────────────────┘
                          │ MCP protocol
┌─────────────────────────▼────────────────────────────────┐
│  hive-mcp — THE HOST (AGPL-3.0)                          │
│                                                          │
│   Memory  ──►  Chroma vectors + scoped entries           │
│   KG      ──►  DataScript / Datalevin / Datahike         │
│   Swarm   ──►  lings + hivemind                          │
│   Session ──►  catchup / wrap rituals                    │
│                                                          │
│   protocols · registries · orchestrators · noop defaults │
└─────────────────────────┬────────────────────────────────┘
                          │ IAddon — addon → core, never the reverse
      ┌───────────────────┼───────────────────┐
      ▼                   ▼                   ▼
  :addon              :library             :addon
  (user-facing        (backend: vector     (user-facing
   tools)              store, terminal)     tools)
```

Dependencies and knowledge flow **addon → core**. A `requiring-resolve` of a concrete addon
namespace from core is the smell that says the boundary broke.

---

## Extensibility

hive-mcp uses a plugin architecture based on the **IAddon protocol** with automatic classpath discovery. Creating a new addon takes one command:

```bash
clojure -Sdeps '{:deps {io.github.hive-agi/hive-mcp {:mvn/version "1.1.2"}}}' \
  -Tnew create :template hive-agi/addon :name com.example/my-addon
```

hive-mcp publishes to Clojars, so the released coordinate is all you need — no `:git/sha`
pinning. Latest version: see the [releases page](https://github.com/hive-agi/hive-mcp/releases).

This generates a complete project with:
- **IAddon protocol implementation** (defrecord with 8 lifecycle methods)
- **META-INF classpath manifest** (auto-discovered at startup, zero core changes)
- **Unit tests** (12 tests covering lifecycle, tools, health)
- **REPL-ready** development setup

### Addon Types

| Type | Use case |
|------|----------|
| **Native** | Clojure code in the same JVM — direct function calls |
| **MCP Bridge** | Proxy to external MCP servers via stdio/sse |
| **External** | Non-MCP integrations (REST APIs, CLI tools) |

Manifests also carry `:addon/kind` — `:addon` for anything contributing user-facing tools,
`:library` for pure backends (vector store, terminal, instrumentation).

### How It Works

Addons are discovered via `META-INF/hive-addons/*.edn` manifest files on the classpath (same pattern as Java's `ServiceLoader`). Manifests declare dependencies, and addons are loaded in topological order. No changes to hive-mcp core code needed.

Behaviour reaches core code paths through **generic extension keys**. Core defines the seam
and applies whatever is registered; it never learns that a given addon exists:

```clojure
;; in core — addon-agnostic, the only legitimate kind of core change
(ext/get-extension :catchup/wrap)

;; in the addon's IAddon/hooks — registered at initialize!, removed at shutdown!
{:catchup/wrap my-addon.catchup/wrap-fn}
```

### Depending on the host without depending on the host

An addon **must not** `:require` any `hive-mcp.*` namespace — the host is a runtime, not a
dependency. What it needs is expressed as a port:

- **Contracts to implement** → `hive-addon` (for `IAddon`) and `hive-contracts`
- **Host services at runtime** → soft resolution (`requiring-resolve`) behind a var-map, so
  the addon loads and degrades gracefully when the host is absent

A load-time require on the host is the violation; a soft runtime resolve is not. One hard
require makes a published addon unloadable from a plain Maven fetch.

See [The Core Engine](https://github.com/hive-agi/hive-mcp/wiki/Core-Engine),
[Creating Addons](https://github.com/hive-agi/hive-mcp/wiki/Creating-Addons) and
[ADR-0007](https://github.com/hive-agi/hive-mcp/wiki/ADR-0007-hive-addons-architecture).

---

## Agents and Skills

Two ways to package reusable agent behaviour, both **plain markdown you can drop in, copy
between machines, or publish for others** — no code, no rebuild, no host restart.

| | **Agent definitions** | **Presets** |
|---|---|---|
| Answers | *Who is this agent?* | *How should it work?* |
| Format | Markdown + YAML frontmatter | Plain markdown |
| Carries | Identity, tool allowlist, model, hooks | Methodology, constraints, output format |
| Lives in | `.claude/agents/*.md` | `resources/presets/*.md`, custom dirs, or the memory store |
| Composes by | Priority override — highest source wins | Concatenation — stack as many as you need |

**An agent definition is a role; a preset is a skill.** One definition per agent, as many
presets as the job needs.

```markdown
---
name: reviewer
description: Reviews diffs for correctness and contract violations
tools: ["memory", "git", "fs"]
---
You review changes. Lead with the defect, not the summary.
```

Definitions resolve from four sources — `:user` (`~/.claude/agents/`) overrides `:project`
(`.claude/agents/`) overrides `:plugin` (addon-contributed) overrides `:built-in` — so you
can shadow any of them without editing them. Installing is a file copy; sharing is a
`git clone` into `~/.claude/agents/`, or an addon that contributes definitions for a whole team.

**47 presets ship built-in** across methodology (`tdd`, `solid`, `ddd`, `clarity`), roles
(`reviewer`, `debugger`, `security-auditor`, `researcher`) and coordination
(`task-coordinator`, `hivemind`). The `preset` tool handles the whole
lifecycle — `list`, `get`, `add`, `delete`, and **semantic `search`**, so you can find one
by describing the job rather than knowing its name.

See [Agents and Skills](https://github.com/hive-agi/hive-mcp/wiki/Agents-and-Skills).

---

## For LLMs

See [`CLAUDE.md`](CLAUDE.md) for project conventions, tool patterns, and memory usage guidelines.

## Documentation

**[docs.hive-mcp.com](https://docs.hive-mcp.com)** renders the current documentation as
a site, generated from the same markdown as the
**[Wiki](https://github.com/hive-agi/hive-mcp/wiki)**, which is where it is edited.
Agents want [docs.hive-mcp.com/llms.txt](https://docs.hive-mcp.com/llms.txt), which
indexes every page next to its canonical markdown.

Start with these four:

| Guide | Description |
|---|---|
| **[FOSS Quickstart](https://github.com/hive-agi/hive-mcp/wiki/FOSS-Quickstart)** | Batteries-included open-source stack, one command |
| **[The Core Engine](https://github.com/hive-agi/hive-mcp/wiki/Core-Engine)** | What hive-mcp is: host, harness, and the OCP boundary |
| **[Agents and Skills](https://github.com/hive-agi/hive-mcp/wiki/Agents-and-Skills)** | Drop-in agent definitions and presets |
| **[Creating Addons](https://github.com/hive-agi/hive-mcp/wiki/Creating-Addons)** | Scaffold and publish your own addon |

Everything else:

| Guide | Description |
|---|---|
| [Installation](https://github.com/hive-agi/hive-mcp/wiki/Installation) | Detailed setup |
| [Infrastructure Setup](https://github.com/hive-agi/hive-mcp/wiki/Infrastructure-Setup) | Docker, Ollama, Chroma |
| [Ecosystem](https://github.com/hive-agi/hive-mcp/wiki/Ecosystem) | Architecture and open-source strategy |
| [Interfaces and Protocols](https://github.com/hive-agi/hive-mcp/wiki/Interfaces-and-Protocols) | All ~49 protocols with signatures |
| [ADR-0007](https://github.com/hive-agi/hive-mcp/wiki/ADR-0007-hive-addons-architecture) | Why the addon architecture looks like this |
| [Addon Classpath Discovery](https://github.com/hive-agi/hive-mcp/wiki/Addon-Classpath-Discovery) | How manifests are found and loaded |
| [Tools Reference](https://github.com/hive-agi/hive-mcp/wiki/Tools-Reference) | Tool surface and DSL verbs |
| [Presets](https://github.com/hive-agi/hive-mcp/wiki/Presets) | System prompts for ling specialization |
| [Session Continuity](https://github.com/hive-agi/hive-mcp/wiki/Session-Continuity) | catchup and wrap |
| [Emacs Configuration](https://github.com/hive-agi/hive-mcp/wiki/Emacs-Configuration) | Optional Emacs surface |
| [Troubleshooting](https://github.com/hive-agi/hive-mcp/wiki/Troubleshooting) | Common issues |

---

## Related

| Repository | Description |
|---|---|
| **[bb-mcp](https://github.com/hive-agi/bb-mcp)** | The standard client entry point: the lightweight Babashka MCP server (~50MB RAM) every client launches, published at `~/.local/share/hive-mcp/bb-mcp`; `bb-mcp setup` registers it |
| **[lsp-mcp](https://github.com/hive-agi/lsp-mcp)** | Clojure-LSP bridge addon (analysis, callers, references) |
| **[basic-tools-mcp](https://github.com/hive-agi/basic-tools-mcp)** | File read/write/glob/grep tools addon |
| **[hive-dsl](https://github.com/hive-agi/hive-dsl)** | DSL verb compiler for batch operations |
| **[hive-test](https://github.com/hive-agi/hive-test)** | Test utilities for hive-mcp addons |
| **[olympus-web-ui](https://github.com/hive-agi/olympus-web-ui)** | Web dashboard for swarm monitoring |
| **[hive-mcp-cli](https://github.com/hive-agi/hive-mcp-cli)** | Go CLI: `hive setup` installs bb-mcp and runs `bb-mcp setup`; `hive doctor` checks it |

---

[AGPL-3.0-or-later](LICENSE)
