# CLAUDE.md - hive-mcp Project Guide

## Project Overview

**hive-mcp** is an Emacs integration for Claude MCP (Model Context Protocol) providing:
- Persistent, project-scoped memory (notes, conventions, decisions, snippets)
- Semantic search via Chroma/Ollama embeddings
- Multi-agent swarm orchestration for parallel task execution
- Full Emacs integration (buffers, git/Magit, projects, CIDER, org-mode)

## Architecture

### Module Organization (SOLID/CLARITY/DDD)

```
elisp/
├── hive-mcp.el                    # Core module, entry point
├── hive-mcp-api.el               # MCP tool handlers (API layer)
├── hive-mcp-memory.el            # Memory persistence (domain layer)
├── hive-mcp-context.el           # Context gathering
├── hive-mcp-channel.el           # Bidirectional WebSocket channel
├── hive-mcp-graceful.el          # Error handling utilities
└── addons/
    ├── hive-mcp-swarm.el         # Swarm orchestration (facade)
    └── swarm/                     # Decomposed swarm modules
        ├── hive-mcp-swarm-terminal.el  # Terminal backend abstraction
        ├── hive-mcp-swarm-presets.el   # Preset loading/management
        ├── hive-mcp-swarm-prompts.el   # Prompt detection/response
        └── hive-mcp-swarm-events.el    # Channel event emission
```

### Design Principles Applied

**SOLID:**
- **S**ingle Responsibility: Each swarm module handles one concern
- **O**pen/Closed: New backends via dispatch, not modification
- **L**iskov Substitution: All preset sources return same format
- **I**nterface Segregation: Separate APIs for auto/human prompt modes
- **D**ependency Inversion: Callers depend on abstractions, not implementations

**CLARITY Framework:**
- **C**omposition over modification: Strategy pattern for terminal backends
- **L**ayers stay pure: Domain logic separated from I/O
- **A**rchitectural performance: Non-blocking async operations
- **R**epresented intent: Clear module naming by responsibility
- **I**nputs are guarded: Validation at module boundaries
- **T**elemetry first: Event emission for monitoring
- **Y**ield safe failure: Graceful fallbacks via `hive-mcp-with-fallback`

## Swarm Module Decomposition

The swarm system was refactored from a monolithic 1400-line file into focused modules:

| Module | Lines | Responsibility | SOLID Principle |
|--------|-------|----------------|-----------------|
| `hive-mcp-swarm-terminal.el` | 250 | Terminal I/O abstraction | SRP, DIP |
| `hive-mcp-swarm-presets.el` | 200 | Preset loading (file + memory) | OCP, LSP |
| `hive-mcp-swarm-prompts.el` | 380 | Prompt detection & response | SRP, ISP |
| `hive-mcp-swarm-events.el` | 130 | Channel event emission | SRP, DIP |
| `hive-mcp-swarm.el` | 1450 | Facade + orchestration | Facade pattern |

### Terminal Backend Abstraction

```elisp
;; Strategy pattern - callers don't know about vterm/eat/claude-code-ide
(hive-mcp-swarm-terminal-send buffer text)  ; Auto-detects backend
(hive-mcp-swarm-terminal-wait-ready buffer callback)  ; Non-blocking!
```

Backends supported:
- `claude-code-ide` (recommended) - MCP WebSocket integration
- `vterm` - Native terminal compilation
- `eat` - Pure Emacs Lisp (experimental)

### Preset Sources (Priority Order)

1. **Memory-based**: Project-scoped conventions tagged `swarm-preset`
2. **File-based**: `.md` files from preset directories

```elisp
;; Both sources return same format (LSP compliance)
(hive-mcp-swarm-presets-get "tdd")  ; Checks memory first, then files
```

### Prompt Handling Modes

| Mode | Description | Implementation |
|------|-------------|----------------|
| `bypass` | CLI flag `--permission-mode bypassPermissions` | No Emacs involvement |
| `auto` | Timer-based auto-approve | `hive-mcp-swarm-prompts--auto-tick` |
| `human` | Forward to master for decision | Push events via channel |

## Key Commands

```bash
# Start the MCP server
./start-mcp.sh

# Run tests
clojure -M:dev:test

# REPL development
clojure -M:dev:nrepl
```

## Common Workflows

### Adding a New Terminal Backend

1. Add detection in `hive-mcp-swarm-terminal-backend-available-p`
2. Add send function `hive-mcp-swarm-terminal--send-<backend>`
3. Update `hive-mcp-swarm-terminal-send` dispatch
4. Add buffer creation in `hive-mcp-swarm-terminal-create-buffer`

### Adding a New Preset Source

1. Create query function returning `(list :content "..." :tags '(...))`
2. Add to `hive-mcp-swarm-presets-get` priority chain
3. Update `hive-mcp-swarm-presets-list` to include source

### Adding a New Event Type

1. Add typed emitter in `hive-mcp-swarm-events.el`:
   ```elisp
   (defun hive-mcp-swarm-events-emit-<event-type> (...)
     (hive-mcp-swarm-events-emit "<event-type>" `((...))))
   ```
2. Call from swarm.el where event occurs

## Testing

```bash
# Unit tests
clojure -M:dev:test

# Emacs integration (manual)
M-x hive-mcp-swarm-mode
M-x hive-mcp-swarm-spawn "test" :presets '("tdd")
M-x hive-mcp-swarm-status
```

## Dependencies

- Emacs 28.1+
- Clojure CLI 1.11+
- Java 17+
- Optional: Ollama (semantic search), vterm/eat (terminal backends)

## File Conventions

- Module files: `hive-mcp-<module>.el`
- Addon files: `elisp/addons/hive-mcp-<addon>.el`
- Submodule files: `elisp/addons/<addon>/hive-mcp-<addon>-<submodule>.el`
- Test files: `test/hive_mcp/<module>_test.clj`
