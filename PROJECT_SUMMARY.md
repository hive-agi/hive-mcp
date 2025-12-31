# PROJECT_SUMMARY.md - emacs-mcp

> **For LLM Assistants**: This summary provides essential context for understanding and working with this codebase. Scan through section headers to find relevant areas for your task.

## Overview

**emacs-mcp** is a Model Context Protocol (MCP) server that enables Claude to interact with a running Emacs instance. It provides bidirectional integration between Claude and Emacs through:

1. **Clojure MCP Server** - Communicates with Claude via STDIO, executes commands in Emacs via `emacsclient`
2. **Emacs Lisp Package** - Provides memory, context gathering, workflows, and UI for users
3. **org-clj Library** - Native Clojure parser for org-mode files (no elisp dependency)

## Project Structure

```
emacs-mcp/
├── src/emacs_mcp/           # Clojure MCP server
│   ├── server.clj           # MCP server entry point
│   ├── tools.clj            # MCP tool handlers (50+ tools)
│   ├── emacsclient.clj      # Shell wrapper for emacsclient
│   ├── telemetry.clj        # Logging and metrics
│   ├── validation.clj       # Input validation with malli
│   ├── resilience.clj       # Circuit breaker, retries
│   ├── synergy.clj          # Dev-tools + emacs bridge helpers
│   ├── prompt_capture.clj   # Prompt Engineering Knowledge Base
│   └── org_clj/             # Native org-mode parser library
│       ├── parser.clj       # Parse org → EDN (22KB, 46 assertions)
│       ├── writer.clj       # Serialize EDN → org
│       ├── query.clj        # Find/filter headlines
│       ├── transform.clj    # Immutable document updates
│       └── render.clj       # Kanban board rendering (OCP)
│
├── elisp/                   # Emacs Lisp package
│   ├── emacs-mcp.el         # Main entry, minor mode
│   ├── emacs-mcp-memory.el  # Persistent JSON storage per-project
│   ├── emacs-mcp-context.el # Buffer/project/git context gathering
│   ├── emacs-mcp-api.el     # Stable API for Claude calls
│   ├── emacs-mcp-workflows.el  # Multi-step automations
│   ├── emacs-mcp-triggers.el   # Keybindings and hooks
│   ├── emacs-mcp-addons.el     # Lazy-load addon system
│   ├── emacs-mcp-transient.el  # Transient UI menus
│   └── addons/              # Modular integrations
│       ├── emacs-mcp-cider.el      # CIDER nREPL integration
│       ├── emacs-mcp-org-kanban.el # Interactive kanban board
│       ├── emacs-mcp-swarm.el      # Multi-agent orchestration
│       └── ...
│
├── test/                    # Clojure tests
├── deps.edn                 # Clojure dependencies
├── bb.edn                   # Babashka tasks
└── kanban.org               # Project task tracking
```

## Key Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| `org.clojure/clojure` | 1.12.1 | Core language |
| `io.modelcontextprotocol/mcp-clojure-sdk` | git SHA | MCP protocol implementation |
| `metosin/malli` | 0.16.4 | Schema validation |
| `org.clojure/core.async` | 1.7.701 | Async operations |
| `org.clojure/data.json` | 2.5.1 | JSON serialization |
| `nrepl/bencode` | 1.2.0 | nREPL communication |
| `taoensso/timbre` | 6.8.0 | Logging |

Emacs requirements: Emacs 28.1+, transient 0.4.0+

## MCP Tools Reference

### Core Emacs Tools
```clojure
;; Execute elisp
(handle-eval-elisp {:code "(buffer-name)"})

;; Buffer operations
(handle-list-buffers _)
(handle-get-buffer-content {:buffer_name "*scratch*"})
(handle-switch-to-buffer {:buffer_name "file.clj"})
(handle-find-file {:file_path "/path/to/file"})

;; Navigation
(handle-goto-line {:line 42})
(handle-insert-text {:text "Hello"})
```

### Memory & Context Tools
```clojure
;; Get full context (buffer, project, git, memory)
(handle-mcp-get-context _)

;; Memory operations (per-project persistent storage)
(handle-mcp-memory-add {:type "note" :content "Remember this" :tags ["important"]})
(handle-mcp-memory-query {:type "convention" :limit 10})

;; Workflows
(handle-mcp-list-workflows _)
(handle-mcp-run-workflow {:name "test-and-commit"})
```

### CIDER/Clojure Tools
```clojure
;; Check CIDER connection
(handle-cider-status _)

;; Evaluate Clojure (via CIDER)
(handle-cider-eval-silent {:code "(+ 1 2)"})      ; No REPL output
(handle-cider-eval-explicit {:code "(println x)"}) ; Shows in REPL
```

### Kanban Tools
```clojure
;; Kanban task management
(handle-mcp-kanban-status _)
(handle-mcp-kanban-create-task {:title "New task"})
(handle-mcp-kanban-move-task {:task_id "uuid" :new_status "inprogress"})

;; Native org-clj tools (no elisp required)
(handle-org-kanban-native-status {:file_path "kanban.org"})
(handle-org-kanban-native-move {:file_path "kanban.org" :task_id "id" :new_status "DONE"})
```

### Swarm Orchestration Tools
```clojure
;; Multi-agent coordination (requires vterm)
(handle-swarm-spawn {:name "tester" :presets ["tdd"]})
(handle-swarm-dispatch {:slave_id "tester-1" :prompt "Run tests"})
(handle-swarm-broadcast {:prompt "Stop work"})
(handle-swarm-collect {:task_id "task-uuid"})
```

### org-clj Library Tools
```clojure
;; Parse org file to JSON
(handle-org-clj-parse {:file_path "file.org"})

;; Query org document
(handle-org-clj-query {:file_path "file.org" :query_type "by_status" :query_value "TODO"})

;; Render kanban board
(handle-org-kanban-render {:file_path "kanban.org" :format "terminal"})
```

### Prompt Capture Tools (RAG Knowledge Base)
```clojure
;; Capture a well-structured prompt with analysis
(handle-prompt-capture {:prompt "Create a Clojure namespace..."
                        :accomplishes "Builds a RAG knowledge base..."
                        :well_structured "Specifies implementation language..."
                        :category "coding"          ; Optional - auto-inferred
                        :quality "success"          ; success, partial, failure, untested
                        :tags ["clojure" "mcp"]})   ; Optional - auto-tagged

;; List/filter captured prompts
(handle-prompt-list {:category "coding" :quality "success" :limit 20})

;; Search prompts by keyword
(handle-prompt-search {:query "RAG" :limit 10})

;; Analyze prompt without saving
(handle-prompt-analyze {:prompt "Some prompt text..."})
;; => {:quality-score 75 :suggestions [...] :inferred-category :coding}

;; Get statistics dashboard
(handle-prompt-stats {})
;; => {:total 42 :by-category {:coding 15 :debug 8 ...} :by-quality {:success 30 ...}}
```

**Categories**: coding, debug, planning, meta, research, config, workflow, architecture
**Quality Ratings**: success, partial, failure, untested
**Storage**: `~/.emacs.d/emacs-mcp/prompts.org` (org-mode format)

## Prompt Capture Library Architecture

Clojure-native prompt engineering knowledge base for RAG:

```clojure
(require '[emacs-mcp.prompt-capture :as pc])

;; Capture a prompt with full analysis
(pc/capture-prompt 
  {:prompt "Create a REST API endpoint..."
   :accomplishes "Implements user authentication"
   :well-structured "Specifies HTTP method, route, and response format"
   :quality :success
   :tags ["api" "auth"]})
;; => {:success true :entry {...} :confirmation "╔═══..."}

;; List prompts with filtering
(pc/list-prompts {:category :coding :quality :success})

;; Search by keyword
(pc/search-prompts "authentication")

;; Get statistics
(pc/get-statistics)
;; => {:total 42 :by-category {:coding 15} :by-quality {:success 30}}

;; Quality assessment (without saving)
(pc/assess-prompt-quality "Some prompt text")
;; => {:score 75 :has-context? true :has-constraints? false ...}
```

### Prompt Entry Schema (Malli)
```clojure
{:id "20251231-uuid"
 :prompt "The actual prompt text"
 :accomplishes "What this prompt achieves"
 :well-structured "Why it's well-structured"
 :improvements "Suggested improvements"  ; Auto-generated
 :category :coding                        ; 8 categories
 :tags ["tag1" "tag2"]                    ; Auto-merged with category tags
 :quality :success                        ; success, partial, failure, untested
 :created "2025-12-31T15:43:21"
 :source "user"                           ; user, observed, generated
 :model "claude-opus-4-5"                 ; Optional
 :context "Additional context"}           ; Optional
```

## org-clj Library Architecture

Native Clojure org-mode parser with immutable data structures:

```clojure
;; Parse
(require '[emacs-mcp.org-clj.parser :as parser])
(def doc (parser/parse-document (slurp "file.org")))

;; Query
(require '[emacs-mcp.org-clj.query :as query])
(query/find-by-status doc "TODO")
(query/find-by-property doc :ID "abc123")
(query/task-stats doc) ; => {:total 10 :todo 3 :in-progress 2 :done 5}

;; Transform (immutable - returns new document)
(require '[emacs-mcp.org-clj.transform :as transform])
(transform/set-status doc "id123" "DONE")
(transform/add-tag doc "id123" "urgent")

;; Write back
(require '[emacs-mcp.org-clj.writer :as writer])
(writer/write-document-to-file updated-doc "file.org")

;; Render kanban board
(require '[emacs-mcp.org-clj.render :as render])
(println (render/render-to-terminal "kanban.org"))
```

### Document Schema (Malli)
```clojure
{:properties {:TITLE "..." :STARTUP "..."}
 :headlines [{:level 1
              :keyword "TODO"        ; nil, TODO, DONE, IN-PROGRESS, etc.
              :priority "A"          ; nil, A, B, C
              :title "Task title"
              :tags ["tag1" "tag2"]
              :properties {:ID "..." :CREATED "..."}
              :planning {:CLOSED "..." :SCHEDULED "..." :DEADLINE "..."}
              :content "Body text..."
              :children [...]}]}     ; Nested headlines
```

## Emacs API Reference

### From Claude (via MCP tools)
```elisp
;; These are called via the Clojure server's tools
(emacs-mcp-api-get-context)           ; Full context
(emacs-mcp-api-memory-add "note" "content" '("tag"))
(emacs-mcp-api-memory-query "convention")
(emacs-mcp-api-run-workflow "name")
```

### User Keybindings (C-c m prefix)
| Key | Command |
|-----|---------|
| `C-c m m` | Open transient menu |
| `C-c m n` | Add note to memory |
| `C-c m s` | Save region as snippet |
| `C-c m c` | Add convention |
| `C-c m d` | Record decision |
| `C-c m l` | Browse memory |
| `C-c m w` | Run workflow |
| `C-c m k` | Open kanban board |

### Kanban Board Keybindings
| Key | Command |
|-----|---------|
| `n/j` | Next task |
| `p/k` | Previous task |
| `h/l` | Previous/next column |
| `RET` | Open task in org file |
| `m` | Move task (prompt) |
| `>/<` | Move right/left |
| `c` | Create task |
| `e` | Edit task title |
| `d` | Delete task |
| `g` | Refresh board |

## Addon System

Addons are lazy-loaded when their trigger packages are detected:

```elisp
;; Auto-configured mappings
'((cider . cider)           ; Load when CIDER loads
  (org-kanban . org-kanban)
  (swarm . vterm))          ; Load when vterm loads

;; Always-load addons
(setq emacs-mcp-addon-always-load '(cider org-kanban))
```

### Addon Lifecycle Hooks
```elisp
(emacs-mcp-addon-register
 'my-addon
 :version "1.0.0"
 :description "My addon"
 :init #'my-sync-setup           ; Synchronous setup
 :async-init #'my-start-server   ; Non-blocking (returns process)
 :shutdown #'my-cleanup)         ; On unload
```

## Development Workflow

### Start Development
```bash
# Start nREPL (port 7910)
bb repl
# Or: clojure -M:nrepl

# Run tests
bb test
# Or: clojure -M:test

# Start MCP server directly
bb mcp
# Or: clojure -X:mcp
```

### Connect from Emacs
```elisp
(cider-connect-clj '(:host "localhost" :port 7910))
```

### Test MCP Tools
```clojure
;; In REPL
(require '[emacs-mcp.tools :as tools])
(tools/handle-emacs-status {})
(tools/handle-mcp-get-context {})
```

## Implementation Patterns

### OCP (Open-Closed Principle)
The render module uses protocols for extensibility:
```clojure
(defprotocol KanbanRenderer
  (render-board [this board-data])
  (render-column [this column-name tasks])
  (render-card [this task]))

;; Add new renderer without modifying existing code
(defrecord HTMLRenderer [...]
  KanbanRenderer
  ...)
```

### Immutable Transforms
All org-clj transforms return new documents:
```clojure
(-> doc
    (transform/set-status id "DONE")
    (transform/set-closed id timestamp)
    (transform/add-tag id "completed"))
```

### Backend Abstraction (Elisp)
The kanban addon uses `cl-defgeneric` for multiple backends:
```elisp
(cl-defgeneric emacs-mcp-kanban--create-task (backend project-id title &optional description))
(cl-defmethod emacs-mcp-kanban--create-task ((_backend (eql standalone)) ...)
(cl-defmethod emacs-mcp-kanban--create-task ((_backend (eql vibe)) ...)
```

### Auto-sync Pattern
Board operations go through unified API for consistency:
```elisp
(defun emacs-mcp-kanban-board-move-task ()
  ;; Use addon API - handles auto-sync and agent tracking
  (emacs-mcp-kanban-move-task id vibe-status))
```

## Extension Points

1. **New MCP Tools**: Add handler in `tools.clj`, register in `tools` vector
2. **New org-clj Renderers**: Implement `KanbanRenderer` protocol
3. **New Elisp Addons**: Copy `addon-template.el`, register with `emacs-mcp-addon-register`
4. **New Kanban Backends**: Implement `cl-defmethod` for each generic function
5. **New Workflows**: Register via `(emacs-mcp-workflow-register name spec)`

## Quick Troubleshooting

| Issue | Check |
|-------|-------|
| Tools not working | `(handle-emacs-status _)` - Emacs server running? |
| Memory tools fail | `(handle-mcp-capabilities _)` - emacs-mcp.el loaded? |
| CIDER tools fail | `(handle-cider-status _)` - CIDER connected? |
| Kanban board empty | Verify `emacs-mcp-kanban-org-file` path |
| Evil-mode keybindings | Board uses emacs state by default |

## Recent Session Context

**Current branch**: `main`

**Recent work** (Dec 2025):
- **Prompt Capture System** - RAG knowledge base for prompt engineering
  - Malli schema validation (Category, QualityRating, PromptEntry)
  - 8-category taxonomy with auto-inference
  - Quality assessment scoring (0-100)
  - Auto-generated improvement suggestions
  - Org-mode storage via org-clj
  - 5 MCP tools: capture, list, search, analyze, stats
- org-clj native parser library (Phases 1-4 complete)
- Interactive kanban board with evil-mode integration
- Board functions integrated with addon unified API
- Melpazoid integration addon with fast-mode

**Released**: v0.3.0 (async nREPL startup, addon lifecycle system)
**Merged PRs**: #7 (async nREPL), #8 (addon lifecycle), #9 (docs reorganization), #13 (melpazoid)
