# Claude Code Slash Commands Setup

This guide explains how to set up Claude Code slash commands (`/wrap`, `/catchup`) that integrate with hive-mcp's memory workflows.

## Overview

hive-mcp provides two essential workflows for session continuity:

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/catchup` | Restore context from memory | Start of session |
| `/wrap` | Preserve context to memory | End of session |

These commands invoke hive-mcp workflows that handle memory storage, kanban sync, and git status automatically.

---

## Setup Instructions

### Step 1: Create Commands Directory

Claude Code looks for custom commands in `~/.claude/commands/`:

```bash
mkdir -p ~/.claude/commands
```

### Step 2: Create `/catchup` Command

Create `~/.claude/commands/catchup.md`:

```markdown
# Catch Up

Use the **hive-mcp workflow** to restore context from memory at session start.

## Instructions

Run the hive-mcp `catchup` workflow:

\`\`\`
mcp__emacs__mcp_run_workflow(name: "catchup")
\`\`\`

## What the Workflow Does

1. **Queries Memory** - Retrieves recent session summaries, decisions, conventions
2. **Checks Kanban** - Lists current tasks by status (todo, doing, review)
3. **Checks Git** - Reports branch, uncommitted changes, recent commits
4. **Finds Expiring** - Lists memories expiring soon that may need promotion
5. **Presents Summary** - Formatted catchup report

## After Catchup

The workflow returns:
- Recent session summaries
- Active decisions and conventions
- Kanban task counts
- Git status
- Recommended starting point

Ask the user: "What would you like to focus on this session?"

## Related Commands

- `/wrap` - End-of-session documentation (stores context for catchup)
- `/ship` - Merge feature branches to staging
- `/ship-pr` - Create PRs for feature branches
```

### Step 3: Create `/wrap` Command

Create `~/.claude/commands/wrap.md`:

```markdown
# End-of-Session Wrap-up

Use the **hive-mcp workflow** to preserve session context in memory.

## Instructions

Run the hive-mcp `wrap` workflow:

\`\`\`
mcp__emacs__mcp_run_workflow(
  name: "wrap",
  args: {
    "accomplishments": ["List of completed tasks"],
    "decisions": ["Key decisions made"],
    "conventions": ["Patterns/conventions to store permanently"],
    "in-progress": ["Tasks still in progress"],
    "next-actions": ["Priority items for next session"],
    "completed-tasks": ["Kanban task IDs to mark done"]
  }
)
\`\`\`

## What the Workflow Does

1. **Stores to Memory** - Accomplishments, decisions, conventions saved with proper scope tags
2. **Creates Session Summary** - Formatted summary stored for `/catchup` to retrieve
3. **Syncs Kanban** - Marks specified tasks as done
4. **Checks Git** - Reports branch status and unmerged feature branches
5. **Cleans Expired** - Removes old ephemeral memory entries

## After Wrap

The workflow returns:
- Count of items stored
- Git status (branch, uncommitted changes, feature branches)
- Kanban before/after state

Next session, run `/catchup` to restore this context from memory.
```

---

## Usage

### Starting a Session

```
> /catchup
```

Claude will:
1. Query your project memory for recent sessions, decisions, conventions
2. Check kanban for active tasks
3. Report git status
4. Suggest where to start

### Ending a Session

```
> /wrap
```

Claude will:
1. Summarize what was accomplished
2. Store decisions and conventions to memory
3. Update kanban tasks
4. Create session summary for next `/catchup`

---

## Prerequisites

1. **hive-mcp installed and running** - The MCP server must be registered with Claude Code
2. **Emacs daemon running** - hive-mcp connects via emacsclient
3. **hive-mcp.el loaded** - Memory and workflow functions available in Emacs

Verify setup:
```bash
claude mcp list
# Should show: emacs (or hive-mcp)
```

---

## How It Works

### Memory Flow

```
Session N                           Session N+1
─────────────────────────────────────────────────────
Work happens...                     /catchup
     │                                  │
     ▼                                  ▼
/wrap                              Queries memory
     │                              ├── Session summaries
     ▼                              ├── Decisions
mcp_run_workflow("wrap")           ├── Conventions
     │                              └── Kanban tasks
     ├── Stores accomplishments         │
     ├── Stores decisions               ▼
     ├── Stores conventions        Presents context
     ├── Updates kanban            "What to focus on?"
     └── Creates session summary
```

### Memory Types

| Type | Duration | Purpose |
|------|----------|---------|
| Note | Short (7 days) | Session summaries, temporary info |
| Decision | Long (90 days) | Architectural choices, rationale |
| Convention | Permanent | Patterns, coding standards |
| Snippet | Medium (30 days) | Reusable code patterns |

---

## Customization

### Project-Specific Commands

You can also place commands in your project's `.claude/commands/` directory for project-specific workflows.

### Workflow Arguments

The `wrap` workflow accepts these optional arguments:

| Argument | Type | Description |
|----------|------|-------------|
| `accomplishments` | string[] | What was completed |
| `decisions` | string[] | Key decisions made |
| `conventions` | string[] | Patterns to store permanently |
| `in-progress` | string[] | Tasks still being worked on |
| `next-actions` | string[] | Priority items for next session |
| `completed-tasks` | string[] | Kanban task IDs to mark done |

Claude analyzes the session context and populates these automatically.

---

## Troubleshooting

### "Workflow not found"

Ensure hive-mcp workflows are defined in Emacs:

```elisp
M-x eval-expression RET
(hive-mcp-workflow-list)
;; Should include "wrap" and "catchup"
```

### "Memory empty on catchup"

Check memory directory exists:
```bash
ls ~/.emacs.d/.local/cache/hive-mcp/
```

Verify project scope is correct:
```bash
# Check for .hive-project.edn in project root
cat /path/to/project/.hive-project.edn
```

### "MCP tool not available"

Verify hive-mcp is registered:
```bash
claude mcp list
```

Check Emacs server is running:
```bash
emacsclient -e '(hive-mcp-mode)'
```

---

## Related Documentation

- [Memory System](./PROJECT_SUMMARY.md#memory)
- [Workflows](./addon-api.md#workflows)
- [Swarm Multi-Agent](./SWARM_BACKENDS.md)
