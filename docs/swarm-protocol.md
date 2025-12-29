# Swarm Protocol Specification

**Version:** 0.1.0
**Status:** Draft
**Author:** Claude + BuddhiLW

## Overview

The Swarm Protocol defines communication between a **Master Claude** (orchestrator) and multiple **Slave Claude** instances (workers) running in Emacs vterm buffers.

```
┌─────────────────────────────────────────────────────────────┐
│                    Master Claude                            │
│                 (current session)                           │
└────────────────────────┬────────────────────────────────────┘
                         │ MCP Protocol
                         v
┌─────────────────────────────────────────────────────────────┐
│                 emacs-mcp-swarm.el                          │
│              (Orchestration Layer)                          │
│                                                             │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │ Session Mgr │  │ Task Queue  │  │  Collector  │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└────────────────────────┬────────────────────────────────────┘
                         │ vterm-send-string / buffer-read
         ┌───────────────┼───────────────┐
         v               v               v
    ┌─────────┐     ┌─────────┐     ┌─────────┐
    │ Slave 1 │     │ Slave 2 │     │ Slave 3 │
    │ (vterm) │     │ (vterm) │     │ (vterm) │
    │ claude  │     │ claude  │     │ claude  │
    └─────────┘     └─────────┘     └─────────┘
```

## Core Concepts

### Identifiers

| ID Type | Format | Example |
|---------|--------|---------|
| `slave_id` | `swarm-{name}-{timestamp}` | `swarm-tester-1735489200` |
| `task_id` | `task-{slave}-{sequence}` | `task-tester-001` |
| `session_id` | `session-{date}-{random}` | `session-20251229-a3f2` |

### Slave States

```
           spawn
             │
             v
    ┌────────────────┐
    │     IDLE       │ ◄──────────────┐
    └───────┬────────┘                │
            │ dispatch                │ complete/fail
            v                         │
    ┌────────────────┐                │
    │    WORKING     │ ───────────────┘
    └───────┬────────┘
            │ error/timeout
            v
    ┌────────────────┐
    │     ERROR      │ ──► restart ──► IDLE
    └───────┬────────┘
            │ kill
            v
    ┌────────────────┐
    │   TERMINATED   │
    └────────────────┘
```

---

## Data Structures

### SlaveConfig

Configuration for spawning a new slave.

```json
{
  "name": "tester",
  "role": "test-runner",
  "cwd": "/path/to/project",
  "system_prompt": "You are a test specialist. Run tests and report failures concisely.",
  "tool_restrictions": ["Bash", "Read", "Grep"],
  "max_concurrent_tasks": 1,
  "timeout_ms": 300000,
  "environment": {
    "CLAUDE_SWARM_ROLE": "slave",
    "CLAUDE_SWARM_MASTER": "session-20251229-a3f2"
  }
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | yes | Human-readable slave name |
| `role` | string | no | Predefined role (see Roles) |
| `cwd` | string | no | Working directory (default: project root) |
| `system_prompt` | string | no | Custom system prompt prepended to tasks |
| `tool_restrictions` | string[] | no | Allowed tools (default: all) |
| `max_concurrent_tasks` | int | no | Task queue limit (default: 1) |
| `timeout_ms` | int | no | Default task timeout (default: 300000) |
| `environment` | object | no | Extra environment variables |

### SlaveState

Current state of a slave instance.

```json
{
  "slave_id": "swarm-tester-1735489200",
  "name": "tester",
  "role": "test-runner",
  "status": "working",
  "buffer": "*swarm-tester*",
  "pid": 12345,
  "current_task": "task-tester-003",
  "tasks_completed": 2,
  "tasks_failed": 0,
  "spawned_at": "2025-12-29T10:00:00Z",
  "last_activity": "2025-12-29T10:05:30Z",
  "uptime_ms": 330000
}
```

### TaskDispatch

Message sent from Master to Slave via prompt.

```json
{
  "task_id": "task-tester-003",
  "type": "dispatch",
  "prompt": "Run the test suite and report any failures",
  "context": {
    "files": ["/src/core.clj", "/test/core_test.clj"],
    "memory_keys": ["conventions", "recent-changes"],
    "git_ref": "feature/swarm"
  },
  "constraints": {
    "timeout_ms": 120000,
    "max_tokens": 4000,
    "require_tools": ["Bash"],
    "forbid_tools": ["Write", "Edit"]
  },
  "priority": "high",
  "callback": {
    "on_complete": "notify-master",
    "on_fail": "retry-once"
  }
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `task_id` | string | yes | Unique task identifier |
| `type` | string | yes | Always "dispatch" |
| `prompt` | string | yes | The actual prompt for the slave |
| `context` | object | no | Additional context to inject |
| `context.files` | string[] | no | Files to read before task |
| `context.memory_keys` | string[] | no | Memory entries to include |
| `context.git_ref` | string | no | Git branch/commit context |
| `constraints` | object | no | Execution constraints |
| `constraints.timeout_ms` | int | no | Task-specific timeout |
| `constraints.max_tokens` | int | no | Response length limit |
| `constraints.require_tools` | string[] | no | Tools that must be available |
| `constraints.forbid_tools` | string[] | no | Tools to disable |
| `priority` | string | no | "critical", "high", "normal", "low" |
| `callback` | object | no | Post-completion actions |

### TaskResponse

Response collected from Slave output.

```json
{
  "task_id": "task-tester-003",
  "type": "response",
  "status": "completed",
  "result": {
    "summary": "All 42 tests passed",
    "output": "...",
    "artifacts": [
      {"type": "file", "path": "/tmp/test-report.xml"}
    ]
  },
  "metrics": {
    "started_at": "2025-12-29T10:05:00Z",
    "completed_at": "2025-12-29T10:05:30Z",
    "duration_ms": 30000,
    "tokens_used": 1250,
    "tool_calls": 5
  },
  "errors": []
}
```

| Field | Type | Description |
|-------|------|-------------|
| `task_id` | string | Matches dispatch task_id |
| `type` | string | Always "response" |
| `status` | string | "completed", "failed", "timeout", "cancelled" |
| `result` | object | Task output |
| `result.summary` | string | Brief summary (first line or extracted) |
| `result.output` | string | Full response text |
| `result.artifacts` | object[] | Generated files/data |
| `metrics` | object | Execution metrics |
| `errors` | object[] | Any errors encountered |

### SwarmStatus

Aggregate status of all slaves.

```json
{
  "session_id": "session-20251229-a3f2",
  "status": "active",
  "slaves": {
    "total": 3,
    "idle": 1,
    "working": 2,
    "error": 0
  },
  "tasks": {
    "queued": 5,
    "in_progress": 2,
    "completed": 15,
    "failed": 1
  },
  "slaves_detail": [
    {"slave_id": "swarm-tester-1735489200", "status": "working", "current_task": "task-tester-003"},
    {"slave_id": "swarm-reviewer-1735489210", "status": "working", "current_task": "task-reviewer-001"},
    {"slave_id": "swarm-docs-1735489220", "status": "idle", "current_task": null}
  ]
}
```

---

## Predefined Roles

Roles provide preset configurations for common slave specializations.

| Role | System Prompt | Tool Restrictions | Use Case |
|------|---------------|-------------------|----------|
| `tester` | "Run tests, report failures concisely" | Bash, Read, Grep | Test execution |
| `reviewer` | "Review code for bugs, style, security" | Read, Grep, Glob | Code review |
| `documenter` | "Write/update documentation" | Read, Write, Edit | Docs |
| `refactorer` | "Refactor code, maintain behavior" | Read, Write, Edit, Bash | Refactoring |
| `researcher` | "Search codebase, answer questions" | Read, Grep, Glob, WebSearch | Research |
| `fixer` | "Fix bugs, address issues" | All | Bug fixes |

### Role Definition Schema

```json
{
  "role_id": "tester",
  "display_name": "Test Runner",
  "system_prompt": "You are a test specialist. Your job is to:\n1. Run the test suite\n2. Report failures clearly\n3. Suggest fixes if obvious\n\nBe concise. Output test results in a structured format.",
  "tool_restrictions": ["Bash", "Read", "Grep", "Glob"],
  "default_timeout_ms": 180000,
  "response_format": {
    "require_summary": true,
    "max_lines": 100
  }
}
```

---

## Communication Protocol

### Prompt Dispatch Method

Since Claude Code runs in a terminal, we communicate via stdin. The orchestrator:

1. **Wraps the prompt** with task metadata as a header comment
2. **Sends via vterm** using `vterm-send-string`
3. **Monitors buffer** for completion markers

#### Dispatch Format

```
# SWARM_TASK_BEGIN
# task_id: task-tester-003
# priority: high
# timeout_ms: 120000

Run the test suite in /project/tests and report any failures.
Focus on:
1. Unit tests
2. Integration tests

Report format:
- Total tests
- Passed/Failed counts
- Failure details

# SWARM_TASK_END
```

#### Response Detection

The orchestrator watches for these patterns in the slave's buffer:

1. **Completion marker**: Slave returns to prompt (`❯` or `$`)
2. **Explicit marker**: `# SWARM_RESPONSE_COMPLETE`
3. **Timeout**: No activity for `timeout_ms`
4. **Error patterns**: "Error:", "Exception:", process exit

### Response Collection Method

```elisp
(defun emacs-mcp-swarm--collect-response (slave-id)
  "Collect response from SLAVE-ID buffer."
  (let* ((slave (emacs-mcp-swarm--get-slave slave-id))
         (buffer (plist-get slave :buffer))
         (task-start (plist-get slave :task-start-marker)))
    (with-current-buffer buffer
      (buffer-substring-no-properties task-start (point-max)))))
```

---

## MCP Tool Interface

### swarm_spawn

Spawn a new slave instance.

**Parameters:**
```json
{
  "name": "tester",
  "role": "tester",
  "cwd": "/path/to/project"
}
```

**Returns:**
```json
{
  "slave_id": "swarm-tester-1735489200",
  "status": "idle",
  "buffer": "*swarm-tester*"
}
```

### swarm_dispatch

Send a task to a slave.

**Parameters:**
```json
{
  "slave_id": "swarm-tester-1735489200",
  "prompt": "Run all tests",
  "timeout_ms": 120000,
  "priority": "high"
}
```

**Returns:**
```json
{
  "task_id": "task-tester-003",
  "status": "dispatched",
  "queued_at": "2025-12-29T10:05:00Z"
}
```

### swarm_status

Get swarm or slave status.

**Parameters:**
```json
{
  "slave_id": null
}
```

**Returns:** `SwarmStatus` object (see above)

### swarm_collect

Collect response from completed task.

**Parameters:**
```json
{
  "task_id": "task-tester-003",
  "timeout_ms": 5000,
  "blocking": true
}
```

**Returns:** `TaskResponse` object (see above)

### swarm_broadcast

Send same prompt to multiple slaves.

**Parameters:**
```json
{
  "prompt": "Update your git branch: git pull origin main",
  "slave_filter": {"role": "fixer"},
  "wait": false
}
```

**Returns:**
```json
{
  "dispatched": [
    {"slave_id": "swarm-fixer-1", "task_id": "task-fixer-1-005"},
    {"slave_id": "swarm-fixer-2", "task_id": "task-fixer-2-003"}
  ]
}
```

### swarm_kill

Terminate slave(s).

**Parameters:**
```json
{
  "slave_id": "swarm-tester-1735489200"
}
```
or
```json
{
  "all": true
}
```

**Returns:**
```json
{
  "killed": ["swarm-tester-1735489200"],
  "count": 1
}
```

---

## Safety & Constraints

### Recursion Prevention

```json
{
  "swarm_config": {
    "max_depth": 2,
    "max_slaves": 5,
    "max_tasks_per_slave": 10,
    "detect_recursion": true
  }
}
```

**Detection method:**
- Environment variable `CLAUDE_SWARM_DEPTH` incremented on spawn
- Slaves check depth before allowing nested spawns
- Hard limit prevents infinite recursion

### Rate Limiting

```json
{
  "rate_limits": {
    "spawns_per_minute": 10,
    "dispatches_per_minute": 60,
    "concurrent_tasks": 10
  }
}
```

### Resource Cleanup

On master session end or explicit cleanup:
1. Send SIGTERM to all slave processes
2. Wait 5s for graceful shutdown
3. Send SIGKILL to remaining
4. Clean up vterm buffers
5. Save task history to memory

---

## Example Workflow

### Parallel Test + Review

```clojure
;; Master spawns two slaves
(swarm_spawn {:name "tester" :role "tester" :cwd "/project"})
(swarm_spawn {:name "reviewer" :role "reviewer" :cwd "/project"})

;; Dispatch parallel tasks
(swarm_dispatch {:slave_id "swarm-tester-..."
                 :prompt "Run all tests, report failures"})
(swarm_dispatch {:slave_id "swarm-reviewer-..."
                 :prompt "Review changes in src/core.clj for bugs"})

;; Poll for completion
(loop []
  (let [status (swarm_status {})]
    (if (= 0 (:working (:slaves status)))
      :done
      (do (Thread/sleep 5000) (recur)))))

;; Collect results
(def test-results (swarm_collect {:slave_id "swarm-tester-..."}))
(def review-results (swarm_collect {:slave_id "swarm-reviewer-..."}))

;; Aggregate and report
(println "Tests:" (:summary (:result test-results)))
(println "Review:" (:summary (:result review-results)))

;; Cleanup
(swarm_kill {:all true})
```

---

## Appendix: Elisp Data Structures

```elisp
;; Slave registry
(defvar emacs-mcp-swarm--slaves (make-hash-table :test 'equal)
  "Hash table of slave-id -> slave-plist.")

;; Slave plist structure
'(:slave-id "swarm-tester-1735489200"
  :name "tester"
  :role "tester"
  :status idle  ; idle | working | error | terminated
  :buffer #<buffer *swarm-tester*>
  :process #<process claude>
  :cwd "/project"
  :current-task nil
  :task-queue ()
  :tasks-completed 0
  :tasks-failed 0
  :spawned-at "2025-12-29T10:00:00Z"
  :config (:timeout-ms 300000 :max-concurrent 1))

;; Task plist structure
'(:task-id "task-tester-003"
  :slave-id "swarm-tester-1735489200"
  :prompt "Run tests"
  :status pending  ; pending | dispatched | working | completed | failed | timeout
  :priority high
  :dispatched-at nil
  :completed-at nil
  :result nil
  :error nil)
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1.0 | 2025-12-29 | Initial draft |
