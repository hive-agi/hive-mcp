# Swarm Composition Patterns (Bio-Inspired Organization)

Like biological cells, cities, and societies, swarm orchestration benefits from **partitioned, specialized work** with repeatable compositions.

## Proposed Swarm Recipes by Task Type

### Refactoring Task
| Agent | Presets | Role |
|-------|---------|------|
| refactorer | refactorer, solid, clarity | Analysis + extraction |
| tdd | tdd, solid | Regression tests |
| validator | tdd | Compile/load check |
| architect | architect, solid | Large restructures (optional) |

### New Feature Task
| Agent | Presets | Role |
|-------|---------|------|
| architect | architect, solid | Design/planning |
| implementer | clarity, solid | Code writing |
| tester | tdd | Test coverage |
| reviewer | solid, clarity | Code review |

### Bug Fix Task
| Agent | Presets | Role |
|-------|---------|------|
| debugger | debug | Root cause analysis |
| tdd | tdd | Regression test for bug |
| fixer | solid | Minimal fix |

### Documentation Task
| Agent | Presets | Role |
|-------|---------|------|
| documenter | docs | Write docs |
| reviewer | clarity | Review clarity |

## Hierarchical Spawning

Master can spawn a "coordinator" ling with master preset, which then spawns specialized sub-lings for its domain:

```
Master (depth 0)
├── coordinator-refactor (depth 1, master preset)
│   ├── refactorer (depth 2)
│   ├── tdd (depth 2)
│   └── validator (depth 2)
└── coordinator-feature (depth 1, master preset)
    ├── architect (depth 2)
    └── implementer (depth 2)
```

Benefits:
- Depth-2 parallelism for complex tasks
- Domain isolation (coordinator owns its sub-swarm)
- Reduced master cognitive load

## Implementation Notes

- Store recipes as memory entries tagged `swarm-recipe`
- API: `(hive-mcp-swarm-spawn-recipe "refactoring")` → spawns full composition
- Each recipe defines: agents, presets, dispatch order, dependencies

## Priority Tags for Shouts

Shouts can be tagged with priority levels:
- `urgent` - Requires immediate action
- `info` - Informational, no action needed
- `progress` - Status update
- `blocked` - Waiting on something
- `completed` - Task finished
