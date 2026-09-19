# Changelog

Notable changes to hive-mcp. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); versioning follows
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

This file starts at 1.0.0, which absorbs the work that accumulated on
`staging/v0.22.0`. Earlier history is in the git log and the release tags.

## What the version number promises

From 1.0.0 the public seam does not move without a major bump. The seam is
three things:

1. **The MCP tool surface**: the tool root names, their `command` vocabulary
   and the shape of a tool's arguments. Adding a root, a command or an optional
   argument is minor. Removing or renaming one, or making an optional argument
   required, is major.
2. **The addon manifest format** and the `IAddon` contract in
   `io.github.hive-agi/hive-addon`, which reached 1.0.0 alongside this release.
   An addon that mounts against hive-mcp 1.x keeps mounting across 1.x.
3. **The ports in `io.github.hive-agi/hive-spi`**, also 1.0.0. A host cannot
   promise stability over ports that publish no promise of their own, which is
   why those two went first.

What is deliberately NOT promised: anything under an implementation namespace
that the tool surface does not expose, the wire format of internal events, and
the on-disk layout of the stores.

One thing the artifact does promise that is easy to miss: hive-mcp 1.x ships
the datalog backends (datahike, datalevin, datascript) as dependencies. Work is
under way to move them behind ports into sibling libraries
([DEVINCULATE-DATALOG]); when they leave the default tree, that is a major
bump, not a quiet minor, because a consumer's storage would change under it.

## [Unreleased]

### Added

- **`agent spawn` takes `sandbox`.** An optional boolean that rides from the
  tool call through the ling's opts to the headless backend's ctx unchanged.
  The host does not interpret it: hive-agent resolves it against its own
  default (`[:services :agent :sandbox]`), so an omitted flag leaves the key
  out and an explicit `false` survives as an opt-out. A new optional argument,
  so minor.

### Changed

- **Event effects and handlers read the swarm store through a facade, not
  `datascript.core`.** `hive-mcp.swarm.datascript` gained `transact!`,
  `current-db`, `q`, `q-db`, `pull`, `listen!` and `unlisten!`. Writes,
  snapshots and listeners go through `hive-spi.swarm.protocol/ISwarmDb`, so
  they follow whatever store the swarm slot holds and honour the test-conn
  isolation seam; the query legs still call datascript on the db value the port
  hands back, because `ISwarmDb` carries no query method yet. Four kernel
  namespaces (`events.effects.coeffect`, `events.effects.infrastructure`,
  `events.effects.notification`, `events.handlers.claim`) no longer name a
  store vendor, and their `kernel.edn` waivers are gone. `datascript.core` now
  enters the swarm path in exactly one namespace, the one `kernel.edn` already
  names its owner.
- **Telemetry health, the Olympus tool and the Olympus state bridge read the
  swarm store through the same facade.** `telemetry.health` persists and queries
  health events, `tools.olympus` pulls and upserts the Olympus singleton, and
  `transport.olympus.state-bridge` registers its tx listener, all without
  naming `datascript.core` or reaching for a raw connection. The three are
  prerequisites for the telemetry and elisp extractions, which cannot move a
  namespace that holds a store vendor by hand. Requirers of `datascript.core`
  under `src/` drop from 12 to 6, and what remains is the KG store
  (`graph.datascript`, `knowledge-graph.connection`,
  `knowledge-graph.store.datascript`), `agora.dialogue`, the private project
  tree store, and the swarm facade itself.
- **The test tree is two trees.** `test/` loads and runs from the committed
  `deps.edn` alone, with no hive-agent and no hive-datascript on the classpath;
  it is what CI runs. The 53 suites that exercise the swarm addon moved to
  `test-swarm/` and run through `-M:test:test-swarm` with the addon supplied by
  `local.deps.edn`, or sandboxed with `bin/test-sandboxed.sh --swarm`. The
  runner requires every namespace its regex selects before any var-level
  filter runs and does not catch a throwing fixture, so a single addon-coupled
  suite under `test/` used to abort the whole public run.
- **Project identity is kernel code: `hive-mcp.project.scope`.** The scope
  hierarchy (`visible-scopes`, `infer-scope-from-path`, the alias registry)
  and `get-current-project-id` read `.hive-project.edn` files and nothing
  else, but they lived in `hive-mcp.knowledge-graph.scope` and
  `hive-mcp.tools.memory.scope`, two namespaces that leave with hive-memory.
  So the kernel reached into a slice it is meant to outlive to learn which
  project it was in. The code moved to `hive-mcp.project.scope`; hivemind,
  the swarm registry and sync, catchup and the route identity layer require
  it directly, and the swarm memory-scope adapter no longer needs a degrade
  path for it. The two old names stay as facades that call through the kernel
  var on every invocation, so a reload or a redef of the kernel reaches a
  caller that still spells the old name. Fifteen kernel census waivers are
  retired (90 to 75 of the baseline 100).
- **Two more swarm-side host calls go through hive-spi ports.** Agora
  dialogue events reach UI clients through `IFrontendPush/emit!`, and the
  hive-mcp messaging adapter's `emit!` now covers the websocket as well as
  the channel socket, each transport guarded on its own. Swarm dispatch reads
  file staleness through `IDiscStaleness` and no longer requires
  `hive-mcp.knowledge-graph.disc`, which retires one more census waiver
  (75 to 74). With no host adapter installed both degrade to the port's
  noop: no push, no staleness warning, no throw.
- **The kanban board is a port the kernel resolves, not a namespace it
  requires.** `IKanbanRead` (list/get) and `IKanbanWrite` (transition/create)
  live in `hive-mcp.spi.kanban` with a registry in
  `hive-mcp.spi.kanban.registry`; `hive-mcp.tools.kanban.port` registers
  core's provider over the existing kanban domain, and the scheduler, the
  plan-to-kanban pipeline and `memory_kanban/query` reach the board through
  the registry on every call, so an addon that owns the board replaces the
  provider without the callers changing. Catchup no longer gathers the board
  itself: contributors register a block with `hive-mcp.spi.catchup-registry`
  and catchup composes whatever is registered, keyed by `:block/id`, which is
  how `hive-mcp.tools.kanban.catchup-block` supplies the `:kanban` summary.
  Two kernel census waivers are retired (73 to 71 of the baseline 100). The
  port and the block registry are HOST-LOCAL for now; they belong in
  hive-contracts and hive-spi, which have not released them, and the kernel
  does not depend on an unreleased coordinate.

### Fixed

- **The swarm host adapters no longer pin the kernel to namespaces that are
  leaving it.** Four adapters statically required hive-memory, hive-workflows,
  hive-observability and hive-agent extraction targets, which the kernel census
  gate counts as unwaived kernel edges. They now resolve those host functions
  by symbol on the call, and answer what the port's Noop answers once the
  namespace is gone.

## [1.6.0] - 2026-09-16

Three threads: the dispatch tree stopped freezing handler values, the channel
grew a real response budget, and a wrap finally knows which session it belongs
to. Minor rather than patch: commands were added to the tool surface and
nothing in the promised seam was removed or renamed.

### Fixed

- **A hot reload could not rewire a tool handler.** The dispatch tree stored
  the handler's VALUE, so every table held the closure compiled at load time
  and a reload changed nothing the router could see. Handlers are now stored
  as vars and the walker derefs at call time; 55 tables were converted, and a
  ratchet test keeps a referenced handler from regressing to a bare value.
  `dispatch.handler/handler?` is the one definition of what is invocable, and
  it admits a var.
- **The same defect in the event registries.** `reg-fx` / `reg-cofx` /
  `reg-event` guarded by `defonce` or a gate atom captured the pre-reload
  closure and never re-ran. Registration is unconditional wherever the
  registry is key-addressed, which is every site touched here; a gate is
  correct only where registration ACCUMULATES. `hive.events/stale-registrations`
  names whatever is left pointing at replaced code.
- `async-result/drain!` destroyed results enqueued while it was running.
- One coordinator window is one audience, and one global cursor, so a session
  touching several repos no longer reads a global shout once per project.
- A project-scoped reader could not match its own directed `:to`.
- `extensions`: the hive-addon listener seam is armed at `install!`, not on the
  first facade call, so an addon that registers before the facade is touched is
  no longer invisible.
- `saa.core-seed` installs instead of trusting `require` to do the work.
- `swarm`: the signature rule was dead in the stored path, and stale spans
  fenced a file forever.
- Sweepers never release an owner the sweep cannot name.
- `kernel.edn` claims `dispatch`, `hot` and `session`; four namespaces were
  unowned and five kernel edges unwaived.
- `init` logs boot-boundary failures through `rescue-log` instead of
  swallowing them.

### Added

- **Session identity and HCR ownership.** `crystal/session-id` was the calendar
  date, so every concurrent session on a box shared one tag and the first wrap
  to run destroyed the others' unharvested records. `session.identity` is the
  pure algebra (a SessionRef resolved against a world snapshot, plus the
  ownership rules); `session.current` is the thin impure adapter. A coordinator
  owns its descendants, a ling owns only itself, and a row with no session id
  belongs to nobody and is never cleared.
- **Channel response budget.** The budget covers the whole response rather than
  each block in isolation, reports what the drain WITHHELD rather than only
  what it saved, and sends pool memories as pointers instead of bodies. The
  normative split is exported as LLMLingua-2 control tags, and the dictionary
  declines what it cannot honestly compress.
- Directed ling-to-ling delivery over A2A envelopes, with broadcasts metered
  and a directed exchange threaded across turns.
- `registry`: an opt-in compact projection of the advertised tool schemas.
- `cli`: a bare subcommand dispatches to the one root that can own it.
- Prompt caching on the OpenAI-compat wire, tool results included, and a
  provider may buy the 1h cache.
- `swarm`: claim a span of a file rather than the whole file.

### Changed

- Dictionary encoding moved out of `channel.core` into `hive-prompt`.
- Every memory type is classified as a Context Codec commitment.
- The dev REPL alias (`bb repl`, `clj -M:dev-repl`) binds an OS-assigned port
  and can no longer land on the serving nREPL port; `bb serve` is the explicit
  act of booting the server.

## [1.5.0] - 2026-09-14

A belt wave of four reviewed cards, plus one regression the review caught and
one the review missed and CI caught. Minor rather than patch to stay consistent
with every prior release; nothing in the promised seam moved, so 1.4.1 would
have been defensible too.

### Fixed

- `kanban list :created_after` / `:updated_after` compared timestamps
  lexicographically, so a threshold spelled in UTC mis-selected against an
  entry stored at an offset: `2026-08-21T16:55-03:00` is 19:55Z, but `16` vs
  `19` reads as earlier. Both sides now parse to `java.time.Instant`.
- The same change then broke every query it was meant to fix.
  `OffsetDateTime/parse` demands `-03:00`, and kanban stores a **colonless**
  offset (`2026-04-26T00:00:00-0300`). Unparseable became nil, nil made the
  predicate false, and the filters silently matched nothing, a quieter failure
  than the bug being fixed. A colonless-offset parse arm was added. When
  touching a timestamp predicate here, feed it both offset spellings.

### Added

- `recall.canary/skip-regressions` and `with-regressions`: a probe that ran on
  the previous tick and skips on this one is now a `:recall/probe-went-dark`
  fault, and it keeps faulting on every later dark tick instead of going quiet
  after one. `verdict` gained `:ran-labels` to carry the comparison.
- `tools.catchup.bucket-types`: one definition of the seven memory types that
  can land in a catchup bucket. `bundle-cache/bundle-types` now aliases it
  rather than restating the literal set, and a test asserts the two are
  `identical?`, not merely equal.

### Changed

- The SAA orchestrator test exercises the `IObservationScorer` port and the
  `:es/score` extension layering, instead of `requiring-resolve`-ing a
  `hive-claude` symbol that was severed from src and left the test asserting
  `(= observations observations)`.
- Timestamp parsing in `tools.kanban.filters` goes through `rescue` instead of
  three nested `(catch Exception _ ...)`.

## [1.4.0] - 2026-09-14

Released as a minor by decision, although it removes tool roots and commands
and makes model configuration required, which the promise above classes as
major. Read **Removed** and **Configuration now required** before upgrading.

### Removed

- Drones. The only worker model is the agentic ling; cheap parallel work is a
  ling-wave (hive-agent) on a configured model. Gone: the `:drone` agent type
  and `hive-mcp.agent.drone*`, the `agent.core` delegation facade,
  `agent.routing`, `agent.config` task-models, `agent.task-classifier`,
  `agent.cost` and `tools.cost`, `IAgent/upgrade!`.
- The `wave` tool root (dispatch, dispatch-validated, status, review, approve,
  auto-approve, reject), `swarm wave`, and the DSL verbs `w!`, `w?`, `wy`, `wn`.
- The `diff` tools (propose / review / approve / apply).
- Agora debates: `debate`, `debate-status`, `continue`, `staged`,
  `stage-status`, `list type=debate`. Ling `dialogue`, `dispatch`, `consensus`,
  `list`, `join` and `history` stay.
- Change-plan / change-item / wave DataScript storage, the Olympus waves view
  and `GET /api/waves`, drone NATS subjects and callbacks, Prometheus drone
  metrics, the drone presets and `wave-coordinator`.
- Forge drone mode: `spawn_mode drone` is refused with
  `:execution/unsupported-mode`.

### Configuration now required

hive-mcp chooses no model in code. Missing values fail loudly naming the key:

- `agent-defaults.<type>` for every agent type spawned without a model
  (e.g. `hive config set agent-defaults.ling '{:provider :venice :model "<id>"}'`).
- `llm-providers.<provider>.default-model` for a provider used without an
  explicit model.
- `embeddings.<provider>.model` (Ollama, and OpenRouter when its key is set).
- `services.forge.budget-tier-models` when `services.forge.budget-routing` is on.

Removed keys (`agent-defaults.drone`, `services.drone`, `models.task-models`,
`models.routing`) are no longer re-written into `config.edn` by the defaults.

### Changed

- `hive-mcp.agent.drone.error-summary` is now `hive-mcp.agent.error-summary`.

### Fixed

- An addon tool can no longer silently shadow a host tool of the same name.
  Which contribution holds a name is now decided by one pure namespace,
  `hive-mcp.addons.tool-claims`, in registration order:

  1. An addon that both PROVIDES a name and lists it in `excluded-tools`
     CLAIMS it, over a core tool of that name and over every other addon.
  2. An `excluded-tools` entry with no provider refuses other addons' tool of
     that name, and never removes a core tool.
  3. Otherwise a core tool of that name wins and the addon's is refused as
     `:shadows-core`. The legacy supertool form is preserved: a `:native`
     addon's `:consolidated` tool still stands in for a `:consolidated` core
     root of the same name.
  4. Otherwise the first provider holds it; later ones are `:duplicate`.

  Registration order is read from a monotonic counter stamped at
  `register-addon!`, not from the registry map's hash order, so the
  first-wins rules are deterministic across restarts.

  Every refused tool is reported rather than dropped in silence:
  `hive-mcp.addons.core/resolve-addon-tools` returns `:installed`,
  `:refused` (each with a reason and the holder) and `:claims`.
  `active-addon-tools` returns just `:installed`, as before.

  A claim also DROPS the host's own tool of that name when the surface is
  built, in `build-server-spec` and `refresh-tools!`. Previously both were
  concatenated, so a claimed name appeared twice and which one answered
  depended on fold order.

### Changed

- `hive-mcp.tools.registry/core-tools` is now public: the host's own tool
  defs (channel tools + domain roots), independent of the caller's role, so a
  name the child-ling set leaves out still counts as a core name when addon
  tools are resolved against it.

## [1.1.2]

A patch release. The tool surface, the manifest format and the ports are
where 1.1.1 left them. Every entry is a fix or a repository-layout change a
consumer does not see; the one behaviour change is a default on `swarm spawn`.

### Fixed

- `kanban list` silently truncated. The store window was hard-coded (100 for
  a bare list, 500 once a status or a post-filter narrowed it) and the
  caller's `limit` only cut the page, so `status=todo` answered 500 of 722
  cards and no `limit` recovered the rest. Catchup's bucket counts had the
  same shape at 200, so the header reported the window as the count. The
  list pipeline is now a pure planner (`kanban.list.plan`) over malli value
  objects (`kanban.list.schema`) and an `IBoardSource` port
  (`kanban.list.source`); the store window is the whole scoped board, one
  constant that list, stats and catchup share, and the handler namespace no
  longer freezes the query functions in `def` aliases.
- The dag-wave scheduler ignored `:depends-on`. `get-kanban-task` resolved
  cards through the vector slot, which answers nil for a kanban-slot card, so
  every dependency counted as done and every remaining todo dispatched in
  wave 1. It reads through the kanban facade now, and a card present only in
  the kanban slot counts as NOT done until its id is in the completed set.
- An addon that contributed a verb to a consolidated tool root got routing
  but no schema slot for its own arguments, and the MCP layer forwards only
  declared params, so `swarm ling-wave dispatch` could never carry
  `providers`. `build-merged-tool` is now applied to every consolidated
  tool-def on each rebuild of the tool table. A core `command` enum is
  extended only when the core declares one, and a colliding property unions
  into `anyOf` instead of the addon retyping the core's.
- The NATS client reconnected at most 5 times, so one nats-server restart
  closed the host connection for the life of the process and every
  NATS-backed feature degraded silently. `:max-reconnects` defaults to -1.
- A ling's deliberate `swarm hivemind shout` vanished into the progress
  digest, which keeps the last `:progress` row per agent. The hivemind path
  marks its data `:deliberate?`, the marker rides the local and backbone
  paths, and the digest never folds such a row.
- The first hivemind read under a new project-id replayed the whole global
  shout history from timestamp 0. Global shouts are judged against, and
  advance, one `[reader "global"]` cursor whatever project the read names;
  project shouts keep their per-project cursor.

### Changed

- A ling that spawns without passing `parent` records itself as the spawn's
  parent, so a grandchild's shouts stop at the ling instead of reaching the
  coordinator. A coordinator-lane caller keeps the spawn root-level, and an
  explicit `parent` still wins.
- The swarm root resolves its subdomain schemas through the shared
  `lazy-resolve-schema-props`, which now accepts a `tools` vector, instead of
  its own copies of the merge.
- The repository root is down to what a checkout needs. The presets moved from
  `presets/` to `resources/presets/`, so they ship in the jar and the image; the
  file fallback (`presets.dir`, `HIVE_MCP_PRESETS_DIR`) defaults to that path.
  The shared test-support namespaces (`hive-mcp.knowledge-graph.store.fixtures`,
  `.harness`, `hive-mcp.recall.golden`) moved from `testing/` to `src/` under the
  same names, so consumers such as hive-knowledge see no change. The observability
  stack configs moved from `config/` to `docker/observability/`, and the test
  sandbox template to `dev/`.
- One test tree. The backend-coupled suites moved from `test-backends/` into
  `test/` under the namespace prefix `hive-mcp.backends`; the default runners
  exclude that prefix by namespace regex and the `:test-backends` alias selects
  exactly it, so `clj -Sdeps "$(cat local.deps.edn)" -M:test:test-backends`
  still runs them and nothing else.

### Removed

- `python/` (the hive-tools Claude Agent SDK wrappers, never published and
  referenced by nothing), `modules/clj-kondo-mcp` (a stale copy of the
  clj-kondo-mcp repository), `recipes/` and the MELPA check (the elisp lives
  in hive-emacs, which validates it), `seeds/` (its ten conventions now live in
  hive memory), `k8s/ollama-embed` (a nomic-embed-text deployment nothing
  referenced), and the root-level `test-int/`, `PLAN.md`, `leak.jfc` and
  `fp-arg-order-convention.md`.

## [1.1.1]

A patch release. The tool surface, the manifest format and the ports are
where 1.1.0 left them; one existing optional argument gained a second reader,
and two defaults that were quietly costing a core or a model load are gone.

### Fixed

- A `git` or `magit` batch-commit operation whose `files` was a
  space-separated string staged nothing and then committed whatever the index
  already held under that operation's message. `files` now normalizes to
  `:all` or a vector of paths (a list, a single path, or a whitespace-separated
  string), and a commit that names paths stages them and verifies the
  restricted index first: a path absent from the working tree, an empty
  `git diff --cached --name-only` for those paths, or any unreadable verdict
  fails the operation instead of falling through to the commit. The `files`
  schema property is spliced from one definition in `tools.core` into both
  tool-defs, so `git` and `magit` cannot disagree about whether a list is
  accepted.
- The lsp-sidecar image and `analyze.sh` run git non-interactively
  (`GIT_TERMINAL_PROMPT=0`, SSH in batch mode with a bounded connect timeout),
  so an uncached or private `:git/url` dependency met during `clojure -Spath`
  fails fast instead of blocking the sidecar on a credential, host-key or
  passphrase prompt.

### Changed

- The `remote` argument, which `fetch` already honoured, now also names the
  remote `push` pushes the current branch to. Absent or blank it contributes
  nothing, and the elisp emitted for a push is byte-identical to before.
- Datalevin-backed slots are opened with `{:background-sampling? false}`. The
  sampler picked a random attribute every 10 s and rescanned it whenever a
  carto scan moved its count by 5%, pinning a core for the length of the
  scan; the query planner samples lazily on first use, so queries are
  unaffected.
- Every legacy Ollama embedding default (config merge, server init,
  `ollama-config`) named `nomic-embed-text`, which configured the memory
  collection with a 768-dimension model at boot and loaded nomic into Ollama
  next to the `qwen3-embedding:4b` lane that actually serves memory. The
  defaults now name `qwen3-embedding:4b`, and the Ollama model table carries
  its 2560 dimensions. A deployment that relied on the old implicit default
  keeps it by setting `embeddings.ollama.model` explicitly.
- The presets gain the v2 axes (carto-first, commit-hygiene, cppb-stratified,
  ddd-ports, gitops-safety, malli-first, memory-crystallize, ocp-data,
  repl-first, subtask-worker, trifecta), each stating one discipline, with
  `ROLES-v2.md` naming the bundles.

## [1.1.0]

The first release after the seam froze. Nothing here moves the tool surface,
the manifest format or the ports; three of the changes are hosts behaving
correctly where a stub or an addon had been standing in for them.

### Fixed

- `multi` on an unextended host now sorts waves and reports cycles instead of
  flattening every op into wave 1. Without the batch addon, `assign-waves`
  fell back to one flat wave, so a dependency chain succeeded at its root and
  errored every dependent op with "dependencies failed", and `detect-cycles`
  fell back to `[]`, so a real cycle validated. Both fall back to
  `hive.events.multi` (Kahn's algorithm, a cycle-reporting validator), which
  the plan namespace already required. A registered extension still wins.
  This survived a green suite because every multi test installed a stub that
  registered the correct implementation; the new tests force every `:bx/*`
  lookup to miss, which is the shape production has.
- On that same unextended host, a `$ref:` the host cannot parse (no `:bx/a`
  extension) is classified as a broken ref and the op is skipped, instead of
  the literal `"$ref:..."` string reaching the handler as a value. The flat
  wave had hidden this: every dependent op failed on its dependencies before
  the classifier could be reached.
- `memory migrate-scoped` puts the tag into the store query instead of
  enumerating a whole project and filtering in Clojure, and refuses a
  migration that resolves zero targets, naming the tag, the project and the
  query, where it used to report `{:migrated 0}` as success.
- The addon loader registers an init answer's `:metadata :extensions` only
  when it is a `{keyword fn}` map, and mirrors that on teardown. An addon
  that reported something else there (a language tier's set of file
  extensions) tripped the registry assert after its own side effects had
  landed, and the teardown then walked the value as map entries; the same
  assert was aborting the tool-registry refresh after a hot reload, leaving
  MCP verbs on pre-reload handlers. A non-map is now logged and skipped.

### Changed

- `io.github.hive-agi/hive-hot` resolves from Clojars (0.1.7) instead of a
  `:git/tag` coordinate. Same version; the last hive-agi library in `:deps`
  that was not an `:mvn/version`.
- The starter notes state what `hive-tmux` needs before it can mount (tmux,
  Python 3, libtmux on the interpreter libpython-clj binds) and why its pin is
  0.1.1.

## [1.0.0]

1.0.0 is not a feature release. It is the release where the seam stops moving,
and the work below is what had to be true first: the committed tree boots and
tests on a clean checkout, the shipped container binds every port it announces,
the host no longer calls its own deprecated vars, and the two libraries the
contract rests on (hive-spi, hive-addon) publish 1.0.0 promises of their own.

Two large refactors stay open on purpose, because neither changes the seam:
[MQ-ADOPT] (internal addon-loader cutover) and [DEVINCULATE-DATALOG] (moving
the datalog drivers behind ports). Shipping 1.0 says the contract is stable,
not that the roadmap is empty.

### Changed

- `io.github.hive-agi/hive-spi` 0.2.1 to **1.0.0** and
  `io.github.hive-agi/hive-addon` 0.3.12 to **1.0.0**. Both gained a CHANGELOG
  and a versioning statement saying what a major, minor and patch mean for an
  implementor; neither changed a contract to get there. This pin is the
  substance of hive-mcp's own promise, not a routine bump: a host cannot
  promise stability over ports that publish none.
- `clj-kondo` over `src`: 129 warnings to 67, 0 errors. Twenty-six files
  carried a require or a refer nothing used. `tools/registry.clj` is the
  opposite case and now states it: its twelve legacy consolidated requires are
  loaded for their registration side effect, excluded by name in an ns-level
  linter config rather than by switching the linter off, so a genuinely dead
  require there still reports.

### Removed

Deprecations the host itself still called, resolved per site rather than left
standing. A deprecation a host keeps calling never lands: callers feel no
pressure to move and the var cannot be removed without breaking the host.

- `hive-mcp.crystal.hooks/harvest-all`, `/harvest-session-progress`,
  `/harvest-completed-tasks`, `/harvest-git-commits` and `/crystallize-session`.
  These were backward-compat delegates from the harvest decomposition. Callers
  use `hive-mcp.crystal.harvest.collect/*` and
  `hive-mcp.crystal.synthesis/synthesize` directly, which is what the
  deprecation notices said to do. `crystal.hooks` now holds only event handlers
  and hook registration, as its own docstring claimed.
- `hive-mcp.knowledge-graph.connection.writer/drain-writer!` and its re-export
  on `connection`. It was a one-line alias for `flush-pending!`; the six call
  sites now call `flush-pending!`.
- `hive-mcp.agent.hive-agent-bridge` and
  `hive-mcp.agent.drone.backend.hive-agent`, deprecated since 0.16.0. The
  bridge dispatches through the `:ag/run` extension key, which no addon in the
  ecosystem registers any more (hive-agent contributes `:ag/context`,
  `:ag/tools`, `:ag/loop-factory`, `:ag/loop-backend`, `:ag/llm-router`), so
  the backend could only ever answer "hive-agent is not available on
  classpath". Nothing in `src` required either namespace: `ext-router` lists
  only `:sdk-drone` and `:agentic-loop`. Selecting `:hive-agent` now falls to
  the `resolve-backend` default, which names the registered backends.

### Added

- `dev/foss_compliance.clj`: measures every public hive-agi repository against
  the packaging, mount-contract, host-coupling, version, CI, licence, README
  and dependency checks. It enumerates the org from the GitHub API rather than
  from a curated list.
- `dev/addon_boot_probe.clj`: constructs every addon manifest on the classpath
  and asserts the result satisfies `IAddon`, reporting the cause when it does
  not.
- `CONTRIBUTING.md`, including the four rules an addon must follow to be
  mountable by any host.
- CI job `boot`: loads the server closure from the committed `deps.edn`, both
  bare core and starter overlay. The existing `deps` job runs `clojure -P`,
  which resolves a tree without ever compiling against it, and every
  workstation hides the difference behind a gitignored `local.deps.edn`. This
  job is the only check in the repo that runs without those overrides.
- `:coverage` alias (cloverage), composing with `:test-unit` so the measured
  suite is the CI suite. Scope it with `--ns-regex`: instrumenting all 428 unit
  namespaces in one JVM is a multi-gigabyte run. First measurement, the
  `hive-mcp.addons.*` slice: 70.15% forms, 79.97% lines.

### Fixed

- Every desktop launch (`-M:dev:nrepl`) died with `BindException: Address
  already in use` on port 7910 as soon as the embedded nREPL actually started
  (0b61a5e). `dev/user.clj` booted the system before `nrepl.cmdline` ran, so
  the `:hive/nrepl` component bound 7910 first and the alias's second nREPL
  had nothing left to bind. The `:nrepl` alias now runs
  `hive-mcp.server.core`, the container's main, and the embedded server is
  the only nREPL: it resolves refactor-nrepl beside CIDER when present and
  writes `.nrepl-port`. `bin/hive-mcp-foss` passes `HIVE_NREPL_PORT` through
  `HIVE_MCP_NREPL_PORT` instead of appending `--port`.
- The container built and then died on boot. `hive-mcp.events.registry`
  delegates to `hive.events.router/get-event`, `get-interceptors` and
  `append-interceptor!`, none of which existed in a published hive-events jar
  (0.5.8 and 0.5.9 ship a byte-identical `router.cljc` defining none of them).
  Fixed by publishing hive-events 0.5.10 and pinning it.
- The unit gate aborted at load on a clean checkout: the store-contract runners
  require `hive-test.memory.store-contract`, which no published hive-test
  carried. Fixed by publishing hive-test 0.3.19 and raising the three pins from
  0.3.15.
- The k8s-headless container booted clean and opened three of its five ports.
  A2A (7912) and WebSocket MCP (7920) were decided by a config store other
  than the one the system handed the component: Integrant merged the
  profile's `:enabled true`, printed it, and the start function then asked
  `config/get-service-value`, which defaults to false and has no `config.edn`
  to read in a container. Both statuses were derived from the request rather
  than the result, so neither could report it. nREPL (7910) died on a
  `NullPointerException` in `nrepl.server/default-handler`: CIDER lists its
  middleware by symbol, those namespaces are not loaded in the container, and
  one nil in the middleware vector loses the whole server. Ports now measured
  bound and the container reports healthy.
- `StdioBridge` and `NoopMcpBridge` declared `IAddon` while omitting
  `excluded-tools` and `hooks`. Both threw `AbstractMethodError`, which the
  host's `rescue` at the call sites turned into "this addon contributes
  nothing" with no error surfaced anywhere: a bridge addon's hooks were never
  registered and its tool exclusions never applied.
- Nine namespaces called `clojure.string/*` or `taoensso.timbre/*` fully
  qualified without requiring them, resolving only by load-order luck.
- `hive-mcp.tools.kanban.events/edit-fx` was defined twice, byte-identically.
- Namespaces defining `reset!` or `run!` now declare the `:refer-clojure
  :exclude`, so the shadowing is intentional in the source instead of a warning
  on every boot.
- `hive-mcp.addons.terminal` is now a re-export of `hive-addon.terminal`
  rather than the definition site of `ITerminalAddon`. Defining a companion
  protocol in the host left vessel addons with no contract to depend on, so
  they had to reify a host namespace to implement it. Historical qualified
  names still resolve.

## [0.22.0] - 2026-09-01

### Added

- FOSS starter pack: `starter.deps.edn`, merged over `deps.edn` by
  `bin/hive-mcp-foss` (`HIVE_STARTER=0` opts out). `deps.edn` itself stays free
  of backend coordinates.
- Addons can be injected after boot, and a late contribution reaches the
  advertised tool surface without a restart.
- The catchup bundle is cached across sessions, with invalidation driven by
  write events.
- Shared test fixtures ship in the jar so consumers can load them.
- Declared addon config is hydrated at start time, and `:runtime/ports` is
  injected at `init-addon!` rather than only at manifest discovery.
- Per-call `timeout_ms` on the Emacs-backed git tools.

### Fixed

- A knowledge-graph cluster is defined by live member count, not by a live
  ratio.
- `get-entries-projected` receives the projection map it actually reads.
- Memory supertool subdomain schema params are folded in correctly.
- The pass secret store is resolved the way `pass` itself finds it.

### Changed

- The Docker image runs the server from source; there is no uber task.

[Unreleased]: https://github.com/hive-agi/hive-mcp/compare/v1.1.2...HEAD
[1.1.2]: https://github.com/hive-agi/hive-mcp/compare/v1.1.1...v1.1.2
[1.1.1]: https://github.com/hive-agi/hive-mcp/compare/v1.1.0...v1.1.1
[1.1.0]: https://github.com/hive-agi/hive-mcp/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/hive-agi/hive-mcp/compare/v0.22.0...v1.0.0
[0.22.0]: https://github.com/hive-agi/hive-mcp/compare/v0.21.1...v0.22.0
