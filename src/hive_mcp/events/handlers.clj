(ns hive-mcp.events.handlers
  "Event handlers for hive-mcp events.

   This namespace is a FACADE that re-exports handlers from domain-specific modules:
   - handlers.task    - Task lifecycle (:task/complete, :task/shout-complete, :git/commit-modified)
   - handlers.ling    - Ling lifecycle (:ling/started, :ling/completed, :ling/ready-for-wrap)
   - handlers.session - Session lifecycle (:session/end, :session/wrap)
   - handlers.kanban  - Kanban state (:kanban/done, :kanban/sync)
   - handlers.crystal - Wrap/crystallize (:crystal/wrap-request, :crystal/wrap-notify)
   - handlers.claim   - File claims (:claim/file-released, :claim/notify-waiting)
   - handlers.hot-reload - Hot reload lifecycle (:hot/reload-start, :hot/reload-success, :file/changed)
   - handlers.kg     - Knowledge Graph (:kg/edge-created, :kg/edge-updated, :kg/edge-removed, :kg/node-promoted)
   - handlers.agora  - Agora dialogue events (:agora/turn-dispatched, :agora/timeout, :agora/turn-completed, :agora/dispatch-next, :agora/consensus)
   - handlers.saa    - SAA workflow (:saa/started, :saa/phase-complete, :saa/completed, :saa/failed)
   - handlers.lifecycle - GC lifecycle (:lifecycle/sweep)

   ## Usage
   ```clojure
   (require '[hive-mcp.events.handlers :as handlers])
   (handlers/register-handlers!)
   ```

   ## Available Events
   - :task/complete         - Signal task completion to hivemind
   - :task/shout-complete   - Broadcast completion with message (P5-1)
   - :git/commit-modified   - Git commit if files changed (P5-2)
   - :ling/started          - Ling spawned and initialized (EVENTS-03)
   - :ling/completed        - Ling finished all work (EVENTS-03)
   - :ling/ready-for-wrap   - Auto-wrap hook on ling completion
   - :session/end           - Session ending, trigger auto-wrap (EVENTS-06)
   - :session/wrap          - Trigger wrap workflow (P5-3)
   - :kanban/done           - Kanban task completed (EVENTS-09)
   - :kanban/sync           - Sync kanban at session end (P5-4)
   - :crystal/wrap-request  - Unified wrap path (Option A)
   - :crystal/wrap-notify   - Wrap notification for HIVEMIND piggyback
   - :claim/file-released   - File claim released, notify waiting lings
   - :claim/notify-waiting  - Send targeted shout to waiting ling
   - :system/error          - Structured error telemetry (Telemetry Phase 1)
   - :hot/reload-start      - Hot reload process started
   - :hot/reload-success    - Hot reload completed successfully
   - :file/changed          - File modification detected by watcher
   - :kg/edge-created       - New KG edge added
   - :kg/edge-updated       - KG edge confidence changed
   - :kg/edge-removed       - KG edge deleted
   - :kg/node-promoted      - Knowledge promoted to parent scope
   - :agora/turn-dispatched   - Agora turn dispatched to ling
   - :agora/timeout           - Agora timeout
   - :agora/turn-completed    - Unified ling turn completion
   - :agora/dispatch-next     - Relay the next turn to a ling participant
   - :agora/consensus         - Crystallize dialogue result to memory
   - :saa/started             - SAA workflow initiated
   - :saa/phase-complete      - SAA phase transition (Silence->Abstract->Act)
   - :saa/completed           - SAA workflow finished successfully
   - :saa/failed              - SAA workflow error
   - :lifecycle/sweep         - Bounded atom GC sweep (gc-fix-4)"

  (:require [hive-mcp.events.handlers.task :as task]
            [hive-mcp.events.handlers.ling :as ling]
            [hive-mcp.events.handlers.session :as session]
            [hive-mcp.events.handlers.kanban :as kanban]
            [hive-mcp.events.handlers.crystal :as crystal]
            [hive-mcp.events.handlers.claim :as claim]
            [hive-mcp.events.handlers.system :as system]
            [hive-mcp.events.handlers.hot-reload :as hot-reload]
            [hive-mcp.events.handlers.kg :as kg]
            [hive-mcp.events.handlers.agora :as agora]
            [hive-mcp.events.handlers.saa :as saa]
            [hive-mcp.events.handlers.memory-read :as memory-read]
            [hive-mcp.events.handlers.lifecycle :as lifecycle]
            [hive-mcp.events.registry :as registry]
            [hive-spi.swarm.guards :as guards]
            [clojure.set :as set]
            [hive-mcp.events.handlers.resilience :as resilience]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(def expected-events
  "Canonical advertised event-ids. The set MUST track the namespace docstring;
   any drift is surfaced by `verify-handlers!` at boot (ENGINE-L0.4)."
  #{:task/complete :task/shout-complete :git/commit-modified
    :ling/started :ling/completed :ling/ready-for-wrap
    :session/end :session/wrap
    :kanban/done :kanban/sync
    :crystal/wrap-request :crystal/wrap-notify
    :claim/file-released :claim/notify-waiting
    :system/error
    :hot/reload-start :hot/reload-success :file/changed
    :kg/edge-created :kg/edge-updated :kg/edge-removed :kg/node-promoted
    :agora/turn-dispatched :agora/timeout :agora/turn-completed
    :agora/dispatch-next :agora/consensus
    :saa/started :saa/phase-complete :saa/completed :saa/failed
    :memory/query :memory/search :memory/get
    :lifecycle/sweep
    :resilience/dim-mismatch})

(defn verify-handlers!
  "Compare the live event-handler registry against `expected-events` and
   emit a structured WARN for each missing handler. Returns
   {:registered #{…} :missing #{…} :extra #{…}}.

   Called from `register-handlers!` at boot so unregistered canonical events
   surface loudly instead of throwing only on first dispatch (ENGINE-L0.4)."
  []
  (let [registered (registry/registered-events)
        missing    (set/difference expected-events registered)
        extra      (set/difference registered expected-events)]
    (when (seq missing)
      (binding [*out* *err*]
        (println "[hive-events] WARNING: missing canonical handlers:" (sort missing))))
    (when (seq extra)
      (println "[hive-events] Extra handlers registered (not in expected-events):" (sort extra)))
    {:registered registered :missing missing :extra extra}))

(defn register-handlers!
  "Register all event handlers. Call at startup.

   Safe to call multiple times, and it REGISTERS every time.

   Delegates to domain-specific modules:
   - task/register-handlers!    - Task lifecycle
   - ling/register-handlers!    - Ling lifecycle
   - session/register-handlers! - Session lifecycle
   - kanban/register-handlers!  - Kanban state
   - crystal/register-handlers! - Wrap/crystallize
   - claim/register-handlers!   - File claims
   - system/register-handlers!  - System telemetry (Phase 1)
   - hot-reload/register-handlers! - Hot reload lifecycle
   - kg/register-handlers!       - Knowledge Graph edges
   - agora/register-handlers!    - Agora events
   - saa/register-handlers!      - SAA workflow lifecycle
   - lifecycle/register-handlers! - GC lifecycle sweep (gc-fix-4)

   It used to skip the whole body when `*registered` was already true. That
   atom is a `defonce`, which clj-reload preserves, so after a hot reload the
   flag still read true, this function did nothing, and the event registry kept
   dispatching to handler closures compiled before the reload. Kanban
   20260916134011-1246379c.

   Every handler below is registered BY KEY and last-writer-wins, so re-running
   is free. The flag now says only whether this is the first registration, which
   is what the log line and the return value were always about.

   Returns true.

   NOTE: this is the ROOT of a two-level fan-out. Several of the delegates carry
   their own `defonce` gate, and a gate at either level is enough to keep the
   old closures. Fixing this one alone changes nothing."
  []
  (let [first? (not @*registered)]
    ;; Register all domain handlers
    (task/register-handlers!)
    (ling/register-handlers!)
    (session/register-handlers!)
    (kanban/register-handlers!)
    (crystal/register-handlers!)
    (claim/register-handlers!)
    (system/register-handlers!)
    (hot-reload/register-handlers!)
    (kg/register-handlers!)
    (agora/register-handlers!)
    (saa/register-handlers!)
    (memory-read/register-handlers!)
    (lifecycle/register-handlers!)
    (resilience/register-handlers!)

    (verify-handlers!)
    (reset! *registered true)
    (when first?
      (println "[hive-events] Handlers registered: :task/complete :task/shout-complete :git/commit-modified :ling/started :ling/completed :ling/ready-for-wrap :session/end :session/wrap :kanban/sync :kanban/done :crystal/wrap-request :crystal/wrap-notify :claim/file-released :claim/notify-waiting :system/error :hot/reload-start :hot/reload-success :file/changed :kg/edge-created :kg/edge-updated :kg/edge-removed :kg/node-promoted :agora/turn-dispatched :agora/timeout :agora/turn-completed :agora/dispatch-next :agora/consensus :saa/started :saa/phase-complete :saa/completed :saa/failed :memory/query :memory/search :memory/get :lifecycle/sweep"))
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing.

   Guarded by `when-not-coordinator` — no-op when the live coordinator
   is running so test fixtures cannot flip `*registered` to false while
   handlers remain installed (which would corrupt event state)."
  []
  (guards/when-not-coordinator
   "events.handlers/reset-registration! blocked"
   (reset! *registered false)))