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

  (:require [hive-mcp.events.handlers.claim :as claim]
            [hive-mcp.events.handlers.system :as system]
            [hive-mcp.events.handlers.hot-reload :as hot-reload]
            [hive-mcp.events.handlers.lifecycle :as lifecycle]
            [hive-mcp.events.registry :as registry]
            [hive-spi.swarm.guards :as guards]
            [clojure.set :as set]
            [hive-mcp.events.handlers.resilience :as resilience]
            [hive-mcp.events.contributions :as contrib]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(def kernel-events
  "Event ids the KERNEL's own handler modules register: claim, system,
   hot-reload, lifecycle and resilience. This set is a promise the kernel can
   keep with no addon mounted and no domain module in the build.

   Domain events are not here. Each domain DECLARES what it registers when it
   contributes (hive-mcp.events.contributions), so the full expectation is
   `(expected-events)` and it shrinks honestly as domains leave."
  #{:claim/file-released :claim/notify-waiting
    :system/error
    :hot/reload-start :hot/reload-success :file/changed
    :lifecycle/sweep
    :resilience/dim-mismatch})

(defn expected-events
  "Every event id this build may honestly expect in the registry: the kernel's
   own `kernel-events` plus whatever the contributed handler domains declared.

   It is a function, not the constant it used to be: with domains contributing
   at boot (in-core manifest) and at addon `initialize!`, the answer depends on
   what is mounted. A build with no domains expects only `kernel-events`."
  []
  (into kernel-events (contrib/declared-events)))

(defn verify-handlers!
  "Compare the live event-handler registry against `(expected-events)` and
   emit a structured WARN for each missing handler. Returns
   {:registered #{…} :missing #{…} :extra #{…}}.

   Called from `register-handlers!` at boot so unregistered events surface
   loudly instead of throwing only on first dispatch (ENGINE-L0.4). What is
   EXPECTED now depends on what contributed: a domain that left with its addon
   is not missing, it is absent, and it stops being demanded."
  []
  (let [expected   (expected-events)
        registered (registry/registered-events)
        missing    (set/difference expected registered)
        extra      (set/difference registered expected)]
    (when (seq missing)
      (binding [*out* *err*]
        (println "[hive-events] WARNING: missing handlers:" (sort missing))))
    (when (seq extra)
      (println "[hive-events] Extra handlers registered (not declared):" (sort extra)))
    {:registered registered :missing missing :extra extra}))

(defn register-handlers!
  "Register all event handlers. Call at startup.

   Safe to call multiple times, and it REGISTERS every time.

   Two sources, and the kernel knows only the first:

   - the KERNEL's own modules (claim, system, hot-reload, lifecycle,
     resilience), required statically and called here;
   - CONTRIBUTED domains (task, ling, session, kanban, crystal, kg, agora,
     saa, memory-read), which arrive through hive-mcp.events.contributions:
     `load-manifest!` for the ones still shipped inside core, and
     `contribute!` at `initialize!` for the ones that have become addons.
     Adding or removing a domain is a manifest or addon change, never an edit
     to this function.

   It used to skip the whole body when `*registered` was already true. That
   atom is a `defonce`, which clj-reload preserves, so after a hot reload the
   flag still read true, this function did nothing, and the event registry kept
   dispatching to handler closures compiled before the reload. Kanban
   20260916134011-1246379c.

   Every handler is registered BY KEY and last-writer-wins, so re-running is
   free. The flag now says only whether this is the first registration, which
   is what the log line and the return value were always about.

   Returns true.

   NOTE: this is the ROOT of a two-level fan-out. Several of the delegates carry
   their own `defonce` gate, and a gate at either level is enough to keep the
   old closures. Fixing this one alone changes nothing."
  []
  (let [first? (not @*registered)]
    ;; Kernel handler modules
    (claim/register-handlers!)
    (system/register-handlers!)
    (hot-reload/register-handlers!)
    (lifecycle/register-handlers!)
    (resilience/register-handlers!)

    ;; Domain modules: in-core ones declare themselves in the manifest, addons
    ;; contribute at initialize!. Re-running load-manifest! is free and picks
    ;; up a domain that arrived since the last call.
    (contrib/load-manifest!)
    (let [{:keys [failed]} (contrib/register-all! :handlers)]
      (when (seq failed)
        (binding [*out* *err*]
          (println "[hive-events] handler contributions failed:" (sort (keys failed))))))

    (verify-handlers!)
    (reset! *registered true)
    (when first?
      (println "[hive-events] Handlers registered:" (sort (expected-events))))
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