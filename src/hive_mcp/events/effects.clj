(ns hive-mcp.events.effects
  "Concrete effect implementations for the hive-mcp event system -- facade module.

   This namespace re-exports key public vars from the effects sub-modules
   for backward compatibility. New code should require specific sub-modules:

   - hive-mcp.events.effects.coeffect         -- coeffects (now, agent-context, db-snapshot, etc.)
   - hive-mcp.events.effects.notification     -- shout, log, channel, olympus
   - hive-mcp.events.effects.memory           -- memory-write, wrap-notify, wrap-crystallize
   - hive-mcp.events.effects.agent            -- dispatch-task, swarm-send-prompt, agora, saa
   - hive-mcp.events.effects.dispatch         -- event chaining (dispatch, dispatch-n)
   - hive-mcp.events.effects.infrastructure   -- ds-transact, git, kanban, metrics
   - hive-mcp.events.effects.kg               -- knowledge graph edges
   - hive-mcp.events.effects.lifecycle        -- GC lifecycle sweep (gc-fix-4)

   Usage:
   ```clojure
   (require '[hive-mcp.events.effects :as effects])
   (effects/register-effects!)
   ```"

  (:require [hive-mcp.events.effects.coeffect :as cofx-effects]
            [hive-mcp.events.effects.notification :as notif-effects]
            [hive-mcp.events.effects.dispatch :as dispatch-effects]
            [hive-mcp.events.effects.infrastructure :as infra-effects]
            [hive-mcp.events.effects.lifecycle :as lifecycle-effects]
            [hive-spi.swarm.guards :as guards]
            [taoensso.timbre :as log]
            [hive-mcp.events.contributions :as contrib]
            [hive-mcp.swarm.adapters.soft :as soft]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Re-exports: Memory handler injection (public API)
;; =============================================================================

(defn set-memory-write-handler!
  "Set the handler function for the :memory-write effect.

   The memory effect module is a hive-memory extraction target, so this
   injection point resolves it BY SYMBOL: with the module present the call
   lands exactly as before, and without it the injection is a no-op returning
   nil, which is what a build with no memory domain should do."
  [f]
  (soft/host-or 'hive-mcp.events.effects.memory/set-memory-write-handler!
                (constantly nil)
                f))

(defn set-wrap-crystallize-handler!
  "Set the handler function for the :wrap-crystallize effect.

   Same seam as `set-memory-write-handler!`: resolved by symbol, a no-op when
   the memory effect module is not in the build."
  [f]
  (soft/host-or 'hive-mcp.events.effects.memory/set-wrap-crystallize-handler!
                (constantly nil)
                f))

;; =============================================================================
;; Registration
;; =============================================================================

(defonce ^:private *registered (atom false))

(defn register-effects!
  "Register all concrete effect handlers and coeffects.

   Safe to call multiple times, and it REGISTERS every time.

   Two sources, and the kernel knows only the first:

   - KERNEL submodules, required statically and called here: coeffect
     (:now :agent-context :db-snapshot :waiting-lings :request-ctx),
     notification (:shout :targeted-shout :log :channel-publish
     :emit-system-error :olympus-broadcast), dispatch (:dispatch :dispatch-n),
     infrastructure (:ds-transact :git-commit :kanban-sync :kanban-move-done
     :report-metrics :tool-registry-refresh) and lifecycle (:lifecycle/sweep-fx).
   - CONTRIBUTED domains (memory, agent, kg) through
     hive-mcp.events.contributions: the in-core manifest declares them today,
     their addons contribute at `initialize!` tomorrow, and neither case edits
     this function.

   NOTE: the :crystal/wrap-notify EVENT handler is registered in
   hive-mcp.events.handlers.crystal/register-handlers! with proper defensive
   stats handling. Do NOT duplicate it here.

   It used to skip the whole body when `*registered` was already true. That atom
   is a `defonce`, which clj-reload preserves, so after a hot reload the flag
   still read true, this function did nothing, and every effect kept running the
   closure compiled before the reload. Kanban 20260916134011-1246379c.

   Every effect is registered BY KEY and last-writer-wins, so re-running is
   free. The flag now says only whether this is the first registration, which is
   what the log line and the return value were always about.

   Returns true.

   NOTE: this is the ROOT of a two-level fan-out, and some submodules carry their
   own `defonce` gate. A gate at either level keeps the old closures."
  []
  (let [first? (not @*registered)]
    (cofx-effects/register-coeffects!)

    (notif-effects/register-notification-effects!)
    (dispatch-effects/register-dispatch-effects!)
    (infra-effects/register-infrastructure-effects!)
    (lifecycle-effects/register-lifecycle-effects!)

    (contrib/load-manifest!)
    (let [{:keys [ran failed]} (contrib/register-all! :effects)]
      (when (seq failed)
        (log/error "[hive-events] effect contributions failed:" (sort (keys failed))))
      (when first?
        (log/info "[hive-events] effect contributions registered:" (sort ran))))

    (reset! *registered true)
    (if first?
      (log/info "[hive-events] kernel effect/coeffect submodules registered (coeffect, notification, dispatch, infrastructure, lifecycle)")
      (log/debug "[hive-events] Effect/coeffect submodules re-registered"))
    true))

(defn reset-registration!
  "Reset registration state. Primarily for testing.

   Guarded by `when-not-coordinator` — no-op when the live coordinator
   is running so test fixtures cannot reset effect registration while
   the live registry is in use."
  []
  (guards/when-not-coordinator
   "events.effects/reset-registration! blocked"
   (reset! *registered false)))
