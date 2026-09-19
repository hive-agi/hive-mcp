;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.adapters.events
  "hive-mcp adapter for the hive-spi.swarm.ports.events SPI.

   One reify implements ALL SEVEN protocols, delegating to the host-owned
   hive-mcp namespaces this port replaces:

   - IEventDispatch/dispatch!        -> hive-mcp.events.dispatch/dispatch
                                        (via the hive-mcp.events.core
                                        re-export; validates at the boundary
                                        and records Prometheus telemetry)
   - IEventDispatch/handler-registered?
                                     -> hive-mcp.events.registry/
                                        handler-registered? (via the
                                        hive-mcp.events.core re-export)
   - IAgentEventPublisher/*          -> hive-mcp.nats.bridge/publish-event! and
                                        /publish-shout! (soft dep for
                                        agent/ling/spawn.clj; resolved through
                                        requiring-resolve here so a host build
                                        without nats still loads this adapter)
   - IHookTrigger/trigger-hooks      -> hive-mcp.hooks.core/trigger-hooks with the
                                        hooks registry injected by
                                        hive-mcp.server.lifecycle
                                        (sync/set-hooks-registry!); no registry
                                        installed -> no-op, matching
                                        swarm/sync.clj's `(when-let [registry ...])`
   - ISwarmTelemetry/set-lings-active!
                                     -> hive-mcp.telemetry.prometheus/
                                        set-lings-active!
   - IDagWaveScheduler/*             -> hive-mcp.scheduler.dag-waves/start-dag!,
                                        /stop-dag!, /dag-status. Note the host
                                        start-dag! THROWS on an already-active
                                        plan; the adapter keeps that signal and
                                        returns {:started false ...} instead so
                                        the port's no-throw contract holds.
   - IAgentEventBroadcaster/emit-agent-event!
                                     -> hive-mcp.transport.olympus/
                                        emit-agent-event! (the facade re-exports
                                        transport.olympus.stream; a soft dep for
                                        swarm/sync.clj, resolved through
                                        requiring-resolve here too)
   - IVesselContextSource/resolve-agent-context
                                     -> hive-mcp.protocols.vessel/
                                        resolve-agent-context (queries the host
                                        vessel registry, first non-nil wins)

   Install at addon init via install!. A standalone process without this
   adapter runs on the SPI's Noop."
  (:require [hive-spi.swarm.ports.events :as spi]
            [clojure.tools.logging :as log]
            [hive-mcp.events.core :as events]
            [hive-mcp.hooks.core :as hooks]
            [hive-mcp.telemetry.prometheus :as prom]
            [hive-mcp.scheduler.dag-waves :as dag-waves]
            [hive-mcp.protocols.vessel :as vessel]
            [hive-mcp.swarm.sync :as sync]))

(defn make-adapter
  "Build the hive-mcp implementation of all events SPI protocols."
  []
  (reify

    spi/IEventDispatch
    (dispatch! [_this event-v]
      ;; events/dispatch throws on an invalid event or an unregistered
      ;; handler; the port is fire-and-forget, so guard with
      ;; handler-registered? exactly like the swarm-slice call sites do
      ;; (boot-race pattern) and never rethrow — a dispatch failure is
      ;; logged and dropped, matching the Noop's silent degradation.
      (try
        (when (events/handler-registered? (first event-v))
          (events/dispatch event-v))
        nil
        (catch Exception e
          (log/error e "events port: dispatch failed for" (first event-v))
          nil)))
    (handler-registered? [_this event-id]
      (events/handler-registered? event-id))

    spi/IAgentEventPublisher
    (publish-event! [_this payload]
      ;; nats.bridge is an optional dep for agent/ling/spawn.clj — keep it
      ;; soft here too: a host built without nats degrades to nil, the same
      ;; as today's requiring-resolve miss.
      (try
        (when-let [publish! (requiring-resolve 'hive-mcp.nats.bridge/publish-event!)]
          (publish! payload))
        nil
        (catch Exception _ nil)))
    (publish-shout! [_this payload]
      (try
        (when-let [publish! (requiring-resolve 'hive-mcp.nats.bridge/publish-shout!)]
          (publish! payload))
        nil
        (catch Exception _ nil)))

    spi/IHookTrigger
    (trigger-hooks [_this event context]
      ;; The hook registry is the one the host injected via
      ;; sync/set-hooks-registry! (the same one swarm/sync.clj reads). With
      ;; no registry installed, triggering is a no-op returning [] — the
      ;; exact behaviour of sync.clj's `(when-let [registry ...])` guard.
      (if-let [registry (sync/get-hooks-registry)]
        (hooks/trigger-hooks registry event context)
        []))

    spi/ISwarmTelemetry
    (set-lings-active! [_this n]
      (try
        (prom/set-lings-active! n)
        (catch Exception _ nil)))

    spi/IDagWaveScheduler
    (start-dag! [_this plan-id opts]
      ;; The host start-dag! throws when a plan is already active; the port
      ;; must not. Report the refusal in the return value so a caller cannot
      ;; distinguish a refusal from the Noop's :no-scheduler case by control
      ;; flow — only by the payload.
      (try
        (let [result (dag-waves/start-dag! plan-id opts)]
          (assoc result :active true))
        (catch Exception e
          {:started false :active false :plan-id plan-id :reason :already-active
           :error (ex-message e)})))
    (stop-dag! [_this]
      (try
        (dag-waves/stop-dag!)
        (catch Exception e {:stopped false :error (ex-message e)})))
    (dag-status [_this]
      (try
        (dag-waves/dag-status)
        (catch Exception _ {:active false})))

    spi/IAgentEventBroadcaster
    (emit-agent-event! [_this event-type agent-data]
      ;; transport.olympus is a soft dep for swarm/sync.clj — keep requiring-
      ;; resolve: no headed UI in the host build -> silent nil, the same as
      ;; sync.clj's `(when-let [emit-fn ...])` guard.
      (try
        (when-let [emit! (requiring-resolve 'hive-mcp.transport.olympus/emit-agent-event!)]
          (emit! event-type agent-data))
        nil
        (catch Exception _ nil)))

    spi/IVesselContextSource
    (resolve-agent-context [_this agent-id]
      (try
        (vessel/resolve-agent-context agent-id)
        (catch Exception _ nil)))))

(defn install!
  "Install the hive-mcp adapter into the SPI slot. Idempotent by overwrite —
   the last installer wins, which is what a re-init should do."
  []
  (spi/set-events! (make-adapter)))
