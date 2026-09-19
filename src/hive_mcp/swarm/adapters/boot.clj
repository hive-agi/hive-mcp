(ns hive-mcp.swarm.adapters.boot
  "Installs every hive-mcp adapter behind the swarm host ports.

   The swarm reaches its host only through the hive-spi.swarm.ports.* slots.
   With nothing installed those slots answer with their noop, so the swarm
   runs but its host effects (events, hivemind delivery, scope, readiness)
   go nowhere. This namespace fills every slot with the hive-mcp adapter.

   Two callers, one effect:
   - hive-agent's addon init resolves `install-all!` late through
     hive-agent.host.hive-mcp, so production installs at mount time.
   - the compat shims in hive-mcp call `ensure!` before delegating, so code
     that reaches the swarm without an addon init (tests, a REPL) still
     sees the real host.

   `install-all!` is idempotent by overwrite; `ensure!` runs it at most once."
  (:require [hive-mcp.swarm.adapters.agent-context :as agent-context]
            [hive-mcp.swarm.adapters.events :as events]
            [hive-mcp.swarm.adapters.inbox.piggyback :as piggyback-sink]
            [hive-mcp.swarm.adapters.ling-host :as ling-host]
            [hive-mcp.swarm.adapters.memory-scope :as memory-scope]
            [hive-mcp.swarm.adapters.messaging :as messaging]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn install-all!
  "Install every swarm host adapter and register the piggyback inbox sink.
   Safe to call repeatedly: each install replaces the slot's current value."
  []
  (agent-context/install!)
  (events/install!)
  (ling-host/install!)
  (memory-scope/install!)
  (messaging/install!)
  (piggyback-sink/install!)
  (log/debug "swarm host adapters installed")
  :installed)

(defonce ^:private installed
  (delay (install-all!)))

(defn ensure!
  "Install the adapters once per JVM. Cheap after the first call."
  []
  @installed)
