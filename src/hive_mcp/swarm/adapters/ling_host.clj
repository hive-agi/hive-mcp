;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.adapters.ling-host
  "hive-mcp adapter for the hive-spi.swarm.ports.ling-host SPI.

   One reify implements BOTH ILingReadiness and ILingCatchup, delegating to
   the host-owned hive-mcp namespaces:

   - ILingReadiness/wait-for-ling-ready -> hive-mcp.tools.consolidated.
     workflow.readiness/wait-for-ling-ready (keeps the DataScript poll, the
     per-spawn-mode checks and the [:services :forge :readiness-timeout-ms]
     read host-side). The original swarm call site (agent.ling.spawn/
     dispatch-after-ready!) reached this via a soft requiring-resolve, so
     the adapter resolves it lazily too — deferring readiness.clj's own
     require graph (emacs client, headless, config) until first dispatch
     and avoiding load cycles at addon init.
   - ILingCatchup/ling-catchup -> hive-mcp.workflows.catchup-ling/
     ling-catchup (keeps the extension-layer requiring-resolve and the
     token-budget truncation host-side). hive-mcp.workflows is a
     hive-workflows extraction target in the kernel census, so it is resolved
     by symbol on the call (hive-mcp.swarm.adapters.soft) and answers the
     port's Noop (nil) once the namespace has left.

   Install at addon init via install!. A standalone process without this
   adapter runs on the SPI's Noop (readiness: {:ready? false :phase
   :no-host ...}; catchup: nil)."
  (:require [hive-spi.swarm.ports.ling-host :as spi]
            [hive-mcp.swarm.adapters.soft :as soft]))

(def ^:private wait-for-ling-ready-sym
  'hive-mcp.tools.consolidated.workflow.readiness/wait-for-ling-ready)

(defn make-adapter
  "Build the hive-mcp implementation of both ling-host SPI protocols."
  []
  (reify spi/ILingReadiness
    (wait-for-ling-ready [_this agent-id spawn-mode]
      (try
        (if-let [f (requiring-resolve wait-for-ling-ready-sym)]
          (f agent-id spawn-mode)
          {:ready? false :phase :no-host :elapsed-ms 0 :attempts 0})
        (catch Throwable _
          {:ready? false :phase :host-error :elapsed-ms 0 :attempts 0})))
    spi/ILingCatchup
    (ling-catchup [_this opts]
      (try
        (soft/host-or 'hive-mcp.workflows.catchup-ling/ling-catchup
                      #(spi/ling-catchup spi/noop opts)
                      opts)
        (catch Throwable _ nil)))))

(defn install!
  "Install the hive-mcp adapter into the SPI slot. Idempotent by overwrite —
   the last installer wins, which is what a re-init should do."
  []
  (spi/set-ling-host! (make-adapter)))
