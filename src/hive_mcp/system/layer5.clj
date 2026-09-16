(ns hive-mcp.system.layer5
  "Integrant key implementations — Layer 5: hot-reload, schedulers, stdio.

   These keys manage background processes and the MCP stdio server:
     - :hive/hot-reload      — File watcher for hot-reload with claim-aware debounce
     - :hive/decay-scheduler — Periodic memory/edge/disc staleness decay
     - :hive/housekeeping    — Periodic GC sweep + stale resource cleanup
     - :hive/registry-sync   — Lings registry sync (elisp ↔ Clojure)
     - :hive/mcp-stdio       — MCP stdio server (Phase 7). nil in K8s profiles.

   Each init-key wraps existing functions from:
     - server/init.clj       → init-hot-reload-watcher!, start-decay-scheduler!,
                                stop-decay-scheduler!, start-housekeeping-scheduler!,
                                stop-housekeeping-scheduler!, start-registry-sync!
     - server/routes.clj     → build-server-spec
     - server/lifecycle.clj  → read-project-config
     - hive-hot.core         → stop-watcher!
     - swarm/registry        → stop-registry-sync!

   halt-key! stops each component and logs shutdown."
  (:require [integrant.core :as ig]
            [hive-mcp.server.init :as init]
            [hive-mcp.server.lifecycle :as lifecycle]
            [hive-mcp.dns.result :as result]
            [clojure.core.async :as async]
            [taoensso.timbre :as log]
            [hive-mcp.system.sweep-coordinator :as sweep-coordinator]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; :hive/hot-reload — File watcher for hot-reload
;; =============================================================================

(defmethod ig/init-key :hive/hot-reload
  [_ config]
  (log/info ":hive/hot-reload init — starting hot-reload watcher" config)
  ;; server-context-atom is needed for MCP auto-heal after reload.
  ;; We create a local atom here — :hive/mcp-stdio will populate it later.
  (let [server-context-atom (atom nil)
        project-config      (lifecycle/read-project-config)]
    (result/rescue nil
      (init/init-hot-reload-watcher! server-context-atom project-config))
    {:server-context-atom server-context-atom
     :status              :running}))

(defmethod ig/halt-key! :hive/hot-reload
  [_ state]
  (log/info ":hive/hot-reload halt — stopping file watcher")
  (result/rescue nil
    (when-let [stop! (requiring-resolve 'hive-hot.core/stop-watcher!)]
      (stop!)
      (log/info ":hive/hot-reload stopped"))))

;; =============================================================================
;; :hive/decay-scheduler — Periodic memory/edge/disc staleness decay
;; =============================================================================

(defmethod ig/init-key :hive/decay-scheduler
  [_ config]
  (log/info ":hive/decay-scheduler init — starting decay scheduler" config)
  (result/rescue nil
    (init/start-decay-scheduler!))
  ;; The recall canary's boot arm rides here because this key initializes after
  ;; the memory addon has registered its store — the canary needs a store to
  ;; read back from. Off-thread; a fault lands on the log at ERROR.
  (result/rescue nil
    (init/run-recall-canary-async!))
  {:status :running})

(defmethod ig/halt-key! :hive/decay-scheduler
  [_ state]
  (when (= :running (:status state))
    (log/info ":hive/decay-scheduler halt — stopping decay scheduler")
    (result/rescue nil
      (init/stop-decay-scheduler!)
      (log/info ":hive/decay-scheduler stopped"))))

;; =============================================================================
;; :hive/housekeeping — Periodic GC sweep + stale resource cleanup
;; =============================================================================

(defmethod ig/init-key :hive/housekeeping
  [_ config]
  (log/info ":hive/housekeeping init — starting housekeeping scheduler" config)
  (result/rescue nil
    (init/start-housekeeping-scheduler!))
  {:status :running})

(defmethod ig/halt-key! :hive/housekeeping
  [_ state]
  (when (= :running (:status state))
    (log/info ":hive/housekeeping halt — stopping housekeeping scheduler")
    (result/rescue nil
      (init/stop-housekeeping-scheduler!)
      (log/info ":hive/housekeeping stopped"))))

;; =============================================================================
;; :hive/sweep-coordinator — ISweepable registry heartbeat
;; =============================================================================

(defmethod ig/init-key :hive/sweep-coordinator
  [_ config]
  (log/info ":hive/sweep-coordinator init — starting sweep coordinator" config)
  (result/rescue nil
    ;; A sweeper registers from the `defonce` in its OWN namespace, so a sweeper
    ;; nobody requires never registers and is silently absent from the
    ;; heartbeat. Requiring them here is what puts them on the load path.
    ;; Deleting a line below deletes a sweep, with no other symptom.
    ;;
    ;; The two sweeps NOT listed here (headless/watchdog, lings/terminal-liveness)
    ;; register incidentally, because their host namespaces load for unrelated
    ;; reasons. That is not a design, it is how it happens to work today.
    (doseq [sweeper-ns '[hive-mcp.system.sweepers.orphan-channel
                         hive-mcp.system.sweepers.async-result]]
      (require sweeper-ns))
    ;; ONE OWNER PER SWEEP, enforced at the REGISTRATION SITE, not here.
    ;; :hive/housekeeping owns terminal liveness on its own 5 minute timer, so
    ;; hive-mcp.swarm.lifecycle.terminal-sweep deliberately does not register.
    ;; This used to unregister it at start instead, which silently did nothing:
    ;; the registration had not happened yet, because housekeeping's own
    ;; resolve-and-call is what loads that namespace, so the defonce fired
    ;; afterwards and re-armed the duplicate. Ownership cannot be enforced by
    ;; timing.
    (sweep-coordinator/start!))
  {:status :running})

(defmethod ig/halt-key! :hive/sweep-coordinator
  [_ state]
  (when (= :running (:status state))
    (log/info ":hive/sweep-coordinator halt — stopping sweep coordinator")
    (result/rescue nil
      (sweep-coordinator/stop!)
      (log/info ":hive/sweep-coordinator stopped"))))

;; =============================================================================
;; :hive/registry-sync — Lings registry sync (elisp ↔ Clojure)
;; =============================================================================

(defmethod ig/init-key :hive/registry-sync
  [_ config]
  (log/info ":hive/registry-sync init — starting lings registry sync" config)
  (result/rescue nil
    (init/start-registry-sync!))
  {:status :running})

(defmethod ig/halt-key! :hive/registry-sync
  [_ state]
  (when (= :running (:status state))
    (log/info ":hive/registry-sync halt — stopping registry sync")
    (result/rescue nil
      (when-let [stop! (requiring-resolve 'hive-mcp.tools.swarm.registry/stop-registry-sync!)]
        (stop!)
        (log/info ":hive/registry-sync stopped")))))

;; =============================================================================
;; :hive/mcp-stdio — MCP stdio server (Phase 7)
;;
;; Creates the MCP server spec, starts the stdio JSON-RPC server, and stores
;; the context in :hive/hot-reload's server-context-atom for auto-heal.
;;
;; In K8s profiles this key is nil (excluded via profile overlay).
;;
;; Integration with :hive/keepalive:
;;   init-key returns the join promise so that server/core.clj (or dev/user.clj)
;;   can pass it to keepalive/await-shutdown-or-stdio! for clean blocking.
;;   halt-key! is a no-op — keepalive handles shutdown signaling.
;; =============================================================================

(defmethod ig/init-key :hive/mcp-stdio
  [_ {:keys [hot-reload] :as _config}]
  (log/info ":hive/mcp-stdio init — building MCP server spec and starting stdio server")
  (let [server-id (random-uuid)
        result
        (try
          (let [;; Require at init time — these may not be loaded in K8s profiles
                routes-ns   (requiring-resolve 'hive-mcp.server.routes/build-server-spec)
                io-server   (requiring-resolve 'io.modelcontext.clojure-sdk.stdio-server/stdio-server)
                create-ctx! (requiring-resolve 'io.modelcontext.clojure-sdk.server/create-context!)
                start!      (requiring-resolve 'jsonrpc4clj.server/start)
                spec        (assoc (routes-ns) :server-id server-id)
                log-ch      (async/chan (async/sliding-buffer 20))
                server      (io-server {:log-ch log-ch})
                context     (assoc (create-ctx! spec) :server server)]
            ;; Wire server context into hot-reload for MCP auto-heal after reload
            (when-let [ctx-atom (:server-context-atom hot-reload)]
              (reset! ctx-atom context)
              (log/info ":hive/mcp-stdio — server context wired to :hive/hot-reload"))
            ;; Start JSON-RPC server — returns a join promise (derefable)
            (let [join (start! server context)]
              (log/info ":hive/mcp-stdio — stdio server started, server-id:" server-id)
              {:server server :context context :join join :log-ch log-ch}))
          (catch Throwable t
            (log/error t ":hive/mcp-stdio init threw — MCP tools will be unavailable in this session")
            nil))]
    (merge {:server-id server-id
            :status    (if result :running :failed)}
           result)))

(defmethod ig/halt-key! :hive/mcp-stdio
  [_ state]
  (when (= :running (:status state))
    (log/info ":hive/mcp-stdio halt — closing log channel")
    ;; Close the log channel. The actual stdio server shutdown is driven by
    ;; keepalive delivering :shutdown (which races the join promise).
    ;; We don't System/exit here — that's the caller's decision.
    (result/rescue nil
      (when-let [ch (:log-ch state)]
        (async/close! ch))
      (log/info ":hive/mcp-stdio halted"))))
